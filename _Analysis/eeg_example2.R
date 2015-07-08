#example to code:
# add highpass to lowpass to and compare to original to show wonky residual

library(plyr) #for ddply/dlply
library(data.table) #for crbindlist
library(ggplot2) #for visuals
library(mgcv) #for gam
# library(pracma) #for detrend
# library(signal) #for butter & filter
source('ITD.R')
start = proc.time()[3]
#read in the eeg data
eeg = readBin(
	con = '../_EEG/multimodal_ior_p05.eeg'
	, what = 'double'
	, size = 4 #32-bit (2^4)
	, n = 1e9 #big number specifying max to read
)
print(proc.time()[3]-start)
#convert to matrix (makes some computations faster)
eeg2 = matrix(eeg,ncol=64,byrow=T)
print(proc.time()[3]-start)

#pull out aux
aux = data.frame(
	t = 1:nrow(eeg) #time
	, v = eeg[,64] #voltage
	, d = c(0,diff(eeg[,64])) #delta-v (change in voltage)
)

#remove aux from eeg
eeg = eeg[,1:63]

#re-reference to average mastoid (currently referenced to left)
eeg = eeg[,1:63]-eeg[,20]/2



# #slow way to get trigger times; commented out as there is a faster method below
# aux$trigger = FALSE
# aux$trigger_duration = NA
# aux$trigger_num = NA
# start = 1
# trigger_count = 0
# while(any(aux$d[start:nrow(aux)]>1e3)){
# 	next_up = start+which.max(aux$d[start:nrow(aux)]>1e3)
# 	aux$trigger[next_up] = TRUE
# 	aux$trigger_num[next_up] = trigger_count + 1
# 	trigger_count = trigger_count + 1
# 	next_down = next_up+which.max(aux$d[(next_up+1):nrow(aux)]<(-1e3))
# 	aux$trigger_duration[next_up] = next_down - next_up
# 	start = next_down
# 	# print(sum(aux$trigger))
# }

#fast way to compute trigger times
up = aux[aux$d>1e5,] #using 1e3 as a criterion delta-voltage
up$dt = c(Inf,diff(up$t))
up = up[up$dt>2000,] #assume triggers are sent at least 2s apart

#compute down and then RT for checking against behavioural dataj
down = aux[aux$d<(-1e5),]
down$dt = c(Inf,diff(down$t))
down = down[down$dt>2000,]
d = data.frame(cbind(
	up_t = up$t
	,down_t = down$t
	,rt = down$t-up$t #this might generate a warning
))
# d = d[d$rt<1000,]
# d = d[d$rt>0,]

#read in the behavioural data
b = read.table(
	file = '../_Data/p05_2015_07_04_14_34/p05_2015_07_04_14_34_data.txt'
	, header = T
)

#toss trials that don't have a target (and therefore no trigger)
b = b[b$trigger_shown,] #when blinks/saccades happen in practice, trial is terminated


#copy eeg start to behavioural data
b$eeg_start = d$up_t

#toss trials we don't want for eeg analysis
b = b[b$block!='practice',]
b = b[!b$saccade,]
b = b[!b$blink,]
b = b[!b$pre_target_response,]
b = b[!b$feedback_response,]

#refactorize target location (because it no longer has the "catch" level)
b$target_location = factor(b$target_location)

#compute cued factor
b$cued = factor(
	ifelse(
		b$target_location == 'catch'
		, 'catch'
		, ifelse(
			as.character(b$cue_location)==as.character(b$target_location)
			, 'valid'
			, 'invalid'
		)
	)
)


#collect epoched eeg data
eeg_data = dlply(
	.data = b
	, .variables = .(trial_num,block)
	, .fun = function(x){
		trial_eeg = eeg[(x$eeg_start-2000):(x$eeg_start+1000),] #cue onset to target timeout
# 		#ITD-based bandpass
# 		for(channel in 1:62){
# 			from_itd = get_all_itds(trial_eeg[,channel])
# 			keep = NULL
# 			for(i in 2:(length(from_itd)-1)){
# 				# print(paste(i,round(from_itd[[i]]$hz[1])))
# 				if((from_itd[[i]]$hz[1]<100)&(from_itd[[i]]$hz[1]>1)){
# 					keep = c(keep,i)
# 				}
# 			}
# 			from_itd_df = rbindlist(from_itd[keep])
# 			y_passed = from_itd_df[ , list(y = sum(y)), by="x"]
# 			trial_eeg[,channel] = y_passed$y
# 		}
		# trial_eeg = t(t(trial_eeg)-colMeans(eeg[(x$eeg_start-2000):(x$eeg_start-1000),])) #baseline correction from 1s pre-cue
		#reformat to long
		trial_eeg_long = data.frame(
			channel = rep(1:ncol(trial_eeg),each=nrow(trial_eeg))
			, value = as.vector(trial_eeg)
			, time = -2000:1000
		)
		row.names(x) = NULL
		out = cbind(x,trial_eeg_long)
		return(out)
	}
	, .progress = 'time'
)
#turn list from above to a data frame (faster than ddply)
eeg_data = data.frame(.Call("Crbindlist", eeg_data, FALSE, FALSE))

print(proc.time()[3]-start)

#show all channels for visual-visual
ggplot(
	data = eeg_data[(eeg_data$channel!=63)&(eeg_data$cue_modality=='visual')&(eeg_data$target_modality=='visual'),]
	, mapping = aes(
		x = time
		, y = value
		, colour = cued
		, fill = cued
	)
)+
facet_wrap(
	~ channel
	# , scale = 'free_y'
)+
geom_smooth(
	method = 'gam'
	, formula = y ~ s(x , bs='ts' , k=20) #k affects wigglyness
)+
geom_vline(
	xintercept = 0
	, linetype = 2
)

#show all conditions for one channel
ggplot(
	data = eeg_data[(eeg_data$channel==11),] #try channel 10, for example
	, mapping = aes(
		x = time
		, y = value
		, colour = cued
		, fill = cued
	)
)+
facet_grid(
	cue_modality ~ target_modality
	# , scale = 'free_y'
)+
geom_smooth(
	method = 'gam'
	, formula = y ~ s(x,bs='ts',k=100) #k affects wigglyness
)+
geom_vline(
	xintercept = 0
	, linetype = 2
)
