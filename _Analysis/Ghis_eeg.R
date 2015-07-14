library(plyr) #for ddply/dlply
library(data.table) #for crbindlist
library(ggplot2) #for visuals
library(mgcv) #for gam


start = proc.time()[3]
#read in the eeg data
eeg = readBin(
	con = '~/Experiments/Multimodal_ior/_EEG/multimodal_ior_p02.eeg'
	, what = 'double'
	, size = 4 #32-bit (2^4)
	, n = 1e9 #big number specifying max to read
)
print(proc.time()[3]-start)
#convert to matrix (makes some computations faster)
eeg = matrix(eeg,ncol=63,byrow=T)
print(proc.time()[3]-start)

#pull out aux
aux = data.frame(
	t = 1:nrow(eeg) #time
	, v = eeg[,63] #voltage
	, d = c(0,diff(eeg[,63])) #delta-v (change in voltage)
)

#remove aux from eeg
eeg = eeg[,1:62]

#slow way to get trigger times; commented out as there is a faster method below
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
up = aux[aux$d>1e3,] #using 1e3 as a criterion delta-voltage
up$dt = c(Inf,diff(up$t))
up = up[up$dt>2000,] #assume triggers are sent at least 2s apart

#compute down and then RT for checking against behavioural dataj
down = aux[aux$d<(-1e3),]
down$dt = c(Inf,diff(down$t))
down = down[down$dt>2000,]
d = data.frame(cbind(
	up_t = up$t
	,down_t = down$t
	,rt = down$t-up$t #this might generate a warning
))
d = d[d$rt<1000,]
d = d[d$rt>0,]

#read in the behavioural data
b = read.table(
	file = '~/Experiments/Multimodal_ior/_Data/p02_2015_06_24_14_55/p02_2015_06_24_14_55_data.txt'
	, header = T
)

#toss trials that don't have a target (and therefore no trigger)
b = b[((!b$saccade)&(!b$blink))|(b$block!='practice'),] #when blinks/saccades happen in practice, trial is terminated
# b = b[(!is.na(b$target_response_rt))|(b$target_location=='catch'),]
b = b[b$target_location!='catch',]
b = b[!is.na(b$target_response_rt),]

#filter trials where target appears but is cancelled thanks to blink/saccade during practice
# (this could be obviated if such trials were tagged during data collection)
for(i in 1:nrow(b)){
	while(abs(d$rt[i]-b$target_response_rt[i])>5){
		d = d[-i,]
		print(i)
	}
}

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
b$cued = factor(b$cue_location==b$target_location)


#this is where one might apply a bandpass filter and/or do ICA to the eeg data for noise reduction
# but won't bother for the moment.

#collect epoched eeg data
eeg_data = dlply(
	.data = b
	, .variables = .(trial_num,block)
	, .fun = function(x){
		trial_eeg = eeg[(x$eeg_start-1000):(x$eeg_start+1000),] #cue onset to target timeout
		trial_eeg = t(t(trial_eeg)-colMeans(eeg[(x$eeg_start-2000):(x$eeg_start-1000),])) #baseline correction from 1s pre-cue
		#reformat to long
		trial_eeg_long = data.frame(
			channel = rep(1:ncol(trial_eeg),each=nrow(trial_eeg))
			, value = as.vector(trial_eeg)
			, time = -1000:1000
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
	, formula = y ~ s(x,bs='ts',k=20) #k affects wigglyness
)+
geom_vline(
	xintercept = 0
	, linetype = 2
)

#show all conditions for one channel
ggplot(
	data = eeg_data[(eeg_data$channel==10),] #try channel 10, for example
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
	, formula = y ~ s(x,bs='ts',k=20) #k affects wigglyness
)+
geom_vline(
	xintercept = 0
	, linetype = 2
)
