#load useful packages
library(plyr) #for ddply/dlply
library(data.table) #for crbindlist
library(ggplot2) #for visuals
library(mgcv) #for gam

#load the ITD algorithm
source('ITD.R')

#read in the eeg data
start = proc.time()[3]
eeg = readBin(
	con = '../_EEG/multimodal_ior_p06.eeg'
	#con = 'temp2.eeg'
	, what = 'double'
	, size = 4 #32-bit (2^4)
	, n = 1e9#33*100000*60#1e11 #big number specifying max to read
)
print(proc.time()[3]-start)

#convert to matrix (makes some computations faster)
eeg = matrix(eeg,ncol=65,byrow=T)
print(proc.time()[3]-start)

#double-check that we've read in the data ok
plot(eeg[1:1e4,1],type='l')
plot(eeg[1:1e4,2],type='l')
plot(eeg[1:1e6,65],type='l') #should show impulses from square-waves when sensor is triggered
#if not, the data should be really spikey with a big y-axis range


#pull out aux
aux = data.frame(
	t = 1:nrow(eeg) #time
	, v = eeg[,65] #voltage
	, d = c(0,diff(eeg[,65])) #delta-v (change in voltage)
)

#remove aux from eeg
eeg = eeg[,1:64]

#re-reference to average mastoid (currently unreferenced)
eeg = eeg-(eeg[,10]+eeg[,21])/2
plot(eeg[1:1e4,1],type='l')


#compute trigger times
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
	,rt = down$t-up$t
))

#read in the behavioural data
b = read.table(
	file = '../_Data/p06_2015_07_14_16_13/p06_2015_07_14_16_13_data.txt'
	, header = T
)

#toss trials that don't have a target (and therefore no trigger)
b = b[b$target_started_TF,] #when blinks/saccades happen in practice, trial is terminated

#double check the data are lined up by looking at RTs
any( (d$rt[!is.na(b$target_response_rt)]-b$target_response_rt[!is.na(b$target_response_rt)]) > 5 )


#copy eeg start to behavioural data
b$eeg_start = d$up_t

#toss trials we don't want for eeg analysis
b = b[b$block!='practice',]
b = b[
	!b$pre_target_response
	& !b$feedback_response
	& !b$critical_blink
	& !b$critical_saccade
	,
]

#toss catch trials & misses
b = b[
	b$target_type=='target'
	& !is.na(b$target_response_rt)
,]


#compute cued factor
b$cued = factor(
	ifelse(
		as.character(b$cue_location)==as.character(b$target_location)
		, 'valid'
		, 'invalid'
	)
)


#collect epoched eeg data
eeg_data = dlply(
	.data = b
	, .variables = .(trial_num,block)
	, .fun = function(x){
		trial_eeg = eeg[(x$eeg_start-2000):(x$eeg_start+1000),] #cue onset to target timeout
		#ITD-based bandpass
		for(channel in 1:64){
			from_itd = get_all_itds(trial_eeg[,channel],freq=1e3,show_progress=F)
			keep = NULL
			for(i in 2:(length(from_itd)-2)){
				# print(paste(i,round(from_itd[[i]]$hz[1])))
				if((mean(from_itd[[i]]$hz,na.rm=T)<100)&(mean(from_itd[[i]]$hz,na.rm=T)>1)){
					keep = c(keep,i)
				}
			}
			from_itd_df = rbindlist(from_itd[keep])
			y_passed = from_itd_df[ , list(y = sum(y)), by="x"]
			trial_eeg[,channel] = y_passed$y
		}
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
	data = eeg_data[(eeg_data$cue_modality=='visual')&(eeg_data$target_modality=='visual'),]
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
	data = eeg_data[(eeg_data$channel==16),]
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

#actually fit a model
eeg_data$cuedBYcue_modality = factor(with(eeg_data,paste(cued,cue_modality)))
eeg_data$cuedBYtarget_modality = factor(with(eeg_data,paste(cued,target_modality)))
eeg_data$cue_modalityBYtarget_modality = factor(with(eeg_data,paste(cue_modality,target_modality)))
eeg_data$cuedBYcue_modalityBYtarget_modality = factor(with(eeg_data,paste(cued,cue_modality,target_modality)))
fit = bam(
	data = eeg_data[(eeg_data$channel==16),]
	, formula = value ~ cued * cue_modality * target_modality +
		s(time,bs='ts',k=20) +
		s(time,bs='ts',k=20,by=cued) +
		s(time,bs='ts',k=20,by=cue_modality) +
		s(time,bs='ts',k=20,by=target_modality) +
		s(time,bs='ts',k=20,by=cuedBYcue_modality) +
		s(time,bs='ts',k=20,by=cuedBYtarget_modality) +
		s(time,bs='ts',k=20,by=cue_modalityBYtarget_modality) +
		s(time,bs='ts',k=20,by=cuedBYcue_modalityBYtarget_modality)
)

#alternatively, just bootstrap
cells = dlply(
	.data = eeg_data[(eeg_data$channel==16),]
	, .variables = .(cued,cue_modality,target_modality)
	, .fun = function(x){
		t = matrix(
			x$value
			, nrow = length(-2000:1000)
		)
		out = data.frame(
			time = -2000:1000
			, value = rowMeans(t)
			, cued = x$cued[1]
			, cue_modality = x$cue_modality[1]
			, target_modality = x$target_modality[1]
		)
		return(out)
	}
	, .progress = 'time'
)
cells = data.frame(.Call("Crbindlist", cells, FALSE, FALSE))


boots = dlply(
	.data = eeg_data[(eeg_data$channel==16),]
	, .variables = .(cued,cue_modality,target_modality)
	, .fun = function(x){
		t = matrix(
			x$value
			, nrow = length(-2000:1000)
		)
		out = llply(
				.data = 1:1e3
				, .fun = function(iteration){
					selection = sample(1:ncol(t),replace=T)
					out2 = data.frame(
						iteration = iteration
						, time = -2000:1000
						, value = rowMeans(t[,selection])
					)
					return(out2)
				}
		)
		out = data.frame(.Call("Crbindlist", out, FALSE, FALSE))
		out$cued = x$cued[1]
		out$cue_modality = x$cue_modality[1]
		out$target_modality = x$target_modality[1]
		return(out)
	}
	, .progress = 'time'
)
boots = data.frame(.Call("Crbindlist", boots, FALSE, FALSE))


preds = list(
	cells = cells[cells$time%%10==0,]
	, boots = boots[boots$time%%10==0,]
)

ezPlot2(
	preds = preds
	, x = time
	, split = cued
	, row = target_modality
	, col = cue_modality
	, ribbon = T
)



