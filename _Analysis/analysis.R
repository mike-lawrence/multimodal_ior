#load useful packages
library(stringr)
library(plyr)
library(ggplot2)
library(ez)
library(lme4)

#set more useful default contrasts for factors
options(contrasts=c('contr.helmert','contr.poly'))

#define a robust measure of deviation
madmed = function(x){
	median(abs(x-median(x)))
}

#read in a single subject of data
a = read.table(
	file = '../_Data/p06_2015_07_14_16_13/p06_2015_07_14_16_13_data.txt'
	, header = T
	, sep = '\t'
)

#read in all Ss' data
a = ldply(
	.data = list.files(
		path = '../_Data'
		, pattern = '_data.txt'
		, full.names = T
		, recursive = T
	)
	, .fun = function(x){
		read.table(
			file = x
			, header = T
			, sep = '\t'
		)
	}
)

#toss practice
a = a[a$block!='practice',]

#turn block into a number (in case we want to use it as a predictor)
a$block = as.numeric(as.character(a$block))

#look at the rates of various error behaviours
mean(a$pre_target_response)
mean(a$feedback_response)
mean(a$blink)
mean(a$critical_blink)
mean(a$saccade)
mean(a$critical_saccade)

#toss trials with error behaviours
a = a[
	!a$pre_target_response
	& !a$feedback_response
	& !a$critical_blink
	& !a$critical_saccade
	,
]

#compute FA rate
mean(!is.na(a$target_response_rt[a$target_type=='catch']))
#compute Miss rate
mean(is.na(a$target_response_rt[a$target_type=='target']))

#toss catch trials and misses
a = a[
	a$target_type=='target'
	& !is.na(a$target_response_rt)
,]

#create shorthand for rt column
a$rt = a$target_response_rt

#create log-rt column
a$lrt = log(a$rt)

#show rt histograms
ggplot(
	data = a
	, mapping = aes(
		x = rt
	)
)+
facet_wrap(
	~ id
)+
geom_histogram()

#show log-rt histograms
ggplot(
	data = a
	, mapping = aes(
		x = lrt
	)
)+
facet_wrap(
	~ id
)+
geom_histogram()

#label outlier RTs per subject/condition
a = ddply(
	.data = a
	, .variables = .(id,cue_location,cue_modality,target_location,target_modality)
	, .fun = function(x){
		med_lrt = median(x$lrt)
		madmed_lrt = madmed(x$lrt)
		lo_crit = med_lrt-(5*madmed_lrt)
		hi_crit = med_lrt+(5*madmed_lrt)
		x$hi = x$lrt>hi_crit
		x$lo = x$lrt<lo_crit
		x$toss = x$hi | x$lo
		return(x)
	}
)

#compute outlier rate
mean(a$toss)

#show RT histograms with outliers labelled
ggplot(
	data = a
	, mapping = aes(
		x = rt
		, fill = toss
	)
)+
facet_wrap(
	~ id
)+
geom_histogram(
	position = 'identity'
	, colour = 'transparent'
	, alpha = .5
)

#show log-RT histograms with outliers labelled
ggplot(
	data = a
	, mapping = aes(
		x = lrt
		, fill = toss
	)
)+
facet_wrap(
	~ id
)+
geom_histogram(
	position = 'identity'
	, colour = 'transparent'
	, alpha = .5
)

#toss outliers
a = a[!a$toss,]

########
# Run an analysis appropriate for application to a single subject
########

#fit a model for a single subject (not yet collapsed to cue-validity)
fit = lm(
	data = a
	, formula = rt ~ cue_location*cue_modality*target_location*target_modality
)
confint(fit)

#compute cell means
rt_cells = ddply(
	.data = a
	, .variables = .(cue_location,cue_modality,target_location,target_modality)
	, .fun = function(x){
		out = data.frame(
			value = mean(x$rt)
		)
		return(out)
	}
)
#compute bootstrap samples
rt_boots = ddply(
	.data = a
	, .variables = .(cue_location,cue_modality,target_location,target_modality)
	, .fun = function(x){
			s = matrix(
				sample(x$rt,replace=T,size=nrow(x)*1e3)
				, ncol = nrow(x)
				, nrow = 1e3
			)
			out = data.frame(
				iteration = 1:1e3
				, value = rowMeans(s)
			)
		return(out)
	}
)

#put them together for visualization
rt_preds = list(
	cells = rt_cells
	, boots = rt_boots
)

#visualize cell means & bootstrap 95%CIs
ezPlot2(
	preds = rt_preds
	, x = target_location
	, split = cue_location
	, col = target_modality
	, row = cue_modality
)

#collapse to cuing effect
rt_cuing = llply(
	.data = rt_preds
	, .fun = function(x){
		if("iteration"%in%names(x)){
			vars = .(iteration,target_modality,cue_modality)
		}else{
			vars = .(target_modality,cue_modality)
		}
		out = ddply(
			.data = x
			, .variables = vars
			, .fun = function(z){
				out2 = data.frame(
					cuing = factor(c('invalid','valid'))
					, value = c(mean(z$value[z$cue_location!=z$target_location]) , mean(z$value[z$cue_location==z$target_location]))
				)
				return(out2)
			}
		)
		return(out)
	}
)

#visualize cuing invalid vs valid
ezPlot2(
	preds = rt_cuing
	, x = cuing
	, col = target_modality
	, row = cue_modality
)

#visualize cuing difference score
ezPlot2(
	preds = rt_cuing
	, diff = cuing
	, col = target_modality
	, x = cue_modality
)


########
# Analysis that looks at all Ss' together
########

#fit a model
rt_fit = lmer(
	data = a
	, formula = rt ~ cue_location*cue_modality*target_location*target_modality +
			(1+cue_location*cue_modality*target_location*target_modality|id)
)
#compute confidence intervals on effects
confint(rt_fit,method='Wald')

#compute predictions from the model
rt_preds = ezPredict(
	fit = rt_fit
	, zero_intercept_variance = TRUE
)

#compute cuing valid/invalid
rt_cuing = llply(
	.data = rt_preds
	, .fun = function(x){
		if("iteration"%in%names(x)){
			vars = .(iteration,target_modality,cue_modality)
		}else{
			vars = .(target_modality,cue_modality)
		}
		out = ddply(
			.data = x
			, .variables = vars
			, .fun = function(z){
				out2 = data.frame(
					cuing = factor(c('invalid','valid'))
					, value = c(mean(z$value[z$cue_location!=z$target_location]) , mean(z$value[z$cue_location==z$target_location]))
				)
				return(out2)
			}
		)
		return(out)
	}
)

#visualize cuing invalid vs valid
ezPlot2(
	preds = rt_cuing
	, x = cuing
	, col = target_modality
	, row = cue_modality
)

#visualize cuing difference score
ezPlot2(
	preds = rt_cuing
	, diff = cuing
	, col = target_modality
	, x = cue_modality
)
