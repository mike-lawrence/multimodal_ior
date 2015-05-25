library(stringr)
library(plyr)
library(ggplot2)
library(ez)
library(lme4)
options(contrasts=c('contr.helmert','contr.poly'))

madmed = function(x){
	median(abs(x-median(x)))
}

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
a = a[a$id!='708546',] #maryam, wasn't 
a = a[a$id!='637200',] #too few trials (left sick)
a = a[a$id!='584366',] #too few trials ()

a$ttoa = factor(a$ttoa)
a$ctoa = factor(a$ctoa)

a = a[!str_detect(a$block,'practice'),]
a = a[!is.na(a$t2_rt),]

temp = a[!is.na(a$t1_identity),]
mean(is.na(temp$t1_rt))

toss = is.na(a$t1_rt) & (!is.na(a$t1_identity))
mean(toss,na.rm=T)
a = a[!toss,]

mean(a$t2_error)
mean(a$t1_error,na.rm=T)

a$t2_lrt = log(a$t2_rt)
a$t1_lrt = log(a$t1_rt)
a$t1_error_01 = as.numeric(a$t1_error)
a$t2_error_01 = as.numeric(a$t2_error)


########
# Analyze combined data
########

b = a[(!is.na(a$cue))&(!is.na(a$t1_identity)),]

hist(b$t1_rt,br=100)
hist(b$t2_rt,br=100)

ggplot(
	data = b
	, mapping = aes(
		x = t2_rt
		, fill = t2_error
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
ggplot(
	data = b
	, mapping = aes(
		x = t2_lrt
		, fill = t2_error
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
ggplot(
	data = b
	, mapping = aes(
		x = t1_rt
		, fill = t1_error
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
ggplot(
	data = b
	, mapping = aes(
		x = t1_lrt
		, fill = t1_error
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


b = ddply(
	.data = b
	, .variables = .(id)
	, .fun = function(x){
		t2_med_lrt = median(x$t2_lrt[!x$t2_error])
		t2_madmed_lrt = madmed(x$t2_lrt[!x$t2_error])
		t2_lo_crit = t2_med_lrt-(5*t2_madmed_lrt)
		t2_hi_crit = t2_med_lrt+(5*t2_madmed_lrt)
		x$t2_hi = x$t2_lrt>t2_hi_crit
		x$t2_lo = x$t2_lrt<t2_lo_crit
		x$t2_toss = x$t2_hi | x$t2_lo
		t1_med_lrt = median(x$t1_lrt[!x$t1_error])
		t1_madmed_lrt = madmed(x$t1_lrt[!x$t1_error])
		t1_lo_crit = t1_med_lrt-(5*t1_madmed_lrt)
		t1_hi_crit = t1_med_lrt+(5*t1_madmed_lrt)
		x$t1_hi = x$t1_lrt>t1_hi_crit
		x$t1_lo = x$t1_lrt<t1_lo_crit
		x$t1_toss = x$t1_hi | x$t1_lo
		return(x)
	}
)
mean(b$t1_toss)
mean(b$t2_toss)
mean(b$t1_toss|b$t2_toss)


ggplot(
	data = b[!b$t2_error,]
	, mapping = aes(
		x = t2_rt
		, fill = t2_toss
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
ggplot(
	data = b[!b$t2_error,]
	, mapping = aes(
		x = t2_lrt
		, fill = t2_toss
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
ggplot(
	data = b[!b$t1_error,]
	, mapping = aes(
		x = t1_rt
		, fill = t1_toss
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
ggplot(
	data = b[!b$t1_error,]
	, mapping = aes(
		x = t1_lrt
		, fill = t1_toss
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
hist(b$t2_rt,br=100)
hist(b$t2_rt[(!b$t2_toss)&(!b$t1_toss)],br=100)
hist(b$t2_rt,br=100,xlim=c(0,500))
hist(b$t2_rt[(!b$t2_toss)&(!b$t1_toss)],br=100,xlim=c(0,500))
hist(b$t2_lrt,br=100)
hist(b$t2_lrt[(!b$t2_toss)&(!b$t1_toss)],br=100)

hist(b$t1_rt,br=100)
hist(b$t1_rt[(!b$t2_toss)&(!b$t1_toss)],br=100)
hist(b$t1_rt,br=100,xlim=c(0,500))
hist(b$t1_rt[(!b$t2_toss)&(!b$t1_toss)],br=100,xlim=c(0,500))
hist(b$t2_lrt,br=100)
hist(b$t2_lrt[(!b$t2_toss)&(!b$t1_toss)],br=100)

1-mean((!b$t2_toss)&(!b$t1_toss))
b = b[(!b$t2_toss)&(!b$t1_toss),]

mean(b$t1_error)
mean(b$t2_error)
mean(b$t1_error | b$t2_error)
mean(b$t1_error & b$t2_error)



b_means = ddply(
	.data = b
	, .variables = .(id)
	, .fun = function(x){
		data.frame(
			rt = mean(x$t2_rt[(!x$t2_error)&(!x$t1_error)])
			, er = mean(x$t2_error[(!x$t1_error)])
		)
	}
)
ggplot(
	data = b_means
	, mapping = aes(
		x = rt
		, y = er
		, label = id
	)
)+
geom_text()

b_t2_er_fit = glmer(
	data = b[!b$t1_error,]
	, formula = t2_error_01 ~ ttoa*ctoa*cue + (1+ttoa*ctoa*cue|id)
	, family = binomial(link='probit')
	, nAGQ = 0
	, control = glmerControl(optCtrl=list(maxfun=1e7))
)
confint(b_t2_er_fit,method='Wald')
b_t2_er_preds = ezPredict(b_t2_er_fit,zero=T)
ezPlot2(
	b_t2_er_preds
	, x = ttoa
	, split = cue
	, col = ctoa
	, levels = list(
		ctoa = list(
			new_names = c('CTOA=200ms','CTOA=800ms')
		)
		, ttoa = list(
			new_names = c('150ms','350ms','900ms')
		)
		, cue = list(
			new_names = c('Invalid','Valid')
		)
	)
	, y_lab = 'Error Rate\n(log-odds; larger values mean worse performance)'
	, split_lab = 'Cue'
	, x_lab = 'TTOA'
)
ggsave(
	file = 'combined_t2_er_raw.pdf'
	, width = 8
	, height = 5
)
ezPlot2(
	b_t2_er_preds
	, x = ttoa
	, diff = cue
	, col = ctoa
	, levels = list(
		ctoa = list(
			new_names = c('CTOA=200ms','CTOA=800ms')
		)
		, ttoa = list(
			new_names = c('150ms','350ms','900ms')
		)
	)
	, y_lab = 'Cuing effect (Invalid minus Valid)\non Error Rate (log-odds)'
	, x_lab = 'TTOA'
)+
geom_hline(
	yintercept = 0
	, linetype = 3
)
ggsave(
	file = 'combined_t2_er_diff.pdf'
	, width = 8
	, height = 5
)

b_t2_rt_fit = lmer(
	data = b[(!b$t1_error)&(!b$t2_error),]
	, formula = t2_lrt ~ ttoa*ctoa*cue + (1+ttoa*ctoa*cue|id)
	, control = lmerControl(optCtrl=list(maxfun=1e7))
)
alarm()
confint(b_t2_rt_fit2,method='Wald')
b_t2_rt_preds = ezPredict(b_t2_rt_fit,zero=T)
b_t2_rt_preds$cells$value = exp(b_t2_rt_preds$cells$value)
b_t2_rt_preds$boots$value = exp(b_t2_rt_preds$boots$value)
ezPlot2(
	b_t2_rt_preds
	, x = ttoa
	, split = cue
	, col = ctoa
	, levels = list(
		ctoa = list(
			new_names = c('CTOA=200ms','CTOA=800ms')
		)
		, ttoa = list(
			new_names = c('150ms','350ms','900ms')
		)
		, cue = list(
			new_names = c('Invalid','Valid')
		)
	)
	, y_lab = 'RT (ms)'
	, split_lab = 'Cue'
	, x_lab = 'TTOA'
)
ggsave(
	file = 'combined_t2_rt_raw.pdf'
	, width = 8
	, height = 5
)
ezPlot2(
	b_t2_rt_preds
	, x = ttoa
	, diff = cue
	, col = ctoa
	, levels = list(
		ctoa = list(
			new_names = c('CTOA=200ms','CTOA=800ms')
		)
		, ttoa = list(
			new_names = c('150ms','350ms','900ms')
		)
	)
	, y_lab = 'Cuing effect (Invalid minus Valid)\non RT (ms)'
	, x_lab = 'TTOA'
)+
geom_hline(
	yintercept = 0
	, linetype = 3
)
ggsave(
	file = 'combined_t2_rt_diff.pdf'
	, width = 8
	, height = 5
)


b_t1_er_fit = glmer(
	data = b
	, formula = t1_error_01 ~ ttoa*ctoa*cue + (1+ttoa*ctoa*cue|id)
	, family = binomial(link='probit')
	, nAGQ = 0
	, control = glmerControl(optCtrl=list(maxfun=1e7))
)
alarm()
confint(b_t1_er_fit,method='Wald')
b_t1_er_preds = ezPredict(b_t1_er_fit,zero=T)
ezPlot2(
	b_t1_er_preds
	, x = ttoa
	, split = cue
	, col = ctoa
	, levels = list(
		ctoa = list(
			new_names = c('CTOA=200ms','CTOA=800ms')
		)
		, ttoa = list(
			new_names = c('150ms','350ms','900ms')
		)
		, cue = list(
			new_names = c('Invalid','Valid')
		)
	)
	, y_lab = 'Error Rate\n(log-odds; larger values mean worse performance)'
	, split_lab = 'Cue'
	, x_lab = 'TTOA'
)
ggsave(
	file = 'combined_t1_er_raw.pdf'
	, width = 8
	, height = 5
)
ezPlot2(
	b_t1_er_preds
	, x = ttoa
	, diff = cue
	, col = ctoa
	, levels = list(
		ctoa = list(
			new_names = c('CTOA=200ms','CTOA=800ms')
		)
		, ttoa = list(
			new_names = c('150ms','350ms','900ms')
		)
	)
	, y_lab = 'Cuing effect (Invalid minus Valid)\non Error Rate (log-odds)'
	, x_lab = 'TTOA'
)+
geom_hline(
	yintercept = 0
	, linetype = 3
)
ggsave(
	file = 'combined_t1_er_diff.pdf'
	, width = 8
	, height = 5
)

b_t1_rt_fit = lmer(
	data = b[(!b$t1_error),]
	, formula = t1_lrt ~ ttoa*ctoa*cue + (1+ttoa*ctoa*cue|id)
	, control = lmerControl(optCtrl=list(maxfun=1e7))
)
alarm()
confint(b_t1_rt_fit,method='Wald')
b_t1_rt_preds = ezPredict(b_t1_rt_fit,zero=T)
b_t1_rt_preds$cells$value = exp(b_t1_rt_preds$cells$value)
b_t1_rt_preds$boots$value = exp(b_t1_rt_preds$boots$value)
ezPlot2(
	b_t1_rt_preds
	, x = ttoa
	, split = cue
	, col = ctoa
	, levels = list(
		ctoa = list(
			new_names = c('CTOA=200ms','CTOA=800ms')
		)
		, ttoa = list(
			new_names = c('150ms','350ms','900ms')
		)
		, cue = list(
			new_names = c('Invalid','Valid')
		)
	)
	, y_lab = 'RT (ms)'
	, split_lab = 'Cue'
	, x_lab = 'TTOA'
)
ggsave(
	file = 'combined_t1_rt_raw.pdf'
	, width = 8
	, height = 5
)
ezPlot2(
	b_t1_rt_preds
	, x = ttoa
	, diff = cue
	, col = ctoa
	, levels = list(
		ctoa = list(
			new_names = c('CTOA=200ms','CTOA=800ms')
		)
		, ttoa = list(
			new_names = c('150ms','350ms','900ms')
		)
	)
	, y_lab = 'Cuing effect (Invalid minus Valid)\non RT (ms)'
	, x_lab = 'TTOA'
)+
geom_hline(
	yintercept = 0
	, linetype = 3
)
ggsave(
	file = 'combined_t1_rt_diff.pdf'
	, width = 8
	, height = 5
)

########
# Analyze prp data
########

no_cue = a[is.na(a$cue),]

ggplot(
	data = no_cue
	, mapping = aes(
		x = t2_rt
		, fill = t2_error
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
ggplot(
	data = no_cue
	, mapping = aes(
		x = t2_lrt
		, fill = t2_error
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
ggplot(
	data = no_cue
	, mapping = aes(
		x = t1_rt
		, fill = t1_error
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
ggplot(
	data = no_cue
	, mapping = aes(
		x = t1_lrt
		, fill = t1_error
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

no_cue = ddply(
	.data = no_cue
	, .variables = .(id)
	, .fun = function(x){
		t2_med_lrt = median(x$t2_lrt[!x$t2_error])
		t2_madmed_lrt = madmed(x$t2_lrt[!x$t2_error])
		t2_lo_crit = t2_med_lrt-(5*t2_madmed_lrt)
		t2_hi_crit = t2_med_lrt+(5*t2_madmed_lrt)
		x$t2_hi = x$t2_lrt>t2_hi_crit
		x$t2_lo = x$t2_lrt<t2_lo_crit
		x$t2_toss = x$t2_hi | x$t2_lo
		t1_med_lrt = median(x$t1_lrt[!x$t1_error])
		t1_madmed_lrt = madmed(x$t1_lrt[!x$t1_error])
		t1_lo_crit = t1_med_lrt-(5*t1_madmed_lrt)
		t1_hi_crit = t1_med_lrt+(5*t1_madmed_lrt)
		x$t1_hi = x$t1_lrt>t1_hi_crit
		x$t1_lo = x$t1_lrt<t1_lo_crit
		x$t1_toss = x$t1_hi | x$t1_lo
		return(x)
	}
)
ggplot(
	data = no_cue[!no_cue$t2_error,]
	, mapping = aes(
		x = t2_rt
		, fill = t2_toss
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
ggplot(
	data = no_cue[!no_cue$t2_error,]
	, mapping = aes(
		x = t2_lrt
		, fill = t2_toss
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
ggplot(
	data = no_cue[!no_cue$t1_error,]
	, mapping = aes(
		x = t1_rt
		, fill = t1_toss
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
ggplot(
	data = no_cue[!no_cue$t1_error,]
	, mapping = aes(
		x = t1_lrt
		, fill = t1_toss
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
hist(no_cue$t2_rt,br=100)
hist(no_cue$t2_rt[(!no_cue$t2_toss)&(!no_cue$t1_toss)],br=100)
hist(no_cue$t2_rt,br=100,xlim=c(0,500))
hist(no_cue$t2_rt[(!no_cue$t2_toss)&(!no_cue$t1_toss)],br=100,xlim=c(0,500))
hist(no_cue$t2_lrt,br=100)
hist(no_cue$t2_lrt[(!no_cue$t2_toss)&(!no_cue$t1_toss)],br=100)

hist(no_cue$t1_rt,br=100)
hist(no_cue$t1_rt[(!no_cue$t2_toss)&(!no_cue$t1_toss)],br=100)
hist(no_cue$t1_rt,br=100,xlim=c(0,500))
hist(no_cue$t1_rt[(!no_cue$t2_toss)&(!no_cue$t1_toss)],br=100,xlim=c(0,500))
hist(no_cue$t2_lrt,br=100)
hist(no_cue$t2_lrt[(!no_cue$t2_toss)&(!no_cue$t1_toss)],br=100)

1-mean((!no_cue$t2_toss)&(!no_cue$t1_toss))
no_cue = no_cue[(!no_cue$t2_toss)&(!no_cue$t1_toss),]

mean(no_cue$t1_error)
mean(no_cue$t2_error)
mean(no_cue$t1_error | no_cue$t2_error)
mean(no_cue$t1_error & no_cue$t2_error)


no_cue_t2_er_fit = glmer(
	data = no_cue[!no_cue$t1_error,]
	, formula = t2_error_01 ~ ttoa + (1+ttoa|id)
	, family = binomial(link='probit')
	, nAGQ = 0
	, control = glmerControl(optCtrl=list(maxfun=1e7))
)
confint(no_cue_t2_er_fit,method='Wald')
no_cue_t2_er_preds = ezPredict(no_cue_t2_er_fit,zero=T)
ezPlot2(
	no_cue_t2_er_preds
	, x = ttoa
	, levels = list(
		ttoa = list(
			new_names = c('150ms','350ms','900ms')
		)
	)
	, y_lab = 'Error Rate\n(log-odds; larger values mean worse performance)'
	, x_lab = 'TTOA'
)
ggsave(
	file = 'nocue_t2_er.pdf'
	, width = 5
	, height = 5
)

no_cue_t2_rt_fit = lmer(
	data = no_cue[(!no_cue$t1_error)&(!no_cue$t2_error),]
	, formula = t2_lrt ~ ttoa + (1+ttoa|id)
	, control = lmerControl(optCtrl=list(maxfun=1e7))
)
alarm()
confint(no_cue_t2_rt_fit,method='Wald')
no_cue_t2_rt_preds = ezPredict(no_cue_t2_rt_fit,zero=T)
no_cue_t2_rt_preds$cells$value = exp(no_cue_t2_rt_preds$cells$value)
no_cue_t2_rt_preds$boots$value = exp(no_cue_t2_rt_preds$boots$value)
ezPlot2(
	no_cue_t2_rt_preds
	, x = ttoa
	, levels = list(
		ttoa = list(
			new_names = c('150ms','350ms','900ms')
		)
	)
	, y_lab = 'RT (ms)'
	, x_lab = 'TTOA'
)
ggsave(
	file = 'nocue_t2_rt.pdf'
	, width = 5
	, height = 5
)


no_cue_t1_er_fit = glmer(
	data = no_cue
	, formula = t1_error_01 ~ ttoa + (1+ttoa|id)
	, family = binomial(link='probit')
	, nAGQ = 0
	, control = glmerControl(optCtrl=list(maxfun=1e7))
)
alarm()
confint(no_cue_t1_er_fit,method='Wald')
no_cue_t1_er_preds = ezPredict(no_cue_t1_er_fit,zero=T)
ezPlot2(
	no_cue_t1_er_preds
	, x = ttoa
	, levels = list(
		ttoa = list(
			new_names = c('150ms','350ms','900ms')
		)
	)
	, y_lab = 'Error Rate\n(log-odds; larger values mean worse performance)'
	, x_lab = 'TTOA'
)
ggsave(
	file = 'nocue_t1_er.pdf'
	, width = 5
	, height = 5
)

no_cue_t1_rt_fit = lmer(
	data = no_cue[(!no_cue$t1_error),]
	, formula = t1_lrt ~ ttoa + (1+ttoa|id)
	, control = lmerControl(optCtrl=list(maxfun=1e7))
)
alarm()
confint(no_cue_t1_rt_fit,method='Wald')
no_cue_t1_rt_preds = ezPredict(no_cue_t1_rt_fit,zero=T)
no_cue_t1_rt_preds$cells$value = exp(no_cue_t1_rt_preds$cells$value)
no_cue_t1_rt_preds$boots$value = exp(no_cue_t1_rt_preds$boots$value)
ezPlot2(
	no_cue_t1_rt_preds
	, x = ttoa
	, levels = list(
		ttoa = list(
			new_names = c('150ms','350ms','900ms')
		)
	)
	, y_lab = 'RT (ms)'
	, x_lab = 'TTOA'
)
ggsave(
	file = 'nocue_t1_rt.pdf'
	, width = 5
	, height = 5
)


########
# Analyze no_t1 data
########

no_t1 = a[is.na(a$t1_identity),]

ggplot(
	data = no_t1
	, mapping = aes(
		x = t2_rt
		, fill = t2_error
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
ggplot(
	data = no_t1
	, mapping = aes(
		x = log(t2_rt)
		, fill = t2_error
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


no_t1 = ddply(
	.data = no_t1
	, .variables = .(id)
	, .fun = function(x){
		med_lrt = median(x$t2_lrt[!x$t2_error])
		madmed_lrt = madmed(x$t2_lrt[!x$t2_error])
		lo_crit = med_lrt-(5*madmed_lrt)
		hi_crit = med_lrt+(5*madmed_lrt)
		x$hi = x$t2_lrt>hi_crit
		x$lo = x$t2_lrt<lo_crit
		x$toss = x$hi | x$lo
		return(x)
	}
)
mean(no_t1$toss)
ggplot(
	data = no_t1
	, mapping = aes(
		x = t2_rt
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
ggplot(
	data = no_t1
	, mapping = aes(
		x = t2_lrt
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



hist(no_t1$t2_rt,br=100)
hist(no_t1$t2_rt[!no_t1$toss],br=100)
hist(no_t1$t2_rt,br=100,xlim=c(0,500))
hist(no_t1$t2_rt[!no_t1$toss],br=100,xlim=c(0,500))
hist(no_t1$t2_lrt,br=100)
hist(no_t1$t2_lrt[!no_t1$toss],br=100)

no_t1 = no_t1[!no_t1$toss,]


no_t1_er_fit = glmer(
	data = no_t1
	, formula = t2_error_01 ~ ctoa*cue + (1+ctoa*cue|id)
	, family = binomial(link='probit')
	, nAGQ = 0
	, control = glmerControl(optCtrl=list(maxfun=1e7))
)
alarm()
confint(no_t1_er_fit,method='Wald')
no_t1_er_preds = ezPredict(no_t1_er_fit,zero=T)
ezPlot2(
	no_t1_er_preds
	, x = ctoa
	, split = cue
	, levels = list(
		ctoa = list(
			new_names = c('200ms','800ms')
		)
		, cue = list(
			new_names = c('Invalid','Valid')
		)
	)
	, y_lab = 'Error Rate\n(log-odds; larger values mean worse performance)'
	, split_lab = 'Cue'
	, x_lab = 'CTOA'
)
ggsave(
	file = 'not1_t2_er_raw.pdf'
	, width = 5
	, height = 5
)
ezPlot2(
	no_t1_er_preds
	, x = ctoa
	, diff = cue
	, levels = list(
		ctoa = list(
			new_names = c('200ms','800ms')
		)
		, cue = list(
			new_names = c('Invalid','Valid')
		)
	)
	, y_lab = 'Cuing effect (Invalid minus Valid)\non Error Rate (log-odds)'
	, x_lab = 'CTOA'
)+
geom_hline(
	yintercept = 0
	, linetype = 3
)
ggsave(
	file = 'not1_t2_er_diff.pdf'
	, width = 5
	, height = 5
)

no_t1_rt_fit = lmer(
	data = no_t1[!no_t1$t2_error,]
	, formula = t2_lrt ~ ctoa*cue + (1+ctoa*cue|id)
	, control = lmerControl(optCtrl=list(maxfun=1e7))
)
alarm()
confint(no_t1_rt_fit,method='Wald')
no_t1_rt_preds = ezPredict(no_t1_rt_fit,zero=T)
no_t1_rt_preds$cells$value = exp(no_t1_rt_preds$cells$value)
no_t1_rt_preds$boots$value = exp(no_t1_rt_preds$boots$value)
ezPlot2(
	no_t1_rt_preds
	, x = ctoa
	, split = cue
	, levels = list(
		ctoa = list(
			new_names = c('200ms','800ms')
		)
		, cue = list(
			new_names = c('Invalid','Valid')
		)
	)
	, y_lab = 'RT (ms)'
	, split_lab = 'Cue'
	, x_lab = 'CTOA'
)
ggsave(
	file = 'not1_t2_rt_raw.pdf'
	, width = 5
	, height = 5
)
ezPlot2(
	no_t1_rt_preds
	, x = ctoa
	, diff = cue
	, levels = list(
		ctoa = list(
			new_names = c('200ms','800ms')
		)
		, cue = list(
			new_names = c('Invalid','Valid')
		)
	)
	, y_lab = 'Cuing effect (Invalid minus Valid)\non RT (ms)'
	, x_lab = 'CTOA'
)+
geom_hline(
	yintercept = 0
	, linetype = 3
)
ggsave(
	file = 'not1_t2_rt_diff.pdf'
	, width = 5
	, height = 5
)



































