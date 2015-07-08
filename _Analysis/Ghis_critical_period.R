############################ Define Datasets and Open Libraries #########################

library(stringr)


# no feedback
setwd("~/R/MultiIOR/Data/Behavioral Pilots/This Directory")
library(plyr)

files = list.files()
aa = ldply(
  .data = files
  , .fun = function(x){
    to_return = read.table(
      file = x
      , header = TRUE
      , as.is = TRUE
      , sep = "\t"
    )
    return(to_return)
  }
)

# rest by rest feedback + practice immediate 
setwd("~/R/MultiIOR/Data/Behavioral Pilots/Other Directory")
library(plyr)

files = list.files()
aaa = ldply(
  .data = files
  , .fun = function(x){
    to_return = read.table(
      file = x
      , header = TRUE
      , as.is = TRUE
      , sep = "\t"
    )
    return(to_return)
  }
)

# combine both datasets together
rbind.all.columns <- function(x, y) {
  
  x.diff <- setdiff(colnames(x), colnames(y))
  y.diff <- setdiff(colnames(y), colnames(x))
  
  x[, c(as.character(y.diff))] <- NA
  
  y[, c(as.character(x.diff))] <- NA
  
  return(rbind(x, y))
}

a = rbind.all.columns(aa, aaa)

######################### Eyetracking #############################


c = NULL
pnum = c(2:5, 7:10)
# cycle through each participant folder
for (w in pnum) {
  setwd(sprintf("~/R/MultiIOR/Data/Shared Directory/p%i", w))
  
  saccades = NULL
  blinks = NULL
  eye_trial_data = NULL 
  
  saccades = read.table('saccades.txt',sep='\t')
  names(saccades)[2:3] = c('start','end')
  blinks = read.table('blinks.txt',sep='\t')
  names(blinks)[2:3] = c('start','end')
  
  eye_trial_data = read.table('trials.txt',sep='\t')
  names(eye_trial_data) = c('time','message','id' , 'block','trial_num')
  
  eye_trial_data = eye_trial_data[eye_trial_data$block!='practice',]
  eye_trial_data$block = as.numeric(as.character(eye_trial_data$block))
  
  eye_trial_data = reshape(eye_trial_data,direction='wide',v.names='time',idvar=c('id','block','trial_num'),timevar='message')
  names(eye_trial_data) = str_replace(names(eye_trial_data),'time.','')
  
  if (sprintf("p%i", w) == "p10"){
    b = a[a$id == sprintf("p%i", w),]
  }
  else{
    b = a[a$id == sprintf("p0%i", w),]
  }
  b = merge(b,eye_trial_data,all=T)
  
  b$critical_blink = FALSE
  b$critical_saccade = FALSE
  b$start_critical_period = b$trial_start + (b$fixation_duration)*1000
  b$end_critical_period = b$start_critical_period + 1300
  for(i in 1:nrow(b)){
    b$critical_blink[i] = sum( ( (blinks$start>b$start_critical_period[i]) & (blinks$start<b$end_critical_period[i]) ) | ( (blinks$end>b$start_critical_period[i]) & (blinks$end<b$end_critical_period[i]) ) ) > 0
    b$critical_saccade[i] = sum( ( (saccades$start>b$start_critical_period[i]) & (saccades$start<b$end_critical_period[i]) ) | ( (saccades$end>b$start_critical_period[i]) & (saccades$end<b$end_critical_period[i]) ) ) > 0
  }
  
  # this is the other critical period defined by alex   
  b$critical_blink2 = FALSE
  b$critical_saccade2 = FALSE
  
  # use the same start of critical period 
  b$end_critical_period2 = b$start_critical_period + 1000 + b$target_response_rt
  for(i in 1:nrow(b)){
    b$critical_blink2[i] = sum( ( (blinks$start>b$start_critical_period[i]) & (blinks$start<b$end_critical_period2[i]) ) | ( (blinks$end>b$start_critical_period[i]) & (blinks$end<b$end_critical_period2[i]) ) ) > 0
    b$critical_saccade2[i] = sum( ( (saccades$start>b$start_critical_period[i]) & (saccades$start<b$end_critical_period2[i]) ) | ( (saccades$end>b$start_critical_period[i]) & (saccades$end<b$end_critical_period2[i]) ) ) > 0
  }    
  
  c = rbind(c, b)
}

# aggregate data 
c = c[c$block != "practice",]
c$either = FALSE
c$either[c$critical_blink == TRUE | c$critical_saccade == TRUE] = TRUE 

discard = aggregate(either ~ id, data = c, FUN = sum)
discardLength = aggregate(either ~ id, data = c, FUN = length)
discardProp = discard$either/discardLength$either

# for alex's interval
c$either2 = FALSE
c$either2[c$critical_blink2 == TRUE | c$critical_saccade2 == TRUE] = TRUE 

discard2 = aggregate(either2 ~ id, data = c, FUN = sum)
discardLength2 = aggregate(either2 ~ id, data = c, FUN = length)
discardProp2 = discard2$either2/discardLength2$either2

# any blink or saccade
a$either = FALSE
a$either[a$blink == TRUE | a$saccade == TRUE] = TRUE 

#I get rid of participant 1 in here
oldDiscard = aggregate(either ~ id, data = a[a$id != "p01",], FUN = sum)
oldDiscardLength = aggregate(either ~ id, data = a[a$id != "p01",], FUN = length)
oldDiscardProp = oldDiscard$either/oldDiscardLength$either

#I put into a matrix
mat = matrix(c(oldDiscardProp, discardProp, discardProp2), nrow = 3, byrow = TRUE)

# and data frame...
dat = data.frame(mat)

########################## IOR ############################

# new IOR
d = c[c$either == FALSE,]
d$cued = FALSE
d$cued[d$target_location == "right" & d$cue_location == "right" | d$target_location == "left" & d$cue_location == "left"] = TRUE

newIOR = aggregate(target_response_rt ~ id + cued, data = d, FUN = mean)
newIORDiff = newIOR$target_response_rt[(length(pnum)+1):(2*length(pnum))] - newIOR$target_response_rt[1:length(pnum)]

# newer IOR
dd = c[c$either2 == FALSE,]
dd$cued = FALSE
dd$cued[dd$target_location == "right" & dd$cue_location == "right" | dd$target_location == "left" & dd$cue_location == "left"] = TRUE

newIOR2 = aggregate(target_response_rt ~ id + cued, data = dd, FUN = mean)
newIORDiff2 = newIOR2$target_response_rt[(length(pnum)+1):(2*length(pnum))] - newIOR2$target_response_rt[1:(length(pnum))]


# look at old IOR
a2 = a[a$either == FALSE & a$id != "p01" & a$block != "practice",]
a2$cued = FALSE
a2$cued[a2$target_location == "right" & a2$cue_location == "right" | a2$target_location == "left" & a2$cue_location == "left"] = TRUE

oldIOR = aggregate(target_response_rt ~ id + cued, data = a2, FUN = mean)
oldIORDiff = oldIOR$target_response_rt[(length(pnum)+1):(2*length(pnum))] - oldIOR$target_response_rt[1:(length(pnum))]

# matrix
mat2 = matrix(c(oldIORDiff,newIORDiff, newIORDiff2), nrow = 3, byrow = TRUE)
dat2 = data.frame(mat2)
