library(plyr)
library(ggplot2) 
library(stringr)
library(gridExtra)

# use this vector length later in text 
files = list.files("~/GitHub/multimodal_ior/For Analysis/TXT")

# read in data 
a = ldply(
  .data = list.files(
    path = '~/GitHub/multimodal_ior/For Analysis/TXT'
    , pattern = '_data.txt'
    , full.names = T
    , recursive = T
  )
  , .fun = function(x){
    read.table(
      file = x
      , header = TRUE
      , sep = "\t"
    )
  }
)

################################### EYE TRACKING #################################

# order data frame as a function of dates then create reverse ordered list of dates 
a$date = as.Date(sprintf("%i/%i", a$month, a$day), format = "%m/%d")
a= a[order(a$date),]
date_order = as.vector(levels(as.factor(a$date)))
date_order = rev(date_order)

# remember which ids have the same date 
date_id = table(a$date, a$id)

# get rid of practice 
#a = a[a$block != "practice",]

cc = NULL
bb = NULL

pnum = c(2:5, 7:length(files) )

# cycle through each participant folder
for (w in pnum) {
  setwd(sprintf("~/GitHub/multimodal_ior/For Analysis/ASC/p%i", w))
  
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
  
  if (w>10 & w<16) {
    temp = 16 - w
    b = a[a$date == date_order[temp],]
  } else if (w == 10) {
      b = a[a$id == sprintf("p%i", w),]
  } else if (w == 9) {
      b = a[a$id == sprintf("p0%i", w),]
  } else {
      temp = 16 - w
      b = a[a$date == date_order[temp - 1],] 
  }
  
  b = merge(b,eye_trial_data,all=T)
  c = b
  
  
  # when exactly do blinks and saccades start, for all participants
  for(i in 1:nrow(b)){
    temp = min(blinks$start[ (blinks$start>b$trial_start[i]) & (blinks$start<b$trialDone[i]) ])
    b$blink_start[i] = (temp - b$trial_start[i])
    b$blink_after_cue[i] = (temp - (b$trial_start[i] + b$fixation_duration*1000) )
  }
  
  
  for(i in 1:nrow(b)){
    temp = min(saccades$start[ (saccades$start>b$trial_start[i]) & (saccades$start<b$trialDone[i]) ])
    b$saccade_start[i] = (temp - b$trial_start[i])
    b$saccade_after_cue[i] = (temp - (b$trial_start[i] + b$fixation_duration*1000) )
  }  
  
  
  bb = rbind(bb, b)
  
# critical blinks and saccades for first 10 participants
  if (w < 11) {
    c$critical_blink2 = FALSE
    c$critical_saccade2 = FALSE
    c$start_critical_period = c$trial_start + (c$fixation_duration)*1000
    c$end_critical_period = c$start_critical_period + 1300
    for(i in 1:nrow(b)){
      c$critical_blink2[i] = sum( ( (blinks$start>c$start_critical_period[i]) & (blinks$start<c$end_critical_period[i]) ) | ( (blinks$end>c$start_critical_period[i]) & (blinks$end<c$end_critical_period[i]) ) ) > 0
      c$critical_saccade2[i] = sum( ( (saccades$start>c$start_critical_period[i]) & (saccades$start<c$end_critical_period[i]) ) | ( (saccades$end>c$start_critical_period[i]) & (saccades$end<c$end_critical_period[i]) ) ) > 0
    }
    cc = rbind(cc, c)
  }
}

# do for each participant - facet wrap ?

# when do the saccades occur?
hist(bb$saccade_start, br = 100, main = "Saccade onset relative to trial initiation", xlab = "Time after trial initiation (ms)")
hist(bb$saccade_after_cue, br = 100, main = "Saccade onset relative to cue onset", xlab = "Time after cue onset (ms)")

# when do the blinks occur?
hist(bb$blink_start, br = 100, main = "Blink onset relative to trial initiation", xlab = "Time after trial initiation (ms)")
hist(bb$blink_after_cue, br = 100, main = "Blink onset relative to cue onset", xlab = "Time after cue onset (ms)")


############################## Klein Table #################################

# I want 1 row/subject and 1 column/cueing/cue_modality/target_modality + 1 column for exclusion proportion

d=a

# get rid of "tactice"
d[d$target_modality == "tactice",]$target_modality = "tactile"

# order according to date to make easier 
d$date = as.Date(sprintf("%i/%i", d$month, d$day), format = "%m/%d")
d = d[order(d$date),]

# check to see that this matches with participant ids
date_id = table(d$date, d$id)

d$cued = FALSE
d$cued[d$target_location == "right" & d$cue_location == "right" | d$target_location == "left" & d$cue_location == "left"] = TRUE

d = d[d$block != "practice",]

d$SorB = FALSE
d$SorB[d$blink == TRUE | d$saccade == TRUE] = TRUE 

d$count = TRUE 

dSum = aggregate(SorB ~ hour + date, data = d, FUN = sum)
dLength = aggregate(count ~ hour + date, data = d, FUN = sum)
dProp = dSum$SorB/dLength$count

# now add the critical intervals from participants 1 to 10 
cc$CritSorB = FALSE
cc$CritSorB[cc$critical_blink2 == TRUE | cc$critical_saccade2 == TRUE] = TRUE 

critSum = aggregate(CritSorB ~ id, data = cc, FUN = sum)
cc$count = TRUE 
critLength = aggregate(count ~ id, data = cc, FUN = sum)
critProp = critSum$CritSorB/critLength$count

# now do the same for critical interval
d$CritSorB = FALSE
d$CritSorB[d$critical_blink == TRUE | d$critical_saccade == TRUE] = TRUE 

dSum2 = aggregate(CritSorB ~ hour + date, data = d, FUN = sum)
dLength2 = aggregate(count ~ hour + date, data = d, FUN = sum)
dProp2 = dSum2$CritSorB/dLength2$count

# add early critical interval to prop2
dProp2[2:5] = critProp[1:4]
dProp2[7:10] = critProp[5:8]
dProp2[1] = NA
dProp2[6] = NA

#Get rid of blink/saccade trials
e = d
e = e[e$SorB == FALSE,]

# LOOK AT IOR WITHOUT BLINKS OR SACCADES 
eAgg = aggregate(target_response_rt ~ cued + cue_modality + target_modality + hour + date, data = e, FUN = mean)

# create data frame of IOR and add other parameters  
kleinTable = data.frame(matrix(eAgg$target_response_rt, nrow = length(files), byrow = TRUE))

usedAgg = aggregate(id ~ hour + date, a, length)
used_trials = usedAgg$id * (1 - dProp)

kleinTable = cbind(round(used_trials), round(kleinTable) )
kleinTable = cbind(round(dProp, digits = 2), kleinTable)

# LOOK AT IOR WITHOUT CRITICAL ... 
f = d
f = f[f$CritSorB == FALSE,]

fAgg = aggregate(target_response_rt ~ cued + cue_modality + target_modality + hour + date, data = f, FUN = mean)

# create data frame of IOR  
kleinTable2 = data.frame(matrix(fAgg$target_response_rt, nrow = length(files), byrow = TRUE))

# get rid of participant 1 and 6 crit 
kleinTable2[1,] = NA
kleinTable2[6,] = NA

# add data frame and parameters to initial data frame 
usedAgg2 = aggregate(id ~ hour + date, a, length)
used_trials2 = usedAgg$id * (1 - dProp2)

kleinTable = cbind(kleinTable, round(dProp2, digits = 2) ) 
kleinTable = cbind(kleinTable, round(used_trials2) )
kleinTable = cbind(kleinTable, round(kleinTable2) )


# set names for all columns: watch out for orderering, etc. 
names(kleinTable) = c("exclusion proportion - entire trial", "trials used - entire trial", "uncued TV", "cued TV", "uncued VV", "cued VV", "uncued TT", "cued TT", "uncued VT", "cued VT", 
                      "exclusion proportion - cue to 300 ms post", "trials used - cue to 300 ms post", "uncued TV", "cued TV", "uncued VV", "cued VV", "uncued TT", "cued TT", "uncued VT", "cued VT")

# check to make sure that names match actual columns 
perms = eAgg[,2:3]

participants = NULL 
for (i in 1:length(files)) {
  if (i < 10) {
    temp = sprintf("p0%i", i)
    participants = rbind(participants, temp)
  } else {
    temp = sprintf("p%i", i)
    participants = rbind(participants, temp)
  }
}
kleinTable = cbind(participants, kleinTable)

plot.table(kleinTable)

# now make summary table of critical vs. non-critical for each "group"

# NOTE: must be a way to cycle through different sequences 


noFed = 1:5
imFed = 6
verbFed = 7:12
restFed = 13:15

v = cbind(noFed, imFed, verbFed, restFed)

df = NULL

# NOTE: I could likely just create another column with factors and then aggregate instead ... 
for (d in 1:length(colnames(v) ) ) {
  dup = duplicated(v[,d])
  inde = length(dup) - sum(dup[dup == TRUE])
  vect = v[,d][1:inde]
  
  df_temp = NULL
  
  for (i in 2:length(kleinTable[1,]) ) {
    if (i %in% c(3,13) ) {
      temp = sum(kleinTable[vect, i], na.rm = TRUE)
      df_temp = cbind(df_temp, temp)      
    } else {
    temp = mean(kleinTable[vect, i], na.rm = TRUE)
    df_temp = cbind(df_temp, temp)
    }
  }
  
  df = rbind(df, df_temp)
}

df = as.data.frame(df)

# round now
df[,-c(1,11)] = round(df[,-c(1,11)])
df[,c(1,11)] = round(df[,c(1,11)], digits = 2)

# set names for all columns
names(df) = c("exclusion proportion - entire trial", "trials used - entire trial", "uncued TV", "cued TV", "uncued VV", "cued VV", "uncued TT", "cued TT", "uncued VT", "cued VT", 
                      "exclusion proportion - cue to 300 ms post", "trials used - cue to 300 ms post", "uncued TV", "cued TV", "uncued VV", "cued VV", "uncued TT", "cued TT", "uncued VT", "cued VT")

# set row/group names
groups = c("no feedback", "immediate feedback", "immediate practice feedback + verbal instructions", " ... + rest break feedback")
df = cbind(groups, df)





