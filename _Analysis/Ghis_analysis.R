library(plyr)
library(ggplot2) 
library(stringr)

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

c = NULL
pnum = c(2:5, 7:length(files))

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
  } else if (sprintf("p%i", w) == "p10" ) {
      b = a[a$id == sprintf("p%i", w),]
  } else if (sprintf("p%i", w) == "p9") {
      b = a[a$id == sprintf("p%0i", w),]
  } else {
      temp = 16 - w
      b = a[a$date == date_order[temp - 1],] 
  }
  
  b = merge(b,eye_trial_data,all=T)

# critical blinks and saccades for first 10 participants
#  if (w < 11) {
#    b$critical_blink = FALSE
#    b$critical_saccade = FALSE
#    b$start_critical_period = b$trial_start + (b$fixation_duration)*1000
#    b$end_critical_period = b$start_critical_period + 1300
#    for(i in 1:nrow(b)){
#      b$critical_blink[i] = sum( ( (blinks$start>b$start_critical_period[i]) & (blinks$start<b$end_critical_period[i]) ) | ( (blinks$end>b$start_critical_period[i]) & (blinks$end<b$end_critical_period[i]) ) ) > 0
#      b$critical_saccade[i] = sum( ( (saccades$start>b$start_critical_period[i]) & (saccades$start<b$end_critical_period[i]) ) | ( (saccades$end>b$start_critical_period[i]) & (saccades$end<b$end_critical_period[i]) ) ) > 0
#    }
#  }

# when exactly to blinks and saccades start, for all participants
  for(i in 1:nrow(b)){
    temp = min(blinks$start[ (blinks$start>b$trial_start[i]) & (blinks$start<b$trialDone[i]) ])
    b$blink_start[i] = (temp - b$trial_start[i])
    b$blink_after_cue[i] = (temp - (b$trial_start[i] + b$fixation_duration*1000) )
  }
  
  
  eyes = NULL
  for(i in 1:nrow(b)){
    temp = min(saccades$start[ (saccades$start>b$trial_start[i]) & (saccades$start<b$trialDone[i]) ])
    b$saccade_start[i] = (temp - b$trial_start[i])
    b$saccade_after_cue[i] = (temp - (b$trial_start[i] + b$fixation_duration*1000) )
  }  
  
  
  c = rbind(c, b)
}

# do for each participant - facet wrap

# when do the saccades occur?
hist(c$saccade_start, br = 100)
hist(c$saccade_after_cue, br = 100)

# how many saccades occur before the cue?
sacProp = sum(c$saccade_after_cue<0)/sum(c$saccade_after_cue) #...

# when do the blinks occur?
hist(c$blink_after_cue, br = 100)
hist(c$blink_start, br = 100)


############################## Klein Table #################################

# I want 1 row/subject and 1 column/cueing/cue_modality/target_modality + 1 column for exclusion proportion

b=a

# get rid of "tactice"
b[b$target_modality == "tactice",]$target_modality = "tactile"

# order according to date to make easier 
b$date = as.Date(sprintf("%i/%i", b$month, b$day), format = "%m/%d")
b = b[order(b$date),]

# check to see that this matches with participant ids
date_id = table(b$date, b$id)

b$cued = FALSE
b$cued[b$target_location == "right" & b$cue_location == "right" | b$target_location == "left" & b$cue_location == "left"] = TRUE

b = b[b$block != "practice",]

b$SorB = FALSE
b$SorB[b$blink == TRUE | b$saccade == TRUE] = TRUE 

b$count = TRUE 

bSum = aggregate(SorB ~ hour + date, data = b, FUN = sum)
bLength = aggregate(count ~ hour + date, data = b, FUN = sum)
bProp = bSum$SorB/bLength$count

# now do the same for critical interval
b$CritSorB = FALSE
b$CritSorB[b$critical_blink == TRUE | b$critical_saccade == TRUE] = TRUE 

bSum2 = aggregate(CritSorB ~ hour + date, data = b, FUN = sum)
bLength2 = aggregate(count ~ hour + date, data = b, FUN = sum)
bProp2 = bSum2$CritSorB/bLength2$count

#Get rid of blink/saccade trials
c = b
c = c[c$SorB == FALSE,]
cAgg = aggregate(target_response_rt ~ cued + cue_modality + target_modality + hour + date, data = c, FUN = mean)

kleinTable = data.frame(matrix(cAgg$target_response_rt, nrow = length(files), byrow = TRUE))

kleinTable$YUP[1:length(files)] = bProp
kleinTable$YUP2[1:length(files)] = bProp2
names(kleinTable) = c("uncued TV", "cued TV", "uncued VV", "cued VV", "uncued TT", "cued TT", "uncued VT", "cued VT", "blink or saccade trial proportion", "critical interval proportion")

rNames = NULL 
for (i in 1:length(files)) {
temp = sprintf("p0%i", i)
rNames = cbind(rNames, temp)
}
row.names(kleinTable) = rNames





