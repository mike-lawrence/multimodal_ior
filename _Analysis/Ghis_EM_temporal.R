library(plyr)
library(ggplot2) 
library(stringr)

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
      , row.names = NULL
      , sep = "\t"
      , fill = TRUE
      , skip = 1
      , col.names = c("id",  "year",  "month",	"day",	"hour",	"minute",	"sex",	"age",	"handedness",	"message_viewing_time",	"block",	"trial_num",	"trial_initiation_time",	"fixation_duration", 	"cue_modality",	"cue_location",	"target_location",	"target_modality",	"target_response_key",	"target_response_rt",	"pre_target_response",	"feedback_response",	"recalibration",	"blink",	"saccade",	"biggest_small_saccade",	"critical_blink",	"critical_saccade", "target_started_TF")
    )
  }
)

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
pnum = c(2:5, 7:15)

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
  }
  else if (sprintf("p%i", w) %in%  c("p10", "p09") ) {
    b = a[a$id == sprintf("p%i", w),]
  }
  else{
    temp = 16 - w
    b = a[a$date == date_order[temp - 1],] 
  }

  b = merge(b,eye_trial_data,all=T)
  
  for(i in 1:nrow(b)){
    b$blink_start[i] = blinks_short - b$trial_start[i]
    b$saccade_start[i] = saccades$start - b$trial_start[i]
  }

  c = rbind(c, b)
}
