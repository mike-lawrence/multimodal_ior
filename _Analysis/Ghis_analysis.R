library(plyr)
library(ggplot2) 

files = list.files('../For Analysis/TXT')

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
      , col.names = c("id",  "year",	"month",	"day",	"hour",	"minute",	"sex",	"age",	"handedness",	"message_viewing_time",	"block",	"trial_num",	"trial_initiation_time",	"fixation_duration", 	"cue_modality",	"cue_location",	"target_location",	"target_modality",	"target_response_key",	"target_response_rt",	"pre_target_response",	"feedback_response",	"recalibration",	"blink",	"saccade",	"biggest_small_saccade",	"critical_blink",	"critical_saccade", "target_started_TF")
    )
  }
)


# I want 1 row/subject and 1 column/cueing/cue_modality/target_modality + 1 column for exclusion proportion


############################## For Ray #################################

# I want 1 row/subject and 1 column/cueing/cue_modality/target_modality + 1 column for exclusion proportion

b=a

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
names(kleinTable) = c("uncued TT", "cued TT", "uncued VT", "cued VT", "uncued TV", "cued TV", "uncued VV", "cued VV", "blink or saccade trial proportion", "critical interval proportion")

rNames = NULL 
for (i in 1:length(files)) {
temp = sprintf("p0%i", i)
rNames = cbind(rNames, temp)
}
row.names(kleinTable) = rNames





