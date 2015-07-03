############################ Define Datasets #########################

# no feedback
setwd("~/R/MultiIOR/Data/Behavioral Pilots/This Directory")
library(plyr)

files = list.files()
a = ldply(
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

# immediate feedback
setwd("~/R/MultiIOR/Data/Behavioral Pilots/Feedback")
library(plyr)

files = list.files()
afed = ldply(
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

# eeg pilots
setwd("~/R/MultiIOR/Data/EEG Pilots")
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


############################## For Ray #################################

# I want 1 row/subject and 1 column/cueing/cue_modality/target_modality + 1 column for exclusion proportion

b=a
b$cued = FALSE
b$cued[b$target_location == "right" & b$cue_location == "right" | b$target_location == "left" & b$cue_location == "left"] = TRUE

b = b[b$block != "practice",]

b$SorB = FALSE
b$SorB[b$blink == TRUE | b$saccade == TRUE] = TRUE 

b$count = TRUE 

bSum = aggregate(SorB ~ id, data = b, FUN = sum)
bLength = aggregate(count ~ id, data = b, FUN = sum)
bProp = bSum$SorB/bLength$count

#Get rid of blink/saccade trials
c = b
c = c[c$SorB == FALSE,]
cAgg = aggregate(target_response_rt ~ cued + cue_modality + target_modality + id , data = c, FUN = mean)

kleinTable = data.frame(matrix(cAgg$target_response_rt, nrow = 5, byrow = TRUE))

kleinTable$YUP[1:5] = bProp
names(kleinTable) = c("uncued TT", "cued TT", "uncued VT", "cued VT", "uncued TV", "cued TV", "uncued VV", "cued VV", "blink or saccade trial proportion")

#Get Feedback
bfed=afed
bfed$cued = FALSE
bfed$cued[bfed$target_location == "right" & bfed$cue_location == "right" | bfed$target_location == "left" & bfed$cue_location == "left"] = TRUE

bfed = bfed[bfed$block != "practice",]

bfed$SorB = FALSE
bfed$SorB[bfed$blink == TRUE | bfed$saccade == TRUE] = TRUE 

bfed$count = TRUE 

bfedSum = aggregate(SorB ~ id, data = bfed, FUN = sum)
bfedLength = aggregate(count ~ id, data = bfed, FUN = sum)
bfedProp = bfedSum$SorB/bfedLength$count

#Get rid of blink/saccade trials
cfed = bfed
cfed = cfed[cfed$SorB == FALSE,]
cfedAgg = aggregate(target_response_rt ~ cued + cue_modality + target_modality + id , data = cfed, FUN = mean)

cfedKlein = data.frame(matrix(cfedAgg$target_response_rt, nrow = 1, byrow = TRUE))

cfedKlein$YUP[1] = bfedProp

#Add feedback to kleinTable
names(cfedKlein) = c("uncued TT", "cued TT", "uncued VT", "cued VT", "uncued TV", "cued TV", "uncued VV", "cued VV", "blink or saccade trial proportion")
kleinTable = rbind(kleinTable, cfedKlein)

# add p07, onwards
bb=aa
bb$cued = FALSE
bb$cued[bb$target_location == "right" & bb$cue_location == "right" | bb$target_location == "left" & bb$cue_location == "left"] = TRUE

bb = bb[bb$block != "practice",]

bb$SorB = FALSE
bb$SorB[bb$blink == TRUE | bb$saccade == TRUE] = TRUE 

bb$count = TRUE 

bbSum = aggregate(SorB ~ id, data = bb, FUN = sum)
bbLength = aggregate(count ~ id, data = bb, FUN = sum)
bbProp = bbSum$SorB/bbLength$count

#Get rid of blink/saccade trials
cc = bb
cc = cc[cc$SorB == FALSE,]
ccAgg = aggregate(target_response_rt ~ cued + cue_modality + target_modality + id , data = cc, FUN = mean)

ccKlein = data.frame(matrix(ccAgg$target_response_rt, nrow = 4, byrow = TRUE))

ccKlein$YUP[1:4] = bbProp

# add to kleintable
names(ccKlein) = c("uncued TT", "cued TT", "uncued VT", "cued VT", "uncued TV", "cued TV", "uncued VV", "cued VV", "blink or saccade trial proportion")
kleinTable = rbind(kleinTable, ccKlein)

# eeg pilots
bbb=aaa
bbb$cued = FALSE
bbb$cued[bbb$target_location == "right" & bbb$cue_location == "right" | bbb$target_location == "left" & bbb$cue_location == "left"] = TRUE

bbb = bbb[bbb$block != "practice",]

bbb$SorB = FALSE
bbb$SorB[bbb$blink == TRUE | bbb$saccade == TRUE] = TRUE 

bbb$count = TRUE 

bbbSum = aggregate(SorB ~ id, data = bbb, FUN = sum) #[bbb$block > 2,], FUN = sum)
bbbLength = aggregate(count ~ id, data = bbb, FUN = sum)
bbbProp = bbbSum$SorB/bbbLength$count

bbb$Critical = FALSE
bbb$Critical[bbb$critical_blink == TRUE | bbb$critical_saccade == TRUE] = TRUE 

# their CRITICAL INTERVALs
bbbCritSum = aggregate(Critical ~ id, data = bbb, FUN = sum)
bbbCritProp = bbbCritSum$Critical/bbbLength$count

# their error rates
bbbBadSum = aggregate(pre_target_response ~ id, data = bbb, FUN = sum)
bbbBadProp = bbbBadSum$pre_target_response/bbbLength$count

#Get rid of blink/saccade trials
ccc = bbb
ccc = ccc[ccc$SorB == FALSE,]
cccAgg = aggregate(target_response_rt ~ cued + cue_modality + target_modality + id , data = ccc, FUN = mean)

cccKlein = data.frame(matrix(cccAgg$target_response_rt, nrow = length(files), byrow = TRUE))

cccKlein$YUP[1:length(files)] = bbbProp

# add to kleintable
names(cccKlein) = c("uncued TT", "cued TT", "uncued VT", "cued VT", "uncued TV", "cued TV", "uncued VV", "cued VV", "blink or saccade trial proportion")
kleinTable = rbind(kleinTable, cccKlein)

rNames = NULL 
for (i in 1:12) {
temp = sprintf("p0%i", i)
rNames = cbind(rNames, temp)
}
row.names(kleinTable) = rNames





