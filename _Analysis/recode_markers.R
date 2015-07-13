setwd("~/R/MultiIOR/MIKE_VMRK")

#get the header info
b = scan(
	file = 'multimodal_ior_forAlex.vmrk'
	, nmax = 10
	, what = 'character'
	, sep = '\n'
)

write(
	x = b
	, file = 'multimodal_ior_forAlex_binary.vmrk'
	, append = T
	, sep = '\n'
)
write(
	x = b
	, file = 'multimodal_ior_forAlex_labels.vmrk'
	, append = T
	, sep = '\n'
)

a = read.table(
	file = 'multimodal_ior_forAlex.vmrk'
	, sep = ','
	, header = FALSE
	, skip = 12
)
a$V2 = gsub(x=a$V2,pattern=' ',replacement='')
a$V2 = gsub(x=a$V2,pattern='S',replacement='')
a$V2 = as.numeric(a$V2)


int_to_bits = function(x){
	paste(substr(intToBits(x)[1:8],2,2),collapse='')
}

a$V2 = sapply(X=a$V2,FUN=int_to_bits)

write.table(
	x = a[,1:5]
	, file = 'multimodal_ior_forAlex_binary.vmrk'
	, append = T
	, sep = ','
	, row.names = F
	, col.names = F
	, quote = FALSE
)



a$cue_location = ifelse(
	substr(a$V2,2,2)==1
	, 'left'
	, 'right'
)
a$cue_modality = ifelse(
	substr(a$V2,3,3)==1
	, 'auditory'
	, 'visual'
)
a$target_location = ifelse(
	substr(a$V2,4,4)==1
	, 'left'
	, 'right'
)
a$target_modality = ifelse(
	substr(a$V2,5,5)==1
	, 'auditory'
	, 'visual'
)
a$target_type = ifelse(
	substr(a$V2,6,6)==1
	, 'catch'
	, 'target'
)
a$trial_start_marker = ifelse(
	(substr(a$V2,7,7)==0)&(substr(a$V2,7,7)==0)
	, 'trial_start'
	, ''
)
a$target_on_marker = ifelse(
	substr(a$V2,7,7)==1
	, 'target_on'
	, ''
)
a$target_off_marker = ifelse(
	substr(a$V2,8,8)==1
	, 'target_off'
	, ''
)

a$V2 = with(
	a
	, paste(cue_location,cue_modality,target_location,target_modality,target_type,trial_start_marker,target_on_marker,target_off_marker)
)

write.table(
	x = a[,1:5]
	, file = 'multimodal_ior_forAlex_labels.vmrk'
	, append = T
	, sep = ','
	, row.names = F
	, col.names = F
	, quote = FALSE
)
