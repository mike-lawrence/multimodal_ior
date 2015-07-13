import os 
os.chdir("C:\Users\ghislaindentremont\Documents\R\MultiIOR\VMRK")
file = "multimodal_ior_forAlex.vmrk"
dataFile = open(file,'r')
with_binary = open("with_binary","w")
with_labels = open("with_labels", "w")
#		markers = []
done = False
dataStarted = False
while not done:
	line = dataFile.readline()
	if not dataStarted:
		if line[0:2] == "Mk":
			dataStarted = True
	if dataStarted:
		if line == '':
			done = True
		elif line[0:2] == "Mk":
			number_string = line.replace(' ','').split(',')[1].strip().strip('S')
#					print number_string
			if number_string == '':
				with_binary.write(line) 
#						markers.append(number_string)	
			else:
				number_interger = int(number_string)
#						print number_interger
				number_binary = bin(number_interger)[2:] 
				if len(number_binary) < 8:
					number_binary = number_binary.zfill(8)
#						print number_binary
#						markers.append(number_binary)
				line = line.split(',')
				line[1] = number_binary[::-1]
				# grab for label 
				line2 = line
				line = ",".join(line)
				print line
				with_binary.write(line)
				# and now I wanna label binary
				for_label = line2[1]
				labels = []
				if for_label[1] == "0":
					labels.append("left cue")
				else:
					labels.append("right cue")
				if for_label[2] == "0":
					labels.append("visual cue")
				else:
					labels.append("auditory cue")
				if for_label[3] == "0":
					labels.append("left target")
				else:
					labels.append("right target")
				if for_label[4] == "0":
					labels.append("visual target")
				else:
					labels.append("auditory target")
				if for_label[5] == "0":
					labels.append("catch")
				else:
					labels.append("target")
				if for_label[6] == "0":
					pass
				else:
					labels.append("trial start")
#						labels = ",".join(labels)
#						labels = labels.strip(",")
				labels = ",".join(labels)
				line2[1] = labels 
				# HERE to change what is in between each label
				line2 = ",".join(line2)
				with_labels.write(line2)


with_labels.close()
with_binary.close()				
dataFile.close()
