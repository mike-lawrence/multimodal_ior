import os 
os.chdir("C:\Users\ghislaindentremont\Documents\R\MultiIOR\Data\Shared Directory\p6")
for file in os.listdir('.'):
	if file.endswith(".asc"):
		dataFile = open(file,'r')
		lastStart=0
		samples = []
		messages = []
		trials = []
		blinks = []
		saccades = []
		done = False
		dataStarted = False
		while not done:
			line = dataFile.readline()
			if not dataStarted:
				if line[0].isdigit():
					dataStarted = True
			if dataStarted:
				if line == '':
					done = True
				elif line[0].isdigit():
					samples.append(line.replace(' ','').split('\t')[0:4])
				elif line[0:3]=='MSG':
					temp = line[4:-1]
					temp = temp.split(' ')
					if len(temp)==2:
						time = temp[0]
						temp = temp[1].split('\t')
						temp.insert(0,time)
						trials.append(temp)
					else:
						messages.append([temp[0],' '.join(temp[1:])])
				elif line[0:5]=='ESACC':
					saccades.append(line.split()[1:])
				elif line[0:6]=='EBLINK':
					blinks.append(line.split()[1:])
				elif line[0:4] in ['SFIX','EFIX','SSAC','SBLI']:
					pass
				else:
					print line #unaccounted-for line
		
		dataFile.close()
		
		samplesFile = open('samples.txt','w')
		samplesFile.write('\n'.join(['\t'.join(thisSample) for thisSample in samples]))
		samplesFile.close()
	
		messagesFile = open('messages.txt','w')
		messagesFile.write('\n'.join(['\t'.join(thisMessage) for thisMessage in messages]))
		messagesFile.close()
	
		trialsFile = open('trials.txt','w')
		trialsFile.write('\n'.join(['\t'.join(thisTrial) for thisTrial in trials]))
		trialsFile.close()
	
		blinksFile = open('blinks.txt','w')
		blinksFile.write('\n'.join(['\t'.join(thisBlink) for thisBlink in blinks]))
		blinksFile.close()
		
		saccadesFile = open('saccades.txt','w')
		saccadesFile.write('\n'.join(['\t'.join(thisSaccade) for thisSaccade in saccades]))
		saccadesFile.close()
		
