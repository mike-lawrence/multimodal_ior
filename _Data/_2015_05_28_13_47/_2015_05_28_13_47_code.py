if __name__ == '__main__':
	########
	#Important parameters
	########

	trigger_led_num = 8
	left_led_num = 9
	right_led_num = 11
	left_tact_num = 13
	right_tact_num = 15

	viewing_distance = 60.0 #units can be anything so long as they match those used in stim_display_width below
	stim_display_width = 54.5 #units can be anything so long as they match those used in viewing_distance above
	stim_display_res = (1920,1080) #pixel resolution of the stim_display
	stim_display_position = (-1440-1920,1680-1080)

	writer_window_size = (200,200)
	writer_window_position = (-1440,0)

	voicekey_window_size = (200,200)
	voicekey_window_position = (-1440+300,0)

	stamper_window_size = (200,200)
	stamper_window_position = (-1440+600,0)
	stamper_window_color = [255,255,255]
	stamper_do_border = True

	do_eyelink = True
	eyelink_window_size = (200,200)
	eyelink_window_position = (-1440+900,0)
	eyelink_ip = '100.1.1.1'
	edf_file_name = 'temp.edf'
	edf_path = './'
	saccade_sound_file = '_Stimuli/stop.wav'
	blink_sound_file = '_Stimuli/stop.wav'
	calibration_dot_size_in_degrees = .5
	gaze_target_criterion_in_degrees = 2

 	
	cue_modality_list = ['visual','auditory']
	cue_location_list = ['left','right']
	target_location_list = ['left','right']
	num_targets_per_catch = 9

	fixation_duration = 0.500
	cue_target_oa = 1.000
	cue_duration = 0.100
	response_timeout = 1.000
	feedback_duration = 1.000

	cue_stim_frequency = 100 #Hz

	#10*2*2*2 = 80 trials
	number_of_block = 10
	trials_for_practice = 40

	instruction_size_in_degrees = 1 #specify the size of the instruction text
	feedback_size_in_degrees = 1 #specify the size of the feedback text
	fixation_size_in_degrees = .5

	text_width = .9 #proportion of the stim_display to use when drawing instructions

	########
	# Import libraries
	########
	import u3 #for labjack
	import sdl2 #for input and display
	import sdl2.sdlmixer
	import numpy #for image and display manipulation
	# from PIL import Image #for image manipulation
	from PIL import ImageFont #for fonts
	# from PIL import ImageOps
	#import aggdraw #for drawing
	import math #for rounding
	import sys #for quitting
	import os #for os.nice and checking if folders/files exist
	import random #for shuffling and random sampling
	import time #for timing
	import shutil #for copying files
	import OpenGL.GL as gl
	try:
		os.nice(-20)
	except:
		pass#print('Can\'t nice')
	try:
		import appnope
		appnope.nope()
	except:
		pass
	import fileForker
	byteify = lambda x, enc: x.encode(enc)

	########
	# Initialize the labjack
	########
	labjack = u3.U3()
	labjack.configU3()

	########
	# Initialize audio and define a class for playing sounds
	########
	sdl2.SDL_Init(sdl2.SDL_INIT_AUDIO)
	sdl2.sdlmixer.Mix_OpenAudio(44100, sdl2.sdlmixer.MIX_DEFAULT_FORMAT, 2, 1024)
	class Sound:
		def __init__(self, fileName):
			self.sample = sdl2.sdlmixer.Mix_LoadWAV(byteify(fileName, "utf-8"))
			self.started = False
		def play(self):
			self.channel = sdl2.sdlmixer.Mix_PlayChannel(-1, self.sample, 0)
			self.started = True
		def stillPlaying(self):
			if self.started:
				if sdl2.sdlmixer.Mix_Playing(self.channel):
					return True
				else:
					self.started = False
					return False
			else:
				return False


	########
	# Define a custom time function using the same clock as that which generates the SDL2 event timestamps
	########

	#define a function that gets the time (unit=seconds,zero=?)
	def get_time():
		return sdl2.SDL_GetPerformanceCounter()*1.0/sdl2.SDL_GetPerformanceFrequency()


	########
	# Initialize the timer and random seed
	########
	sdl2.SDL_Init(sdl2.SDL_INIT_TIMER)
	seed = get_time() #grab the time of the timer initialization to use as a seed
	random.seed(seed) #use the time to set the random seed


	########
	#Perform some calculations to convert stimulus measurements in degrees to pixels
	########
	stim_display_width_in_degrees = math.degrees(math.atan((stim_display_width/2.0)/viewing_distance)*2)
	PPD = stim_display_res[0]/stim_display_width_in_degrees #compute the pixels per degree (PPD)

	calibration_dot_size = calibration_dot_size_in_degrees*PPD
	instruction_size = instruction_size_in_degrees*PPD
	feedback_size = feedback_size_in_degrees*PPD
	fixation_size = fixation_size_in_degrees*PPD
	gaze_target_criterion = gaze_target_criterion_in_degrees*PPD

	########
	# Initialize fonts
	########
	feedback_font_size = 2
	feedback_font = ImageFont.truetype ("_Stimuli/DejaVuSans.ttf", feedback_font_size)
	feedback_height = feedback_font.getsize('XXX')[1]
	while feedback_height<feedback_size:
		feedback_font_size = feedback_font_size + 1
		feedback_font = ImageFont.truetype ("_Stimuli/DejaVuSans.ttf", feedback_font_size)
		feedback_height = feedback_font.getsize('XXX')[1]

	feedback_font_size = feedback_font_size - 1
	feedback_font = ImageFont.truetype ("_Stimuli/DejaVuSans.ttf", feedback_font_size)
	feedback_height = feedback_font.getsize('XXX')[1]

	instruction_font_size = 2
	instruction_font = ImageFont.truetype ("_Stimuli/DejaVuSans.ttf", instruction_font_size)
	instruction_height = instruction_font.getsize('XXX')[1]
	while instruction_height<instruction_size:
		instruction_font_size = instruction_font_size + 1
		instruction_font = ImageFont.truetype ("_Stimuli/DejaVuSans.ttf", instruction_font_size)
		instruction_height = instruction_font.getsize('XXX')[1]

	instruction_font_size = instruction_font_size - 1
	instruction_font = ImageFont.truetype ("_Stimuli/DejaVuSans.ttf", instruction_font_size)
	instruction_height = instruction_font.getsize('XXX')[1]

	########
	# initialize the eyelink
	########
	if do_eyelink:
		eyelink_child = fileForker.childClass(childFile='eyelink_child.py')
		eyelink_child.initDict['window_size'] = eyelink_window_size
		eyelink_child.initDict['window_position'] = eyelink_window_position
		eyelink_child.initDict['stim_display_position'] = stim_display_position
		eyelink_child.initDict['stim_display_res'] = stim_display_res
		eyelink_child.initDict['calibration_display_size'] = stim_display_res#[int(targetOffset*2),int(targetOffset)]
		eyelink_child.initDict['calibration_dot_size'] = int(calibration_dot_size)
		eyelink_child.initDict['eyelink_ip'] = eyelink_ip
		eyelink_child.initDict['edf_file_name'] = edf_file_name
		eyelink_child.initDict['edf_path'] = edf_path
		eyelink_child.initDict['saccade_sound_file'] = saccade_sound_file
		eyelink_child.initDict['blink_sound_file'] = blink_sound_file
		eyelink_child.start()


	########
	# Initialize the writer
	########
	writer_child = fileForker.childClass(childFile='writer_child.py')
	writer_child.initDict['window_size'] = writer_window_size
	writer_child.initDict['window_position'] = writer_window_position
	time.sleep(1) #give the other windows some time to initialize
	writer_child.start()

	########
	# Initialize the voicekey
	########
	voicekey_child = fileForker.childClass(childFile='voicekey_child.py')
	voicekey_child.initDict['window_size'] = voicekey_window_size
	voicekey_child.initDict['window_position'] = voicekey_window_position
	time.sleep(1) #give the other windows some time to initialize
	voicekey_child.start()


	########
	# Initialize the stim_display_mirror_child
	########
	stim_display_mirror_child = fileForker.childClass(childFile='stim_display_mirror_child.py')
	# stim_display_mirror_child.initDict['window_size'] = stim_display_res
	stim_display_mirror_child.initDict['window_position'] = [0,0]
	time.sleep(1) #give the other windows some time to initialize
	stim_display_mirror_child.start()

	########
	# Initialize the stim_display
	########
	class stim_display_class:
		def __init__(self,stim_display_res,stim_display_position,stim_display_mirror_child):
			self.stim_display_res = stim_display_res
			self.stim_display_mirror_child = stim_display_mirror_child
			sdl2.SDL_Init(sdl2.SDL_INIT_VIDEO)
			self.stim_display_res = stim_display_res
			self.stim_display_position = stim_display_position
			self.Window = sdl2.video.SDL_CreateWindow(byteify('stim_display', "utf-8"),self.stim_display_position[0],self.stim_display_position[1],self.stim_display_res[0],self.stim_display_res[1],sdl2.SDL_WINDOW_OPENGL | sdl2.SDL_WINDOW_SHOWN | sdl2.SDL_WINDOW_FULLSCREEN_DESKTOP | sdl2.SDL_RENDERER_ACCELERATED | sdl2.SDL_RENDERER_PRESENTVSYNC)
			self.glContext = sdl2.SDL_GL_CreateContext(self.Window)
			gl.glMatrixMode(gl.GL_PROJECTION)
			gl.glLoadIdentity()
			gl.glOrtho(0, stim_display_res[0],stim_display_res[1], 0, 0, 1)
			gl.glMatrixMode(gl.GL_MODELVIEW)
			gl.glDisable(gl.GL_DEPTH_TEST)
			gl.glReadBuffer(gl.GL_FRONT)
			start = time.time()
			while time.time()<(start+2):
				sdl2.SDL_PumpEvents()
			self.refresh()
			self.refresh()
		def refresh(self,clear_color=[0,0,0,1]):
			sdl2.SDL_GL_SwapWindow(self.Window)
			self.stim_display_mirror_child.qTo.put(['frame',self.stim_display_res,gl.glReadPixels(0, 0, self.stim_display_res[0], self.stim_display_res[1], gl.GL_BGR, gl.GL_UNSIGNED_BYTE)])
			gl.glClearColor(clear_color[0],clear_color[1],clear_color[2],clear_color[3])
			gl.glClear(gl.GL_COLOR_BUFFER_BIT)


	time.sleep(1)
	stim_display = stim_display_class(stim_display_res=stim_display_res,stim_display_position=stim_display_position,stim_display_mirror_child=stim_display_mirror_child)


	########
	# start the event timestamper
	########
	stamper_child = fileForker.childClass(childFile='stamper_child.py')
	stamper_child.initDict['window_size'] = stamper_window_size
	stamper_child.initDict['window_position'] = stamper_window_position
	stamper_child.initDict['window_color'] = stamper_window_color
	stamper_child.initDict['do_border'] = stamper_do_border
	stamper_child.start()


	########
	# Drawing functions
	########

	def text2numpy(my_text,myFont,fg=[255,255,255,255],bg=[0,0,0,0]):
		glyph = myFont.getmask(my_text,mode='L')
		a = numpy.asarray(glyph)#,dtype=numpy.uint8)
		b = numpy.reshape(a,(glyph.size[1],glyph.size[0]),order='C')
		c = numpy.zeros((glyph.size[1],glyph.size[0],4))#,dtype=numpy.uint8)
		# c[:,:,0][b>0] = b[b>0]
		# c[:,:,1][b>0] = b[b>0]
		# c[:,:,2][b>0] = b[b>0]
		# c[:,:,3][b>0] = b[b>0]
		c[:,:,0][b>0] = fg[0]*b[b>0]/255.0
		c[:,:,1][b>0] = fg[1]*b[b>0]/255.0
		c[:,:,2][b>0] = fg[2]*b[b>0]/255.0
		c[:,:,3][b>0] = fg[3]*b[b>0]/255.0
		c[:,:,0][b==0] = bg[0]
		c[:,:,1][b==0] = bg[1]
		c[:,:,2][b==0] = bg[2]
		c[:,:,3][b==0] = bg[3]
		return c.astype(dtype=numpy.uint8)


	def blitNumpy(numpyArray,xLoc,yLoc,xCentered=True,yCentered=True):
		gl.glEnable(gl.GL_BLEND)
		gl.glBlendFunc(gl.GL_SRC_ALPHA, gl.GL_ONE_MINUS_SRC_ALPHA)
		ID = gl.glGenTextures(1)
		gl.glBindTexture(gl.GL_TEXTURE_2D, ID)
		gl.glTexEnvi(gl.GL_TEXTURE_ENV, gl.GL_TEXTURE_ENV_MODE, gl.GL_REPLACE);
		gl.glTexParameterf(gl.GL_TEXTURE_2D, gl.GL_TEXTURE_WRAP_S, gl.GL_CLAMP)
		gl.glTexParameterf(gl.GL_TEXTURE_2D, gl.GL_TEXTURE_WRAP_T, gl.GL_CLAMP)
		gl.glTexParameterf(gl.GL_TEXTURE_2D, gl.GL_TEXTURE_MAG_FILTER, gl.GL_LINEAR)
		gl.glTexParameterf(gl.GL_TEXTURE_2D, gl.GL_TEXTURE_MIN_FILTER, gl.GL_LINEAR)
		gl.glTexImage2D( gl.GL_TEXTURE_2D , 0 , gl.GL_RGBA , numpyArray.shape[1] , numpyArray.shape[0] , 0 , gl.GL_RGBA , gl.GL_UNSIGNED_BYTE , numpyArray )
		gl.glEnable(gl.GL_TEXTURE_2D)
		gl.glBindTexture(gl.GL_TEXTURE_2D, ID)
		gl.glBegin(gl.GL_QUADS)
		x1 = xLoc + 1.5 - 0.5
		x2 = xLoc + numpyArray.shape[1] - 0.0 + 0.5
		y1 = yLoc + 1.0 - 0.5
		y2 = yLoc + numpyArray.shape[0] - 0.5 + 0.5
		if xCentered:
			x1 = x1 - numpyArray.shape[1]/2.0
			x2 = x2 - numpyArray.shape[1]/2.0
		if yCentered:
			y1 = y1 - numpyArray.shape[0]/2.0
			y2 = y2 - numpyArray.shape[0]/2.0
		gl.glTexCoord2f( 0 , 0 )
		gl.glVertex2f( x1 , y1 )
		gl.glTexCoord2f( 1 , 0 )
		gl.glVertex2f( x2 , y1 )
		gl.glTexCoord2f( 1 , 1)
		gl.glVertex2f( x2 , y2 )
		gl.glTexCoord2f( 0 , 1 )
		gl.glVertex2f( x1, y2 )
		gl.glEnd()
		gl.glBindTexture(gl.GL_TEXTURE_2D, 0)
		gl.glDeleteTextures([ID])
		del ID
		gl.glDisable(gl.GL_TEXTURE_2D)
		return None


	def drawText( my_text , myFont , text_width , xLoc , yLoc , fg=[255,255,255,255] , bg=[0,0,0,0] , xCentered=True , yCentered=True , lineSpacing=1.2):
		lineHeight = myFont.getsize('Tj')[0]*lineSpacing
		paragraphs = my_text.splitlines()
		renderList = []
		for thisParagraph in paragraphs:
			words = thisParagraph.split(' ')
			if len(words)==1:
				renderList.append(words[0])
				if (thisParagraph!=paragraphs[len(paragraphs)-1]):
					renderList.append(' ')
			else:
				thisWordIndex = 0
				while thisWordIndex < (len(words)-1):
					lineStart = thisWordIndex
					lineWidth = 0
					while (thisWordIndex < (len(words)-1)) and (lineWidth <= text_width):
						thisWordIndex = thisWordIndex + 1
						lineWidth = myFont.getsize(' '.join(words[lineStart:(thisWordIndex+1)]))[0]
					if thisWordIndex < (len(words)-1):
						#last word went over, paragraph continues
						renderList.append(' '.join(words[lineStart:(thisWordIndex-1)]))
						thisWordIndex = thisWordIndex-1
					else:
						if lineWidth <= text_width:
							#short final line
							renderList.append(' '.join(words[lineStart:(thisWordIndex+1)]))
						else:
							#full line then 1 word final line
							renderList.append(' '.join(words[lineStart:thisWordIndex]))
							renderList.append(words[thisWordIndex])
						#at end of paragraph, check whether a inter-paragraph space should be added
						if (thisParagraph!=paragraphs[len(paragraphs)-1]):
							renderList.append(' ')
		numLines = len(renderList)
		for thisLineNum in range(numLines):
			if renderList[thisLineNum]==' ':
				pass
			else:
				thisRender = text2numpy( renderList[thisLineNum] , myFont , fg=fg , bg=bg )
				if xCentered:
					x = xLoc - thisRender.shape[1]/2.0
				else:
					x = xLoc
				if yCentered:
					y = yLoc - numLines*lineHeight/2.0 + thisLineNum*lineHeight
				else:
					y = yLoc + thisLineNum*lineHeight
				blitNumpy(numpyArray=thisRender,xLoc=x,yLoc=y,xCentered=False,yCentered=False)
		return None


	def draw_feedback(feedback_text,feedback_color):
		if type(feedback_text)==type('a'):
			feedbackArray = text2numpy(feedback_text,feedback_font,fg=feedback_color,bg=[0,0,0,0])
			blitNumpy(feedbackArray,stim_display_res[0]/2,stim_display_res[1]/2,xCentered=True,yCentered=True)


	def draw_dot(size,x_offset=0):
		gl.glColor3f(.5,.5,.5)
		gl.glBegin(gl.GL_POLYGON)
		for i in range(360):
			gl.glVertex2f( stim_display_res[0]/2+x_offset + math.sin(i*math.pi/180)*(size/2.0) , stim_display_res[1]/2 + math.cos(i*math.pi/180)*(size/2.0))
		gl.glEnd()


	def draw_cal_target(x=stim_display_res[0]/2,y=stim_display_res[1]/2):
		gl.glColor3f(.5,.5,.5)
		gl.glBegin(gl.GL_POLYGON)
		for i in range(360):
			gl.glVertex2f( x + math.sin(i*math.pi/180.0)*(calibration_dot_size/2.0) , y + math.cos(i*math.pi/180.0)*(calibration_dot_size/2.0))
		gl.glEnd()
		gl.glColor3f(0,0,0)
		gl.glBegin(gl.GL_POLYGON)
		for i in range(360):
			gl.glVertex2f( x + math.sin(i*math.pi/180.0)*(calibration_dot_size/8.0) , y + math.cos(i*math.pi/180.0)*(calibration_dot_size/8.0))
		gl.glEnd()


	########
	# Helper functions
	########

	#define a function that simply waits untile a specified end time
	def wait_until(wait_end_time):
		while get_time() < wait_end_time:
			pass

	#define a function that waits for a given duration to pass
	def simple_wait(duration):
		start = get_time()
		while get_time() < (start + duration):
			pass


	#define a function that will kill everything safely
	def exit_safely():
		try:
			# labjack.getFeedback(u3.BitStateWrite(trigger_led_num,0))
			labjack.getFeedback(u3.BitStateWrite(left_led_num,0))
			labjack.getFeedback(u3.BitStateWrite(right_led_num,0))
			labjack.getFeedback(u3.BitStateWrite(left_tact_num,0))
			labjack.getFeedback(u3.BitStateWrite(right_tact_num,0))
			labjack.close()
		except:
			print 'Failed to stop labjack'
		try:
			voicekey_child.stop()
		except:
			print 'Failed to stop voicekey_child'
		try:
			stim_display_mirror_child.stop()
		except:
			print 'Failed to stop stim_display_mirror_child'
		try:
			writer_child.stop()
		except:
			print 'Failed to stop writer_child'
		try:
			stamper_child.stop(killAfter=60)
			while stamper_child.isAlive():
				time.sleep(.1)
		except:
			print 'Failed to stop stamper_child'
		if do_eyelink:
			try:
				eyelink_child.stop()
			except:
				print 'Failed to stop eyelink_child'
		sys.exit()


	#define a function that waits for a response
	def wait_for_response():
		done = False
		while not done:
			if not stamper_child.qFrom.empty():
				event = stamper_child.qFrom.get()
				if event['type']=='key':
					response = event['value']
					if response=='escape':
						exit_safely()
					else:
						done = True
		return response


	#define a function that prints a message on the stim_display while looking for user input to continue. The function returns the total time it waited
	def show_message(my_text):
		message_viewing_time_start = get_time()
		gl.glClearColor(0,0,0,1)
		gl.glClear(gl.GL_COLOR_BUFFER_BIT)
		stim_display.refresh()
		drawText( my_text , instruction_font , stim_display_res[0] , xLoc=stim_display_res[0]/2 , yLoc=stim_display_res[1]/2 , fg=[200,200,200,255] )
		simple_wait(0.500)
		stim_display.refresh()
		wait_for_response()
		stim_display.refresh()
		simple_wait(0.500)
		message_viewing_time = get_time() - message_viewing_time_start
		return message_viewing_time


	#define a function that requests user input
	def get_input(get_what):
		textInput = ''
		gl.glClearColor(0,0,0,1)
		gl.glClear(gl.GL_COLOR_BUFFER_BIT)
		stim_display.refresh()
		simple_wait(0.500)
		my_text = get_what+textInput
		drawText( my_text , instruction_font , stim_display_res[0] , xLoc=stim_display_res[0]/2 , yLoc=stim_display_res[1]/2 , fg=[200,200,200,255] )
		stim_display.refresh()
		done = False
		while not done:
			if not stamper_child.qFrom.empty():
				event = stamper_child.qFrom.get()
				if event['type'] == 'key' :
					response = event['value']
					if response=='escape':
						exit_safely()
					elif response == 'backspace':
						if textInput!='':
							textInput = textInput[0:(len(textInput)-1)]
							my_text = get_what+textInput
							drawText( my_text , instruction_font , stim_display_res[0] , xLoc=stim_display_res[0]/2 , yLoc=stim_display_res[1]/2 , fg=[200,200,200,255] )
							stim_display.refresh()
					elif response == 'return':
						done = True
					else:
						textInput = textInput + response
						my_text = get_what+textInput
						drawText( my_text , instruction_font , stim_display_res[0] , xLoc=stim_display_res[0]/2 , yLoc=stim_display_res[1]/2 , fg=[200,200,200,255] )
						stim_display.refresh()
		stim_display.refresh()
		return textInput


	#define a function that obtains subject info via user input
	def get_sub_info():
		year = time.strftime('%Y')
		month = time.strftime('%m')
		day = time.strftime('%d')
		hour = time.strftime('%H')
		minute = time.strftime('%M')
		sid = get_input('ID (\'test\' to demo): ')
		if sid != 'test':
			sex = get_input('Sex (m or f): ')
			age = get_input('Age (2-digit number): ')
			handedness = get_input('Handedness (r or l): ')
		else:
			sex = 'test'
			age = 'test'
			handedness = 'test'
		sub_info = [ sid , year , month , day , hour , minute , sex , age , handedness ]
		return sub_info


	def get_trials():
		trials = []
		for i in range(num_targets_per_catch):
			for cue_modality in cue_modality_list:
				for cue_location in cue_location_list:
					for target_location in target_location_list:
						trials.append([cue_modality,cue_location,target_location])
		for cue_modality in cue_modality_list:
			for cue_location in cue_location_list:
				for target_location in target_location_list:
					trials.append([cue_modality,cue_location,'catch'])
		random.shuffle(trials)
		return trials

	def check_responses():
		responses = []
		while not stamper_child.qFrom.empty():
			event = stamper_child.qFrom.get()
			if event['type'] == 'key' :
				key = event['value']
				time = event['time']
				if key=='escape':
					exit_safely()
				responses.append([key,time])
		return responses


	def do_calibration():
		draw_dot(fixation_size)
		stim_display.refresh()
		eyelink_child.qTo.put('do_calibration')
		calibration_done = False
		while not calibration_done:
			if not stamper_child.qFrom.empty():
				event = stamper_child.qFrom.get()
				if event['type'] == 'key' :
					key = event['value']
					if key=='escape':
						exit_safely()
					else: #pass keys to eyelink
						eyelink_child.qTo.put(['keycode',event['keysym']])
			if not eyelink_child.qFrom.empty():
				message = eyelink_child.qFrom.get()
				if message=='calibration_complete':
					calibration_done = True
				elif (message=='setup_cal_display') or (message=='exit_cal_display'):
					draw_dot(fixation_size)
					stim_display.refresh()
				elif message=='erase_cal_target':
					pass
				elif message=='clear_cal_display':
					stim_display.refresh()
				elif message[0]=='draw_cal_target':
					x = message[1]
					y = message[2]
					draw_cal_target(x,y)
					stim_display.refresh()
				elif message[0]=='image':
					blitNumpy(message[1],stim_display_res[0]/2,stim_display_res[1]/2,xCentered=True,yCentered=True)
					stim_display.refresh()




	#define a function that runs a block of trials
	def run_block(block,message_viewing_time):

		if not do_eyelink:
			#clear any residual between-block responses (not necessary if doing eyelink)
			start = get_time()
			while (get_time()-start)<1:
				check_responses()
		
		#get a trial list
		if block=='practice':
			trial_list = get_trials()[0:trials_for_practice]
		else:
			trial_list = get_trials()
		
		#run the trials
		trial_num = 0
		while len(trial_list)>0:
			#bump the trial number
			trial_num = trial_num + 1
			print 'Block: '+str(block)+'; Trial: '+str(trial_num)
			#parse the trial info
			cue_modality , cue_location , target_location = trial_list.pop()
			
			trial_descrptor = '\t'.join(map(str,[sub_info[0],block,trial_num]))

			#pre-compute labjack output pins
			if cue_modality == 'visual':
				if cue_location=='left':
					cue_labjack_num = left_led_num
				else:
					cue_labjack_num = right_led_num
			else:
				if cue_location=='left':
					cue_labjack_num = left_tact_num
				else:
					cue_labjack_num = right_tact_num
			if target_location=='left':
				target_labjack_num = left_led_num
			elif target_location=='right':
				target_labjack_num = right_led_num

			#make sure all the labjack outputs are off
			# labjack.getFeedback(u3.BitStateWrite(trigger_led_num,0))
			labjack.getFeedback(u3.BitStateWrite(left_led_num,0))
			labjack.getFeedback(u3.BitStateWrite(right_led_num,0))
			labjack.getFeedback(u3.BitStateWrite(left_tact_num,0))
			labjack.getFeedback(u3.BitStateWrite(right_tact_num,0))

			#tell the voicekey to report responses
			# voicekey_child.qTo.put(['report_responses',True])

			draw_cal_target()
			stim_display.refresh()
			start = get_time()
			if do_eyelink:
				outer_done = False
				while not outer_done:
					draw_cal_target()
					stim_display.refresh()
					outer_done = True #set to False below if need to re-do drift correct after re-calibration
					eyelink_child.qTo.put('do_drift_correct')
					inner_done = False
					while not inner_done:
						if not eyelink_child.qFrom.empty():
							message = eyelink_child.qFrom.get()
							if message=='drift_correct_complete':
								inner_done = True
							elif message=='do_calibration':
								do_calibration()
								inner_done = True
								outer_done = False
						while not stamper_child.qFrom.empty():
							event = stamper_child.qFrom.get()
							if event['type'] == 'key' :
								key = event['value']
								time = event['time']
								if key=='escape':
									exit_safely()
								elif key=='p': #recalibration requested
									do_calibration()
									inner_done = True
									outer_done = False
								else:
									eyelink_child.qTo.put(['keycode',event['keysym']])
									#print ['main','keycode',key]
				eyelink_child.qTo.put(['report_blinks',True])
				eyelink_child.qTo.put(['report_saccades',True])
				eyelink_child.qTo.put(['send_message','trial_start\t'+trial_descrptor])
			trial_initiation_time = get_time() - start

			while not voicekey_child.qFrom.empty():
				event = voicekey_child.qFrom.get()

			voicekey_child.qTo.put(['report_responses',True])

			#prep and show the fixation twice (ensures 2nd refresh will block; for better trial start time accuracy)
			for i in range(2):
				draw_dot(fixation_size)
				stim_display.refresh()

			#get the trial start time 
			trial_start_time = get_time() - 1/60.0 #time that the previous (first) refresh returned

			#compute event times
			cue_start_time = trial_start_time + fixation_duration
			cue_done_time = cue_start_time + cue_duration
			target_on_time = cue_start_time + cue_target_oa
			response_timeout_time = target_on_time + response_timeout

			#initialize some variables
			blink = 'FALSE'
			saccade = 'FALSE'
			target_response_key = 'NA'
			target_response_rt = 'NA'
			pre_target_response = 'FALSE'
			feedback_response = 'FALSE'
			recalibration = 'FALSE'

			cue_started = False
			cue_done = False
			cue_off = False
			target_on = False


			trial_done = False
			while not trial_done:
				#manage stimuli
				if not cue_started:
					if get_time()>=cue_start_time:
						labjack.getFeedback(u3.BitStateWrite(cue_labjack_num,1))
						cue_started = True
						last_cue_state = 1
				elif not cue_done:
					if get_time()>=cue_done_time:
						labjack.getFeedback(u3.BitStateWrite(cue_labjack_num,0))
						cue_done = True
					else: #cue is started butnot done
						time_since_start = get_time()-cue_start_time
						this_cue_state = int(time_since_start/(1.0/cue_stim_frequency))%2
						if this_cue_state!=last_cue_state: #only bother sending a change state request when a state *change* is necessary 
							labjack.getFeedback(u3.BitStateWrite(cue_labjack_num,this_cue_state))
							last_cue_state = this_cue_state
				elif not target_on:
					if get_time()>=target_on_time:
						# labjack.getFeedback(u3.BitStateWrite(trigger_led_num,1))
						if target_location!='catch':
							labjack.getFeedback(u3.BitStateWrite(target_labjack_num,1))
						target_on = True
				elif get_time()>=response_timeout_time:
					trial_done = True
					feedback_text = 'Miss!'
					feedback_color = [255,0,0,255]
					break
				#manage eyelink
				if do_eyelink:
					if not eyelink_child.qFrom.empty():
						message = eyelink_child.qFrom.get()
						if (message=='blink') or (message=='gaze_target_lost'):
							if message=='blink':
								blink = 'TRUE'
								feedback_text = 'Blinked!'
							elif message=='gaze_target_lost':
								saccade = 'TRUE'
								feedback_text = 'Eyes moved!'
							feedback_color = [255,0,0,255]
							trial_done = True
							trial_list.append([cue_modality , cue_location , target_location])
							random.shuffle(trial_list)
							break
				# #manage responses
				if not voicekey_child.qFrom.empty():
					# labjack.getFeedback(u3.BitStateWrite(trigger_led_num,1)) #send trigger to EEG
					event = voicekey_child.qFrom.get()
					target_response_key = 'voice'
					target_response_rt = (event[1] - target_on_time)*1000
					feedback_text = str(int(target_response_rt/10))
					feedback_color = [127,127,127,255]
					trial_done = True
					break
				if not stamper_child.qFrom.empty():
					event = stamper_child.qFrom.get()
					if event['type']=='key':
						key_name = event['value']
						key_time = event['time']
						if key_name=='escape':
							exit_safely()
						elif not target_on:
							pre_target_response = 'TRUE'
							feedback_text = 'Too soon!'
							feedback_color = [255,0,0,255]
						else:
							target_response_key = key_name
							target_response_rt = (key_time - target_on_time)*1000
							feedback_text = str(int(target_response_rt/10))
							feedback_color = [127,127,127,255]
						trial_done = True
						break
			#trial done.
			#tell the voicekey to stop reporting responses
			voicekey_child.qTo.put(['report_responses',False])
			#tell eyelink trial is done
			if do_eyelink:
				eyelink_child.qTo.put(['send_message','trialDone\t'+trial_descrptor])
				eyelink_child.qTo.put(['report_blinks',False])
				eyelink_child.qTo.put(['report_saccades',False])
			#make sure all labjack outputs are off
			# labjack.getFeedback(u3.BitStateWrite(trigger_led_num,0))
			labjack.getFeedback(u3.BitStateWrite(left_led_num,0))
			labjack.getFeedback(u3.BitStateWrite(right_led_num,0))
			labjack.getFeedback(u3.BitStateWrite(left_tact_num,0))
			labjack.getFeedback(u3.BitStateWrite(right_tact_num,0))
			#show feedback
			gl.glClear(gl.GL_COLOR_BUFFER_BIT)
			draw_feedback(feedback_text,feedback_color)
			stim_display.refresh()
			feedback_done = False
			feedback_done_time = get_time() + feedback_duration
			while not feedback_done:
				if get_time()>feedback_done_time:
					feedback_done = True
				else:
					while not stamper_child.qFrom.empty():
						event = stamper_child.qFrom.get()
						if event['type'] == 'key' :
							key_name = event['value']
							if key_name=='escape':
								exit_safely()
							elif key_name=='p' and do_eyelink:
								feedback_done = True
								recalibration = 'TRUE'
								do_calibration()
							else: #haven't done a recalibration
								feedback_response = 'TRUE'
								#update feedback
								draw_feedback("Don't respond during feedback!",[255,0,0,255])
								stim_display.refresh()
								feedback_done_time = get_time() + feedback_duration
			#write out trial info
			data_to_write = '\t'.join(map(str,[ sub_info_for_file , message_viewing_time , block , trial_num , trial_initiation_time , cue_modality , cue_location , target_location , target_response_key , target_response_rt , feedback_response , recalibration , blink , saccade]))
			writer_child.qTo.put(['write','data',data_to_write])
		print 'on break'



	########
	# Initialize the data files
	########

	if do_eyelink:
		do_calibration()

	#get subject info
	sub_info = get_sub_info()

	if not os.path.exists('_Data'):
		os.mkdir('_Data')
	if sub_info[0]=='test':
		filebase = 'test'
	else:
		filebase = '_'.join(sub_info[0:6])
	if not os.path.exists('_Data/'+filebase):
		os.mkdir('_Data/'+filebase)

	shutil.copy(sys.argv[0], '_Data/'+filebase+'/'+filebase+'_code.py')

	if do_eyelink:
		eyelink_child.qTo.put(['edf_path','_Data/'+filebase+'/'+filebase+'_eyelink.edf'])

	writer_child.qTo.put(['new_file','data','_Data/'+filebase+'/'+filebase+'_data.txt'])
	header ='\t'.join(['id' , 'year' , 'month' , 'day' , 'hour' , 'minute' , 'sex' , 'age'  , 'handedness' , 'message_viewing_time' , 'block' , 'trial_num' , 'trial_initiation_time' , 'cue_modality' , 'cue_location' , 'target_location' , 'target_response_key' , 'target_response_rt' , 'feedback_response' , 'recalibration' , 'blink' , 'saccade' ])
	writer_child.qTo.put(['write','data',header])


	sub_info_for_file = '\t'.join(map(str,sub_info))


	########
	# Start the experiment
	########

	message_viewing_time = show_message('To begin practice, press any key.')
	block = 'practice'
	run_block('practice',message_viewing_time)
	message_viewing_time = show_message('To begin the experiment, press any key.')
	block_num = 0
	for i in range(number_of_blocks):
		block_num += 1
		run_block(block_num,message_viewing_time)
		if block_num<number_of_blocks:
			message_viewing_time = show_message('Take a break!\n\nWhen you are ready to continue the experiment, press any key.')
	#stop nearly everything *then* show the "all done" message.
	writer_child.stop()
	if do_eyelink:
		eyelink_child.stop()
	message_viewing_time = show_message('You\'re all done!\nPlease the person running this experiment will be with you shortly.')
	stim_display_mirror_child.stop()
	stamper_child.stop(killAfter=60)
	while stamper_child.isAlive():
		time.sleep(.1)
	sys.exit()
