def eyelink_child_function(
qTo
, qFrom
, window_size = [200,200]
, window_position = [0,0]
, stim_display_res = [1920,1080]
, stim_display_position = [1920,0]
, calibration_display_size = [1920,1080]
, calibration_dot_size = 10
, eyelink_ip = '100.1.1.1'
, edf_file_name = 'temp.edf'
, edf_path = './_Data/temp.edf'
, saccade_sound_file = '_Stimuli/stop.wav'
, blink_sound_file = '_Stimuli/stop.wav'
):
	import sdl2
	import sdl2.ext
	import math
	import OpenGL.GL as gl
	import sdl2.sdlmixer
	import pylink
	import numpy
	import sys
	import shutil
	import subprocess
	import time
	import os
	import array
	from PIL import Image
	from PIL import ImageDraw
	try:
		import appnope
		appnope.nope()
	except:
		pass

	byteify = lambda x, enc: x.encode(enc)

	sdl2.SDL_Init(sdl2.SDL_INIT_VIDEO)
	window = sdl2.ext.Window("eyelink",size=window_size,position=window_position,flags=sdl2.SDL_WINDOW_SHOWN)
	windowID = sdl2.SDL_GetWindowID(window.window)
	windowSurf = sdl2.SDL_GetWindowSurface(window.window)
	sdl2.ext.fill(windowSurf.contents,sdl2.pixels.SDL_Color(r=0, g=0, b=0, a=255))
	window.refresh()

	for i in range(10):
		sdl2.SDL_PumpEvents() #to show the windows


	sdl2.SDL_Init(sdl2.SDL_INIT_AUDIO)
	sdl2.sdlmixer.Mix_OpenAudio(44100, sdl2.sdlmixer.MIX_DEFAULT_FORMAT, 2, 1024)
	class Sound:
		def __init__(self, fileName):
			self.sample = sdl2.sdlmixer.Mix_LoadWAV(sdl2.ext.compat.byteify(fileName, "utf-8"))
			self.started = False
		def play(self):
			self.channel = sdl2.sdlmixer.Mix_PlayChannel(-1, self.sample, 0)
			self.started = True
		def still_playing(self):
			if self.started:
				if sdl2.sdlmixer.Mix_Playing(self.channel):
					return True
				else:
					self.started = False
					return False
			else:
				return False

	saccade_sound = Sound(saccade_sound_file)
	blink_sound = Sound(blink_sound_file)

	def exit_safely():
		if 'eyelink' in locals():
			if eyelink.isRecording()==0:
				eyelink.stopRecording()
			eyelink.setOfflineMode()
			eyelink.closeDataFile()
			eyelink.receiveDataFile(edf_file_name,'temp.edf')
			eyelink.close()
			if os.path.isfile('temp.edf'):
				shutil.move('temp.edf', edf_path)
				# if os.path.isfile(edf_path):
				# 	subprocess.call('./edf2asc -y ./'+edf_path,shell=True)
		sys.exit() #process gets hung here if called when showing images from eyelink



	edf_path = './_Data/temp.edf' #temporary default location, to be changed later when ID is established
	done = False
	while not done:
		try:
			print '\nAttempting to connect to eyelink (check that wifi is off!)'
			eyelink = pylink.EyeLink(eyelink_ip)
			done = True
		except:
			while not qTo.empty():
				message = qTo.get()
				if message=='quit':
					exit_safely()
				else:
					qTo.put(message)		

	print 'Eyelink connected'
	eyelink.sendCommand('select_parser_configuration 0')# 0--> standard (cognitive); 1--> sensitive (psychophysical)
	eyelink.sendCommand('sample_rate 500')
	eyelink.setLinkEventFilter("SACCADE,BLINK,FIXATION,LEFT,RIGHT")
	eyelink.openDataFile(edf_file_name)
	eyelink.sendCommand("screen_pixel_coords =  %d %d %d %d" %(stim_display_res[0]/2 - calibration_display_size[0]/2 , stim_display_res[1]/2 - calibration_display_size[1]/2 , stim_display_res[0]/2 + calibration_display_size[0]/2 , stim_display_res[1]/2 + calibration_display_size[1]/2 ))
	eyelink.sendMessage("DISPLAY_COORDS  0 0 %d %d" %(stim_display_res[0],stim_display_res[1]))
	# eyelink.sendCommand("saccade_velocity_threshold = 60")
	# eyelink.sendCommand("saccade_acceleration_threshold = 19500")

	class EyeLinkCoreGraphicsPySDL2(pylink.EyeLinkCustomDisplay):
		def __init__(self):
			self.__target_beep__ = Sound('_Stimuli/type.wav')
			self.__target_beep__done__ = Sound('qbeep.wav')
			self.__target_beep__error__ = Sound('error.wav')
			if sys.byteorder == 'little':
				self.byteorder = 1
			else:
				self.byteorder = 0
			self.imagebuffer = array.array('I')
			self.pal = None
			self.__img__ = None
		def record_abort_hide(self):
			pass
		def play_beep(self,beepid):
			# if beepid == pylink.DC_TARG_BEEP or beepid == pylink.CAL_TARG_BEEP:
			if beepid == pylink.CAL_TARG_BEEP:
				self.__target_beep__.play()
			elif beepid == pylink.CAL_ERR_BEEP or beepid == pylink.DC_ERR_BEEP:
				self.__target_beep__error__.play()
			else:#	CAL_GOOD_BEEP or DC_GOOD_BEEP
				self.__target_beep__done__.play()
		def clear_cal_display(self):
			# print 'clear_cal_display'
			qFrom.put('clear_cal_display')
		def setup_cal_display(self):
			# print 'setup_cal_display'
			qFrom.put('setup_cal_display')
		def exit_cal_display(self): 
			# print 'exit_cal_display'
			qFrom.put('exit_cal_display')
		def erase_cal_target(self):
			# print 'erase_cal_target'
			qFrom.put('erase_cal_target')
		def draw_cal_target(self, x, y):
			# print 'draw_cal_target'
			qFrom.put(['draw_cal_target',x,y])
		def setup_image_display(self, width, height):
			# print 'eyelink: setup_image_display'
			self.img_size = (width,height)
			return 0
		def exit_image_display(self):
			# print 'eyelink: exit_image_display'
			pass
		def image_title(self,text):
			# print 'eyelink: image_title'
			pass
		def set_image_palette(self, r,g,b):
			# print 'eyelink: set_image_palette'
			self.imagebuffer = array.array('I')
			sz = len(r)
			i = 0
			self.pal = []
			while i < sz:
				rf = int(b[i])
				gf = int(g[i])
				bf = int(r[i])
				if self.byteorder:
					self.pal.append((rf<<16) | (gf<<8) | (bf))
				else:
					self.pal.append((bf<<24) |  (gf<<16) | (rf<<8)) #for mac
				i = i+1
		def draw_image_line(self, width, line, totlines,buff):
			# print 'eyelink: draw_image_line'
			i = 0
			while i < width:
				if buff[i]>=len(self.pal):
					buff[i] = len(self.pal)-1
				self.imagebuffer.append(self.pal[buff[i]&0x000000FF])
				i = i+1
			if line == totlines:
				img = Image.fromstring('RGBX', (width,totlines), self.imagebuffer.tostring())
 				img = img.convert('RGBA')
				self.__img__ = img.copy()
				self.__draw__ = ImageDraw.Draw(self.__img__)
				self.draw_cross_hair() #inherited method, calls draw_line and draw_losenge
				qFrom.put(['image',numpy.array(self.__img__)])
				self.__img__ = None
				self.__draw__ = None
				self.imagebuffer = array.array('I')
		def get_color_from_index(self,colorindex):
			if colorindex   ==  pylink.CR_HAIR_COLOR:          return (255,255,255,255)
			elif colorindex ==  pylink.PUPIL_HAIR_COLOR:       return (255,255,255,255)
			elif colorindex ==  pylink.PUPIL_BOX_COLOR:        return (0,255,0,255)
			elif colorindex ==  pylink.SEARCH_LIMIT_BOX_COLOR: return (255,0,0,255)
			elif colorindex ==  pylink.MOUSE_CURSOR_COLOR:     return (255,0,0,255)
			else: return (0,0,0,0)
		def draw_line(self,x1,y1,x2,y2,colorindex):
			# print 'eyelink: draw_line'
			if x1<0: x1 = 0
			if x2<0: x2 = 0
			if y1<0: y1 = 0
			if y2<0: y2 = 0
			if x1>self.img_size[0]: x1 = self.img_size[0]
			if x2>self.img_size[0]: x2 = self.img_size[0]
			if y1>self.img_size[1]: y1 = self.img_size[1]
			if y2>self.img_size[1]: y2 = self.img_size[1]
			imr = self.__img__.size
			x1 = int((float(x1)/float(self.img_size[0]))*imr[0])
			x2 = int((float(x2)/float(self.img_size[0]))*imr[0])
			y1 = int((float(y1)/float(self.img_size[1]))*imr[1])
			y2 = int((float(y2)/float(self.img_size[1]))*imr[1])
			color = self.get_color_from_index(colorindex)
			self.__draw__.line( [(x1,y1),(x2,y2)] , fill=color)
		def draw_lozenge(self,x,y,width,height,colorindex):
			# print 'eyelink: draw_lozenge'
			color = self.get_color_from_index(colorindex)
			imr = self.__img__.size
			x=int((float(x)/float(self.img_size[0]))*imr[0])
			width=int((float(width)/float(self.img_size[0]))*imr[0])
			y=int((float(y)/float(self.img_size[1]))*imr[1])
			height=int((float(height)/float(self.img_size[1]))*imr[1])
			if width>height:
				rad = height/2
				self.__draw__.line([(x+rad,y),(x+width-rad,y)],fill=color)
				self.__draw__.line([(x+rad,y+height),(x+width-rad,y+height)],fill=color)
				clip = (x,y,x+height,y+height)
				self.__draw__.arc(clip,90,270,fill=color)
				clip = ((x+width-height),y,x+width,y+height)
				self.__draw__.arc(clip,270,90,fill=color)
			else:
				rad = width/2
				self.__draw__.line([(x,y+rad),(x,y+height-rad)],fill=color)
				self.__draw__.line([(x+width,y+rad),(x+width,y+height-rad)],fill=color)
				clip = (x,y,x+width,y+width)
				self.__draw__.arc(clip,180,360,fill=color)
				clip = (x,y+height-width,x+width,y+height)
				self.__draw__.arc(clip,360,180,fill=color)
		def get_mouse_state(self):
			# pos = pygame.mouse.get_pos()
			# state = pygame.mouse.get_pressed()
			# return (pos,state[0])
			pass
		def get_input_key(self):
			ky=[]
			while not qTo.empty():
				message = qTo.get()
				if message=='quit':
					print 'received message to exit'
					exit_safely()
				elif message=='voice':
					ky.append(pylink.KeyInput(32,0)) #voicekey response translated to space keypress (for drift correct)
				elif message[0]=='keycode':
					keysym = message[1]
					keycode = keysym.sym
					if keycode == sdl2.SDLK_F1:           keycode = pylink.F1_KEY
					elif keycode == sdl2.SDLK_F2:         keycode = pylink.F2_KEY
					elif keycode == sdl2.SDLK_F3:         keycode = pylink.F3_KEY
					elif keycode == sdl2.SDLK_F4:         keycode = pylink.F4_KEY
					elif keycode == sdl2.SDLK_F5:         keycode = pylink.F5_KEY
					elif keycode == sdl2.SDLK_F6:         keycode = pylink.F6_KEY
					elif keycode == sdl2.SDLK_F7:         keycode = pylink.F7_KEY
					elif keycode == sdl2.SDLK_F8:         keycode = pylink.F8_KEY
					elif keycode == sdl2.SDLK_F9:         keycode = pylink.F9_KEY
					elif keycode == sdl2.SDLK_F10:        keycode = pylink.F10_KEY
					elif keycode == sdl2.SDLK_PAGEUP:     keycode = pylink.PAGE_UP
					elif keycode == sdl2.SDLK_PAGEDOWN:   keycode = pylink.PAGE_DOWN
					elif keycode == sdl2.SDLK_UP:         keycode = pylink.CURS_UP
					elif keycode == sdl2.SDLK_DOWN:       keycode = pylink.CURS_DOWN
					elif keycode == sdl2.SDLK_LEFT:       keycode = pylink.CURS_LEFT
					elif keycode == sdl2.SDLK_RIGHT:      keycode = pylink.CURS_RIGHT
					elif keycode == sdl2.SDLK_BACKSPACE:  keycode = ord('\b')
					elif keycode == sdl2.SDLK_RETURN:     keycode = pylink.ENTER_KEY
					elif keycode == sdl2.SDLK_ESCAPE:     keycode = pylink.ESC_KEY
					elif keycode == sdl2.SDLK_TAB:        keycode = ord('\t')
					elif keycode == pylink.JUNK_KEY:      keycode = 0
					ky.append(pylink.KeyInput(keycode,keysym.mod))
			return ky

	custom_display = EyeLinkCoreGraphicsPySDL2()
	pylink.openGraphicsEx(custom_display)
	new_gaze_target = False
	gaze_target = numpy.array(calibration_display_size)/2.0
	gaze_target_criterion = calibration_dot_size
	do_sounds = False
	report_saccades = False
	report_blinks = False
	last_message_time = time.time()
	last_start_blink_time = time.time()
	while True:
		sdl2.SDL_PumpEvents()
		for event in sdl2.ext.get_events():
			if event.type==sdl2.SDL_WINDOWEVENT:
				if (event.window.event==sdl2.SDL_WINDOWEVENT_CLOSE):
					exit_safely()
		if not qTo.empty():
			message = qTo.get()
			if message=='quit':
				exit_safely()
			elif message[0]=='edf_path':
				edf_path = message[1]
			elif message[0]=='do_sounds':
				do_sounds = message[1]
			elif message[0]=='report_saccades':
				report_saccades = message[1]
			elif message[0]=='report_blinks':
				report_blinks = message[1]
			elif message[0]=='send_message':
				eyelink.sendMessage(message[1])
			elif message=='do_drift_correct':
				if eyelink.isRecording()==0:
					eyelink.stopRecording()
				try:
					error = eyelink.doDriftCorrect(stim_display_res[0]/2,stim_display_res[1]/2,0,1)
					# print error
					if error != 27: 
						qFrom.put('drift_correct_complete')
						eyelink.startRecording(1,1,1,1) #this retuns immediately takes 10-30ms to actually kick in on the tracker
					else:
						qFrom.put('do_calibration')
				except:
					qFrom.put('do_calibration')
			elif message[0]=='new_gaze_target':
				# print message
				new_gaze_target = True
				gaze_target = numpy.array(message[1])
				gaze_target_criterion = numpy.array(message[2])
				# print message
				# print 'waiting for gaze confirmation'
			elif message[0]=='accept_trigger':
				eyelink.accept_trigger()
			elif message=='do_calibration':
				do_sounds = False
				if eyelink.isRecording()==0:
					eyelink.stopRecording()
				eyelink.doTrackerSetup()
				qFrom.put('calibration_complete')
		if eyelink.isRecording()==0: #stupid, I know, but eyelink.isRecording() returns 0 if it *is* indeed recording!
			eye_data = eyelink.getNextData()
			# if eye_data==pylink.SAMPLE_TYPE:
			# 	eye_sample = eyelink.getFloatData()
			# 	gaze = None
			# 	if eye_sample.isRightSample():
			# 		gaze = eye_sample.getRightEye().getGaze()
			# 	elif eye_sample.isLeftSample():
			# 		gaze = eye_sample.getLeftEye().getGaze()
			# 	if gaze!=None:
			# 		if gaze[0]!=-32768.0:
			# 			gaze_dist_from_gaze_target = numpy.linalg.norm(numpy.array(gaze)-gaze_target)
			# 			if new_gaze_target:
			# 				if gaze_dist_from_gaze_target<gaze_target_criterion:
			# 					print ['gaze_target_met',gaze,gaze_target_criterion,gaze_target,gaze_dist_from_gaze_target]
			# 					qFrom.put(['gaze_target_met',gaze_target])
			# 					new_gaze_target = False
			# 				else:
			# 					qFrom.put(['gaze_targetNotMet',gaze_target])
			# 					print ['gaze_targetNotMet',gaze,gaze_target,gaze_dist_from_gaze_target,gaze_target_criterion]
			if eye_data==pylink.ENDSACC:
				eye_sample = eyelink.getFloatData()
				gaze_start = eye_sample.getStartGaze()
				gaze_end = eye_sample.getEndGaze()
				# print ['eyelink: saccade',gaze_start,gaze_end,gaze_target]
				if (gaze_start[0]!=-32768.0) & (gaze_end[0]!=-32768.0):
					gaze_dist_from_gaze_target = numpy.linalg.norm(numpy.array(gaze_end)-gaze_target)
					if gaze_dist_from_gaze_target<1000:
						if new_gaze_target:
							if gaze_dist_from_gaze_target<gaze_target_criterion:
								# print ['gaze_target_met',gaze_end,gaze_target_criterion,gaze_target,gaze_dist_from_gaze_target]
								qFrom.put(['gaze_target_met',gaze_target])
								new_gaze_target = False
						elif gaze_dist_from_gaze_target>gaze_target_criterion:
							if report_saccades:
								qFrom.put('gaze_target_lost')
								# print ['gaze_target_lost',gaze_target]
							if (not saccade_sound.still_playing()) and (not blink_sound.still_playing()):
								if do_sounds:
									saccade_sound.play()
						else:
							if report_saccades:
								qFrom.put(['smaller_saccade',gaze_dist_from_gaze_target,])
			elif eye_data==pylink.STARTBLINK:
				last_start_blink_time = time.time()
			elif eye_data==pylink.ENDBLINK:
				if (time.time()-last_start_blink_time)>.1:
					if report_blinks:
						qFrom.put('blink')
						# print 'eyelink: blink'
					if (not saccade_sound.still_playing()) and (not blink_sound.still_playing()):
						if do_sounds:
							blink_sound.play()



eyelink_child_function(qTo,qFrom,**initDict)