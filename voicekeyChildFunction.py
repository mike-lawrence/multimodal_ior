def voicekeyChildFunction(
qTo
, qFrom
, windowSize = [200,200]
, windowPosition = [0,0]
):
	import sdl2
	import sdl2.ext
	import sys

	sdl2.SDL_Init(sdl2.SDL_INIT_VIDEO)
	window = sdl2.ext.Window("eyelink",size=windowSize,position=windowPosition,flags=sdl2.SDL_WINDOW_SHOWN)
	windowID = sdl2.SDL_GetWindowID(window.window)
	windowSurf = sdl2.SDL_GetWindowSurface(window.window)
	sdl2.ext.fill(windowSurf.contents,sdl2.pixels.SDL_Color(r=0, g=0, b=0, a=255))
	window.refresh()

	for i in range(10):
		sdl2.SDL_PumpEvents() #to show the windows

	#define a function that gets the time (unit=seconds,zero=?)
	def get_time():
		return sdl2.SDL_GetPerformanceCounter()*1.0/sdl2.SDL_GetPerformanceFrequency()

	def exitSafely():
		try:
			stream.stop_stream()
			stream.close()
			p.terminate()
		except:
			pass
		sys.exit()

	# from sys import byteorder
	from array import array
	from struct import pack

	import pyaudio
	import wave
	import time

	THRESHOLD = 500
	CHUNK_SIZE = 1024
	RATE = 44100

	p = pyaudio.PyAudio()
	stream = p.open(format=pyaudio.paInt16, channels=1, rate=RATE, input=True, output=True, frames_per_buffer=CHUNK_SIZE)
	stream_out = stream.read(CHUNK_SIZE)

	report_responses = False
	while True:
		sdl2.SDL_PumpEvents()
		for event in sdl2.ext.get_events():
			if event.type==sdl2.SDL_WINDOWEVENT:
				if (event.window.event==sdl2.SDL_WINDOWEVENT_CLOSE):
					exitSafely()
		if not qTo.empty():
			message = qTo.get()
			if message=='quit':
				exitSafely()
			elif message[0]=='report_responses':
				report_responses = message[1]
		try:
			stream_out = stream.read(CHUNK_SIZE)
		except: #close-down and re-initialize audio
			stream.stop_stream()
			stream.close()
			p.terminate()
			p = pyaudio.PyAudio()
			stream = p.open(format=pyaudio.paInt16, channels=1, rate=RATE, input=True, output=True, frames_per_buffer=CHUNK_SIZE)
			stream_out = stream.read(CHUNK_SIZE)
		if report_responses:
			snd_data = array('h', stream_out)
			# if byteorder == 'big':
			# 	snd_data.byteswap()
			max_amp = max(snd_data)
			if max_amp> THRESHOLD:
				qFrom.put(['response',get_time()])

voicekeyChildFunction(qTo,qFrom,**initDict)