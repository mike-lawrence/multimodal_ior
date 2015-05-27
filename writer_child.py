def writerChildFunction(
qTo
,qFrom
, window_size = [200,200]
, window_position = [0,0]
):
	import sdl2
	import sdl2.ext
	import sys
	import time
	try:
		import appnope
		appnope.nope()
	except:
		pass

	sdl2.SDL_Init(sdl2.SDL_INIT_VIDEO)
	window = sdl2.ext.Window("writer",size=window_size,position=window_position,flags=sdl2.SDL_WINDOW_SHOWN)
	windowID = sdl2.SDL_GetWindowID(window.window)
	windowSurf = sdl2.SDL_GetWindowSurface(window.window)
	sdl2.ext.fill(windowSurf.contents,sdl2.pixels.SDL_Color(r=255, g=255, b=255, a=255))
	window.refresh()

	for i in range(10):
		sdl2.SDL_PumpEvents() #to show the windows

	files = {}

	def exitSafely():
		try:
			for index,fileObj in files.items():
				fileObj.close()
				# gpg -r "Michael Lawrence <mike.lwrnc@gmail.com>" -e mac.txt
		except:
			pass
		sys.exit()

	while True:
		if not qTo.empty():
			message = qTo.get()
			if message=='quit':
				exitSafely()
			elif message[0]=='new_file':
				files[message[1]] = open(message[2],'w')
			elif message[0]=='write':
				files[message[1]].write(message[2]+'\n')
		else:
			time.sleep(1)
		sdl2.SDL_PumpEvents()
		for event in sdl2.ext.get_events():
			if event.type==sdl2.SDL_WINDOWEVENT:
				if (event.window.event==sdl2.SDL_WINDOWEVENT_CLOSE):
					exitSafely()

writerChildFunction(qTo,qFrom,**initDict)