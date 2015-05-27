def stimDisplayMirrorChildFunction(
qTo
,qFrom
, window_size = [1920/2,1080/2]
, window_position = [0,0]
):
	import sdl2
	import sdl2.ext
	import sys
	import time
	from PIL import Image #for image manipulation
	try:
		import appnope
		appnope.nope()
	except:
		pass

	sdl2.SDL_Init(sdl2.SDL_INIT_VIDEO)
	window = sdl2.ext.Window("mirror",size=window_size,position=window_position,flags=sdl2.SDL_WINDOW_SHOWN)
	windowID = sdl2.SDL_GetWindowID(window.window)
	windowSurf = sdl2.SDL_GetWindowSurface(window.window)
	windowArray = sdl2.ext.pixels3d(windowSurf.contents)

	sdl2.ext.fill(windowSurf.contents,sdl2.pixels.SDL_Color(r=255, g=255, b=255, a=255))
	window.refresh()

	for i in range(10):
		sdl2.SDL_PumpEvents() #to show the windows

	def exitSafely():
		sys.exit()

	while True:
		if not qTo.empty():
			message = qTo.get()
			if message=='quit':
				exitSafely()
			elif message[0]=='frame':
				# print ['q',time.time()-message[3]] #time spent in queue
				res = message[1]
				buffer = message[2]
				image = Image.fromstring(mode="RGB", size=res, data=buffer)
				image = image.transpose(Image.ROTATE_270)
				# start = time.time()
				image.thumbnail([res[1]/2,res[0]/2],Image.LANCZOS)
				# print ['resize',time.time()-start]
				windowArray[:,:,0:3] = image
				window.refresh()
		sdl2.SDL_PumpEvents()
		for event in sdl2.ext.get_events():
			if event.type==sdl2.SDL_WINDOWEVENT:
				if (event.window.event==sdl2.SDL_WINDOWEVENT_CLOSE):
					exitSafely()

stimDisplayMirrorChildFunction(qTo,qFrom,**initDict)