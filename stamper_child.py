def stamperChildFunction(
qTo
, qFrom
, window_size = [200,200]
, window_position = [0,0]
, window_color = [255,255,255]
, do_border = True
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
	sdl2.SDL_Init(sdl2.SDL_INIT_TIMER)
	timeFreq = 1.0/sdl2.SDL_GetPerformanceFrequency()
	sdl2.SDL_Init(sdl2.SDL_INIT_VIDEO)
	if do_border:
		flags = sdl2.SDL_WINDOW_SHOWN
	else:
		flags = sdl2.SDL_WINDOW_BORDERLESS | sdl2.SDL_WINDOW_SHOWN
	window = sdl2.ext.Window("pyStamper",size=window_size,position=window_position,flags=flags)
	windowID = sdl2.SDL_GetWindowID(window.window)
	windowSurf = sdl2.SDL_GetWindowSurface(window.window)
	red = sdl2.pixels.SDL_Color(r=255, g=0, b=0, a=255)
	green = sdl2.pixels.SDL_Color(r=0, g=255, b=0, a=255)
	black = sdl2.pixels.SDL_Color(r=0, g=0, b=0, a=255)
	white = sdl2.pixels.SDL_Color(r=255, g=255, b=255, a=255)
	if do_border:
		sdl2.ext.fill(windowSurf.contents,green)
	else:
		sdl2.ext.fill(windowSurf.contents,sdl2.pixels.SDL_Color(r=window_color[0], g=window_color[1], b=window_color[2], a=255))
	window.refresh()

	for i in range(10):
		sdl2.SDL_PumpEvents() #to show the windows

	sdl2.SDL_Init(sdl2.SDL_INIT_JOYSTICK) #uncomment if you want joystick input
	sdl2.SDL_JoystickOpen(0) #uncomment if you want joystick input
	lostFocus = True
	lostColors = [red,black,red,white]
	lastRefreshTime = time.time()
	while True:
		if lostFocus and do_border:
			if time.time()>(lastRefreshTime+(2.0/60)):
				sdl2.ext.fill(windowSurf.contents,lostColors[0])
				window.refresh()
				lostColors.append(lostColors.pop(0))
				lastRefreshTime = time.time()
		sdl2.SDL_PumpEvents()
		if not qTo.empty():
			message = qTo.get()
			if message=='quit':
				sys.exit()
			elif message=='raise':
				sdl2.SDL_RaiseWindow(window.window)
		for event in sdl2.ext.get_events():
			if event.type==sdl2.SDL_WINDOWEVENT:
				if event.window.windowID == windowID:
					if (event.window.event==sdl2.SDL_WINDOWEVENT_CLOSE):
						qFrom.put({'type':'key','time':event.window.timestamp*timeFreq,'value':'escape'})
						sys.exit()
					elif event.window.event==sdl2.SDL_WINDOWEVENT_FOCUS_LOST:
						lostFocus = True
					elif event.window.event==sdl2.SDL_WINDOWEVENT_FOCUS_GAINED:
						lostFocus = False
						if do_border:
							sdl2.ext.fill(windowSurf.contents,green)
							window.refresh()
			else:
				message = {}
				if event.type==sdl2.SDL_KEYDOWN:
					message['type'] = 'key'
					message['time'] = event.key.timestamp*timeFreq
					message['value'] = sdl2.SDL_GetKeyName(event.key.keysym.sym).lower()
					message['keysym'] = event.key.keysym
					qFrom.put(message)
				elif event.type == sdl2.SDL_JOYAXISMOTION:
					message['type'] = 'axis'
					message['axis'] = event.jaxis.axis
					message['time'] = event.jaxis.timestamp*timeFreq
					message['value'] = event.jaxis.value
					qFrom.put(message)
				elif event.type == sdl2.SDL_JOYBUTTONDOWN:
					message['type'] = 'button'
					message['time'] = event.jbutton.timestamp*timeFreq
					message['value'] = event.jbutton.button
					qFrom.put(message)

stamperChildFunction(qTo,qFrom,**initDict)