if __name__ == '__main__':
	import fileForker
	import time
	voicekey_window_size = (200,200)
	voicekey_window_position = (-1440+300,0)
	voicekey_child = fileForker.childClass(childFile='voicekey_child.py')
	voicekey_child.initDict['window_size'] = voicekey_window_size
	voicekey_child.initDict['window_position'] = voicekey_window_position
	voicekey_child.start()
	voicekey_child.qTo.put(['report_responses',True])
	start = time.time()
	while (time.time()-start)<10:
		if not voicekey_child.qFrom.empty():
			print voicekey_child.qFrom.get()
	voicekey_child.stop()