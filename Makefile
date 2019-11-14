all: check

check:
	pidof xmessage && killall xmessage; true
	xmonad --recompile
