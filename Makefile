all: check

check:
	pidof xmessage && killall xmessage; true
	xmonad --recompile
clean:
	@for x in *~ *.out *.hi *.o; do if [ -e $$x ] ; then rm -v  $$x ; fi; done
	
