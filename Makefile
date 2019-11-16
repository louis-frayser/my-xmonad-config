OBJS=*.o lib/*.o *.hi lib/*.hi
BAK=*~ lib/*~
all: check

check:
	@clear 
	ghc -c lib/MyViews.hs lib/MyKeys.hs *.hs
	pidof xmessage && killall xmessage; true
	xmonad --recompile
clean:
	@for x in ${BAK} *.out ${OBJS} ; \
	   do if [ -e $$x ] ; then rm -v  $$x ; fi; done
	   
wc:
	find . -name "*.hs" -o -name "Makefile" | xargs wc -l
