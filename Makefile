TARGET=xmonad-x86_64-linux
OBJS=*.o lib/*.o *.hi lib/*.hi
BAK=*~ */*~
all: check

check:
	@clear 
	ghc -ilib -o ${TARGET} xmonad
	pidof xmessage && killall xmessage; true
	xmonad --recompile
clean:
	@for x in ${BAK} *.out ${OBJS} ; \
	   do if [ -e $$x ] ; then rm -v  $$x ; fi; done

wc:
	@find . -name "*.hs" -o -name "Makefile" | xargs wc -l

clobber: clean
	@find . -name "*.swp" -exec rm -v "{}" \;
