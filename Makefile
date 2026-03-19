PATH=/usr/bin
TARGET=xmonad-x86_64-linux
OBJS=*.o lib/*.o *.hi lib/*.hi
BAK=*~ */*~
all: check restart

restart: ${TARGET}
	./${TARGET} --restart
	sleep 2
	@echo done

check: ${TARGET}
	@clear 
	ghc -ilib -o ${TARGET} xmonad.hs
	pidof xmessage && killall xmessage; true
	./${TARGET} --recompile
	cp -pv ${TARGET} ${HOME}/.local/bin/xmonad
clean:
	@for x in ${BAK} *.out ${OBJS} ; \
	   do if [ -e $$x ] ; then rm -v  $$x ; fi; done

install:
	cp -pv ${TARGET} ${HOME}/.local/bin/xmonad

clobber: clean
	@find . -name "*.swp" -exec rm -v "{}" \;

wc:
	@find . \( -name test -o -name scripts \) -prune -o -name "*.hs" -exec wc -l {} + |sort -n
