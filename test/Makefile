all: run

fifo:
	mkfifo fifo

run: waitio fifo
	./$<

waitio : waitio.hs
	ghc --make $@
