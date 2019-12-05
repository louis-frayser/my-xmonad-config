#! /bin/bash
## Called with buttons.  Writes the selected button to
##  the fifo: .xmonad/run/fifo



fifo=$HOME/.xmonad/run/fifo

(IFS=,;for b in $1;do echo $b;done) |dmenu > $fifo

