#! /bin/bash
export PATH=~/bin:/usr/lucho/bin:/usr/local/bin:/bin:$PATH
#xrandr.cmd
pulseaudio  -D
xsetroot -bg '#773e3e' -fg '#c0c0b0'  -mod 16 16
xdaliclock -24 -cycle -bw 4 -transparent &
terminator&
xscreensaver -no-splash&

