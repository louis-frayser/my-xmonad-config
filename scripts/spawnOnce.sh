#! /bin/bash
export PATH=~/bin:/usr/lucho/bin:/usr/local/bin:/bin:$PATH
#xrandr.cmd
#pulseaudio  -D
#xsetroot -bg '#773e3e' -fg '#c0c0b0'  -mod 16 16
fbsetbg -t /usr/share/backgrounds/larry-the-cow/gentoo-larry-bg-1920x1080.png

#xdaliclock -24 -cycle -bw 4 -transparent &
{ terminator --profile pretty || konsole || mate-terminal;} &
xscreensaver -no-splash&

