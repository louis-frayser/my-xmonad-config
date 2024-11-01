#! /bin/sh

#### This script is called from a keyboard shortcup to 
#### toggle the monitor layout via xrandr. Use ln to 
#### link this to ~/bin/xdpytoggle. It mouse be executable

## Select a script from ~/bin
## These scripts were originally created by arandr(1)
if xrandr -q |grep 3840
then mirror.sh
else default.sh
fi

