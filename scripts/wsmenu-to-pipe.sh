#! /bin/bash
## Called with buttons.  Needs to write selected button
## to .xmonad/run/fifo


x=$HOME/.xmonad
run=$x/run
doc=$x/doc
export DISPLAY=:0
fifo=$run/fifo
help=$doc/xmonad.cat

clear_fifo(){
    rm -fv $fifo 
    mkfifo $fifo
}

{ # All output to .xerrors-log
    buttons=($(IFS=,;for b in $1;do echo $b;done))
    for ((start=0; start < ${#buttons[@]};start++))
    do list=(${buttons[@]:$start})
       left=${list[0]}
       for ((i=1;i<${#list[@]};i++));do
	   pairs+=("$left-${list[$i]}")
       done
    done
    p0=${pairs[@]/#/,}
    px=${p0// /}
    echo $px
set -x
##selected_button=$(xmessage -file $help -buttons "$1" -print </dev/null)
#echo -e ${buttons[@]/%/"\n"} |dmenu > $fifo
for b in ${buttons[@]}; do echo $b;done |dmenu > $fifo

##echo DEBUG: selected: $selected_button 1>&2
##echo  $selected_button  >> $fifo 
 
} 1>&2
