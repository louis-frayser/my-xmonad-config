#! /bin/sh

: {XMONAD_HOME:="$HOME/.config/xmonad"}

RUNDIR=$XMONAD_HOME/run
PIDFILE=$RUNDIR/xmonad.pid

ppids () 
{ 
    local pid=${1:-$$};
    echo $pid;
    if [ $pid -ne 1 ]; then
        ppids $( ps -h -oppid $pid);
    fi
}

## --------------------------------------------------
xmpid(){
    ps -h -opid,fname $(ppids) |
	while read pid fname
	do case $fname in xmonad*) echo $pid
				   break
				   ;;
	   esac
	done
}


if [ -e $PIDFILE ] 
then read pid < $PIDFILE
else
  [ -d $RUNDIR ] || mkdir $RUNDIR
fi
XMPID=$(xmpid)
echo $XMPID > $PIDFILE

xhost local:
#xmessage XMPID: $XMPID &

### Run if not already run
[ "$pid" = "$XMPID" ] || exec "$XMONAD_HOME/scripts/spawnOnce.sh"


### Verify the necesary auxilary packages are installed.

echo "Verifying auxillary packages..."
for x in xmessage xmobar grun dmenu
do echo -n "$x: "
   if which $x
   then echo "OK."
   else echo "Missing!"
	xmessage "Package $x is missing" &
   fi
done
