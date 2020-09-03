#! /bin/sh

XMHOME=$HOME/.xmonad
RUNDIR=$XMHOME/run
PIDFILE=$RUNDIR/monad.pid
if [ -e $PIDFILE ] 
then read pid < $PIDFILE
else
  [ -d $RUNDIR ] || mkdir $RUNDIR
fi
echo $PPID > $PIDFILE
xhost local:
[ "$pid" = $PPID ] || exec $XMHOME/scripts/spawnOnce.sh
