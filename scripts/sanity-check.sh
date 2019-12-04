#! /bin/sh

XMHOME=$HOME/.xmonad
PIDFILE=$XMHOME/run/xmonad.pid
[ -e $PIDFILE ] && read pid < $PIDFILE
echo $PPID > $PIDFILE
[ "$pid" = $PPID ] || exec $XMHOME/scripts/spawnOnce
