e! /bin/bash
# NOTE:  The scrpt needs to return -- run long-running process in the background  
set -x
export PATH=~/bin:/usr/lucho/bin:/usr/local/bin:/bin:$PATH
xrdb -merge ~/.Xresources
setxkbmap -option "compose:lwin"
pulseaudio  -D
xsetroot -bg 'gray30' -fg '#c0c0b0'  -mod 16 16

## Background
#fbsetbg -t /usr/share/backgrounds/larry-the-cow/gentoo-larry-bg-1920x1080.png
fbsetbg -t "/export/images/public/Wallpaper/Hot Chix/tatoo-lady.jpg"
for x in {1..2};do sudo terminator& sleep 1;done
# Mount encrypted directories; and launh a root term while we still have su.
if ! mountpoint ${HOME}/doc
then xterm -e  \
   sh -c  'echo "Mounting the crypt...";\
           sudo ~toor/bin/mount-the-crypt ||  \
              read -p "problems!:"; \
           read -p ":";terminator&'
fi

{
# https://wiki.parabola.nu/GNOME_Keyring#Use_Without_Gnome 2021-05-20
# Start a dbus-session if not already running
source /etc/X11/xinit/xinitrc.d/?0-dbus

# Start or connect Gnome-Keyring
eval $(/usr/bin/gnome-keyring-daemon --start --daemonize --components=gpg,pkcs11,secrets,ssh)

# You probably need to do this too:
export SSH_AUTH_SOCK
export GPG_AGENT_INFO
export GNOME_KEYRING_CONTROL
export GNOME_KEYRING_PID
}

/usr/libexec/mate-settings-daemon&
#xdaliclock -24 -cycle -bw 4 -transparent &
# { terminator --profile pretty || konsole || mate-terminal;} &
xscreensaver -no-splash&
redshift &  # Adjust screen colortemp for night/day

## Browser
## Disabled to prevent writing the cache & profile
##directories before I can set ramdisks for them
## Chrome writes too often for disk's health
##xdg-open "http://localhost:8008" &

exit 0

