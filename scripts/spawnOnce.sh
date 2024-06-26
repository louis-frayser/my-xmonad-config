#! /bin/bash
# NOTE:  The scrpt needs to return -- run long-running process in the background  
set -x
export PATH=~/bin:/usr/lucho/bin:/usr/local/bin:/bin:$PATH
xrdb -merge ~/.Xresources
setxkbmap -option "compose:lwin"
#xrandr.cmd
pulseaudio  -D
xsetroot -bg 'gray30' -fg '#c0c0b0'  -mod 16 16

## Background
#fbsetbg -t /usr/share/backgrounds/larry-the-cow/gentoo-larry-bg-1920x1080.png
fbsetbg -t "/export/images/public/Wallpaper/Hot Chix/tatoo-lady.jpg"

## Warn user to mount the encrypted ~/doc directory
mountpoint ~/doc || { xmessage -font "-*-lucida-*-r-*-*-24-*-*-*-*-*-*-*" "Mount the crypt! Press [Okay]!" & }

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

## Browser
xdg-open "http://localhost:8008" &

exit 0

