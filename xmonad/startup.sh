#!/bin/sh

# Taffybar
if [ -z "$(pgrep taffybar)" ] ; then
     my-taffybar &
fi

# Redshift
#if [ -z "$(pgrep redshift)" ] ; then
#    redshift &
#fi

# Autolock
# if [ -z "$(pgrep xautolock)" ] ; then
    # xautolock -time 1 -locker "if ! grep 'RUNNING' /proc/asound/card*/pcm*/sub*/status;then xscreensaver-command -lock; else echo 'Sound on'; fi"
# fi

# Wallpaper
if [ -x $HOME/.fehbg ] ; then
  $HOME/.fehbg
fi

# Screensaver
if [ -z "$(pgrep xscreensaver)" ] ; then
    xscreensaver -no-splash &
fi

# compton
if [ -z "$(pgrep compton)" ] ; then
    compton -b &
fi

# Network Applet
if [ -z "$(pgrep nm-applet)" ] ; then
    nm-applet &
fi

