#!/bin/sh

PATH=/home/derrick/.local/bin:$PATH

# Taffybar
if [ -z "$(pgrep taffybar)" ] ; then
  echo "Starting taffybar"
  my-taffybar &
fi

# Wallpaper
if [ -x $HOME/.fehbg ] ; then
  echo "Setting wallpaper"
  $HOME/.fehbg
fi

# Screensaver
if [ -z "$(pgrep xscreensaver)" ] ; then
  echo "Starting xscreensaver"
  xscreensaver -no-splash &
fi

# compton
if [ -z "$(pgrep compton)" ] ; then
  echo "Starting compton"
  compton -b &
fi

# Network Applet
if [ -z "$(pgrep nm-applet)" ] ; then
  echo "starting nm-applet"
  nm-applet &
fi

# Slack
if [ -z "$(pgrep slack)" ] ; then
  echo "starting Slack"
  slack &
fi
