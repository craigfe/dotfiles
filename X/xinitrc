#!/bin/sh

# Configure display settings using
xrandr --setprovideroutputsource modesetting NVIDIA-0

xrandr --output eDP-1 --auto \
	--output HDMI-1 --panning 2496x1404+2560+0 --scale 1.3x1.3 --right-of eDP-1

xset +fp /home/craigfe/.fonts
xset fp rehash

exec i3
