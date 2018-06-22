xrandr --setprovideroutputsource modesetting NVIDIA-0
xrandr --auto
xrandr --dpi 110

xset +fp /home/craigfe/.fonts
xset fp rehash

exec i3
