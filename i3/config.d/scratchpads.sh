#!/bin/bash

# Get the current monitor dimensions
dimensions=$(xrandr --current | grep '*' | uniq | awk '{print $1}')
width=$(echo $dimensions | cut -d 'x' -f1)
height=$(echo $dimensions | cut -d 'x' -f2)

mess='[instance="www.messenger.com"]'
mess_height=$height
mess_width=$(calc "round($width * 0.30)")
mess_xpos=$(calc "$width - $mess_width")
mess_ypos=0

spot='[class="Spotify"]'
calc='[instance="calculator"]'
term='[instance="terminal"]'
slack='[class="Slack"]'

cat << EOF
for_window $spot scratchpad show, resize set 2300 1300, move position center
for_window $spot move window to scratchpad
for_window $spot floating enable

for_window $mess floating enable
for_window $mess move scratchpad
for_window $mess resize set ${mess_width}px ${mess_height}px, move position ${mess_xpos}px ${mess_ypos}px

for_window $calc floating enable
for_window $calc move scratchpad
for_window $calc resize set 1200px 800px, move position center
for_window $calc border pixel 0

for_window $term floating enable
for_window $term move scratchpad
for_window $term resize set 1400px 1000px, move position center
for_window $term border pixel 0

for_window $slack floating enable
for_window $slack move scratchpad
for_window $slack resize set 1840px 1000px, move position center
for_window $slack border pixel 1
EOF
