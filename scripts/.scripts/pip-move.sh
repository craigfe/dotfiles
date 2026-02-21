#!/bin/bash
# Get current workspace
current_workspace=$(aerospace list-workspaces --focused)
# Move PiP and Handy windows to current workspace
aerospace list-windows --all | grep -E "(Picture-in-Picture|Picture in Picture|Handy)" | awk '{print $1}' | while read window_id; do
    if [ -n "$window_id" ]; then
        aerospace move-node-to-workspace --window-id "$window_id" "$current_workspace"
    fi
done
