#!/bin/bash

# Get the directory ../../
dotfiles="$(dirname "$(dirname "$(dirname "$(realpath $0)"))")")"
accent=$(cat "$dotfiles/colours/out/theme")

# Default to white if theme has not yet been set
[[ -z $accent ]] && accent="#ffffff"

cat << EOF
# class                 border  backgr. text    indicator child_border
client.focused         $accent $accent $accent $accent
# client.focused	#0099ff #0099ff #000000 #0099ff
# client.unfocused	#666666 #666666b #000000 #66666
# client.urgent	#ff0000 #ff0000 #000000 #ff0000
# client.background	#000000
EOF
