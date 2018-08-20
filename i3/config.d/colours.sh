#!/bin/bash

accent='#FFAF1B'

cat << EOF
# class                 border  backgr. text    indicator child_border
client.focused         $accent $accent $accent $accent

# client.focused	#0099ff #0099ff #000000 #0099ff
# client.unfocused	#666666 #666666b #000000 #66666
# client.urgent	#ff0000 #ff0000 #000000 #ff0000
# client.background	#000000
EOF
