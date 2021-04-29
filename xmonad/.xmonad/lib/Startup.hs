module Startup where

import qualified Config
import XMonad
import XMonad.Core
import XMonad.Hooks.SetWMName
import XMonad.Util.Cursor

myStartupHook =
  do
    spawn "source ~/.fehbg"
    <+> spawn Config.myBarInit
    <+> setDefaultCursor xC_left_ptr
    <+> setWMName "LG3D"

-- <+> spawn "compton"
-- <+> spawn "xsetroot -solid '#F5F6F7'"
-- <+> spawn "xinput --set-prop 13 290 1"
-- <+> spawn "xinput --set-prop 13 302 0"
-- <+> spawn "~/bin/libinput-gestures"
-- <+> spawn "xrandr --output HDMI1 --off"
-- <+> spawn "xrandr --output HDMI1 --auto --right-of eDP1"
-- <+> spawn "firefox"
