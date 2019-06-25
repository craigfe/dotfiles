import           Control.Arrow                       hiding ((<+>), (|||))
import           Data.Char
import qualified DBus.Client                         as DC
import           Graphics.X11.ExtraTypes
import           System.Exit
import           System.IO
import           XMonad
import           XMonad.Actions.SpawnOn
import           XMonad.Actions.Navigation2D
import           XMonad.Actions.WorkspaceNames
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Operations
import qualified XMonad.Util.ExtensibleState         as XS
import           XMonad.Util.NamedScratchpad

import Bindings
import Projects
import Layouts
import Logging
import Mouse
import Scratchpads
import Startup

-- -----------------------------------------------------------------------
-- Window rules
-- -----------------------------------------------------------------------

myManageHook :: ManageHook
myManageHook = manageSpawn
           <+> namedScratchpadManageHook scratchpads

--           <+> manageDocks
           -- <+> composeAll [
                 -- isDialog                        --> placeHook (fixed (0.5, 0.5)),
                 -- className =? "Firefox"          --> doShift (head myWorkspaces),
                 -- className =? "Atom"             --> doShift (myWorkspaces !! 2),
                 -- className =? "Emacs"            --> doShift (myWorkspaces !! 2),
                 -- className =? "Spotify"          --> doShift (myWorkspaces !! 9),
                 -- resource  =? "desktop_window"   --> doIgnore,
                 -- className =? "Gimp"             --> doFloat,
                 -- className =? "Oblogout"         --> doFloat,
                 -- className =? "Vlc"              --> doShift (myWorkspaces !! 8),
                 -- className =? "Zeal"             --> doFloat,
                 -- isFullscreen                    --> (doF W.focusDown <+> doFullFloat)
                 -- ]

-- ------------------------------------------------------------------------
-- Colors and borders
-- ------------------------------------------------------------------------

myNormalBorderColor = "#2F343F"
myBorderWidth = 0

-- toggleHDMI = do
--   count <- countScreens
--   spawn $ "echo " ++ show count ++ " >> ~/test.txt"
--   if count > 1
--     then spawn "xrandr --output HDMI1 --off"
--     else spawn "sleep 0.3; xrandr --output HDMI1 --auto --right-of eDP1"

-- ------------------------------------------------------------------------
-- Set defaults
-- ------------------------------------------------------------------------

defaults accent = def {
    terminal           = myTerminal,
    focusFollowsMouse  = myFocusFollowsMouse,
    borderWidth        = myBorderWidth,
    modMask            = myModMask,
    workspaces         = myWorkspaces,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = accent,
    keys               = myKeys,
    mouseBindings      = myMouseBindings,
    layoutHook         = myLayout accent,
    manageHook         = myManageHook,
    startupHook        = myStartupHook
--     handleEventHook    = E.fullscreenEventHook
}

myNavigation = def {
	  layoutNavigation   = [ (threeColName, centerNavigation)
                               , (bspName, centerNavigation)
                               , (fullscreenName, centerNavigation) ]
	, unmappedWindowRect = [ (fullscreenName, singleWindowRect) ]
	}

main = do
	accentFile <- readFile myAccentFile
	dbus <- DC.connectSession
	let accent = init accentFile
	xmonad
		$ withNavigation2DConfig myNavigation
		$ projects
		$ docks (defaults accent) {
		workspaces = myWorkspaces,
		handleEventHook = docksEventHook,
		logHook = myLogHook accent dbus
		}

