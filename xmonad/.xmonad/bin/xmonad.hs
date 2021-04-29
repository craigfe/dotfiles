{-# OPTIONS -fno-warn-missing-signatures #-}

import qualified Bindings
import qualified Config
import qualified DBus.Client as DC
import qualified Layouts
import qualified Logging
import qualified Mouse
import qualified Projects
import qualified Scratchpads
import qualified Startup
import XMonad
import XMonad.Actions.SpawnOn
import XMonad.Hooks.ManageDocks
import XMonad.Util.NamedScratchpad

-- -----------------------------------------------------------------------
-- Window rules
-- -----------------------------------------------------------------------

myManageHook :: ManageHook
myManageHook =
  manageSpawn
    <+> namedScratchpadManageHook Scratchpads.v

-- ------------------------------------------------------------------------
-- Colors and borders
-- ------------------------------------------------------------------------

myNormalBorderColor = "#2F343F"

myBorderWidth = 0

defaults accent =
  def
    { -- key bindings
      modMask = Bindings.myModMask,
      workspaces = Bindings.myWorkspaces,
      keys = Bindings.myKeys,
      mouseBindings = Mouse.bindings,
      focusFollowsMouse = Mouse.myFocusFollowsMouse,
      clickJustFocuses = Mouse.myClickJustFocuses,
      -- simple stuff
      terminal = Config.myTerminal,
      borderWidth = myBorderWidth,
      normalBorderColor = myNormalBorderColor,
      focusedBorderColor = accent,
      -- hooks, layouts
      layoutHook = Layouts.myLayout accent,
      manageHook = myManageHook,
      startupHook = Startup.myStartupHook
    }

main :: IO ()
main = do
  accentFile <- readFile Config.myAccentFile
  dbus <- DC.connectSession
  let accent = init accentFile
  xmonad $
    Layouts.my2DNavigation $
      Projects.v $
        docks
          (defaults accent)
            { handleEventHook = docksEventHook,
              logHook = Logging.myLogHook accent dbus
            }
