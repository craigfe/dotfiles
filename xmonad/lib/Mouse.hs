module Mouse where

import qualified Data.Map as M
import XMonad

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

bindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
bindings XConfig {XMonad.modMask = prevModMask} =
  M.fromList
    [ -- Set the window to floating mode and move by dragging
      ((prevModMask, button1), \w -> focus w >> mouseMoveWindow w),
      --     -- Raise the window to the top of the stack
      --     ((modMask, button2),
      --        \w -> focus w >> windows W.swapMaster),

      -- Set the window to floating mode and resize by dragging
      ((prevModMask, button3), \w -> focus w >> mouseResizeWindow w)
    ]
