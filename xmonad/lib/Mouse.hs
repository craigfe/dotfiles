module Mouse where

import XMonad
import qualified Data.Map                            as M

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myMouseBindings XConfig {XMonad.modMask = modMask} = M.fromList
  [
--     -- Set the window to floating mode and move by dragging
--     ((modMask, button1),
--      \w -> focus w >> mouseMoveWindow w),

--     -- Raise the window to the top of the stack
--     ((modMask, button2),
--        \w -> focus w >> windows W.swapMaster),

--     -- Set the window to floating mode and resize by dragging
--     ((modMask, button3),
--        \w -> focus w >> mouseResizeWindow w)
  ]
