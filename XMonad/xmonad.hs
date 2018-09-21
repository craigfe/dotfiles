import qualified Codec.Binary.UTF8.String as UTF8
import Data.Maybe
import Graphics.X11.ExtraTypes -- XF86 keys
import System.IO
import System.Exit
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.SpawnOn
import XMonad.Hooks.DynamicLog
-- import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
-- import XMonad.Hooks.Place
import XMonad.Hooks.SetWMName
import XMonad.Layout.ShowWName
import XMonad.Layout.Accordion
import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps
import XMonad.Layout.IndependentScreens
import XMonad.Layout.MultiToggle
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.Spacing
import XMonad.Util.Cursor
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceCompare

-- import qualified XMonad.Hooks.EwmhDesktops as E
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import qualified DBus as D
import qualified DBus.Client as DC

-- ------------------------------------------------------------------------
-- Application Names
-- ------------------------------------------------------------------------

myTerminal = "urxvt"
myScreensaver = "/usr/bin/gnome-screensaver-command --lock"
mySelectScreenshot = "select-screenshot"
myScreenshot = "screenshot"
myLauncher = "/home/craigferguson/repos/dotfiles/rofi/menu/run"
myLock = "i3lock -c 000000"
mySystemMenu = "/home/craigferguson/repos/dotfiles/rofi/menu/system"
mySink = "alsa_output.pci-0000_00_1f.3.analog-stereo"

-- ------------------------------------------------------------------------
-- Workspaces
-- ------------------------------------------------------------------------

-- wsGEN = "\xf269"
-- wsWRK = "\xf02d"
-- wsSYS = "\xf300"
-- wsMED = "\xf001"
-- wsTMP = "\xf2db"
-- wsGAM = "\xf11b"

myWorkspaces :: [String]
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]


-- myWorkspaces = clickable workspaces
--   where clickable l =
--           [ "<action=xdotool key super+" ++ show i ++ " button=1>" ++ ws ++ "</action>"
--           | (i,ws) <- zip ([1..9] ++ [0]) l ]
--         workspaces = zipWith makeLabel [1..10] icons
--         makeLabel index icon = show index ++ ": <fn=1>" ++ icon : "</fn> "
--         icons = [ '\xf269', '\xf120', '\xf121', '\xf128', '\xf128',
--                   '\xf128', '\xf128', '\xf128', '\xf1b6', '\xf1bc' ]

-- ------------------------------------------------------------------------
-- Scratchpads
-- ------------------------------------------------------------------------

scratchpads = 
  [ NS "spotify" "spotify" (className =? "Spotify") (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
  , NS "messenger" "google-chrome --app=https://www.messenger.com/" (appName =? "www.messenger.com") (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)) 
  ]

-- -----------------------------------------------------------------------
-- Window rules
-- -----------------------------------------------------------------------

myManageHook :: ManageHook
myManageHook = manageSpawn
--           <+> manageDocks
           <+> namedScratchpadManageHook scratchpads
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
-- Layouts
-- ------------------------------------------------------------------------

accent  = "#90F9FF"
base03  = "#002b36"
base02  = "#073642"
base01  = "#586e75"
base00  = "#657b83"
base0   = "#839496"
base1   = "#93a1a1"
base2   = "#eee8d5"
base3   = "#fdf6e3"
yellow  = "#b58900"
orange  = "#cb4b16"
red     = "#dc322f"
magenta = "#d33682"
violet  = "#6c71c4"
blue    = "#268bd2"
cyan    = "#2aa198"
green   = "#859900"

topBarTheme = def
    { fontName              = "mono 10"
    , inactiveBorderColor   = base03
    , inactiveColor         = base03
    , inactiveTextColor     = base03
    , activeBorderColor     = accent
    , activeColor           = accent
    , activeTextColor       = accent
    , urgentBorderColor     = red
    , urgentTextColor       = yellow
    , decoHeight            = 5
    }

myLayout = tiled ||| fullscreen ||| accordion
--        ||| noBorders (fullscreenFull Full)
  where
    tiled      = (named "Tiled" . addTopBar . myGaps . avoidStruts) (Tall nmaster delta ratio)
    fullscreen = (named "Fullscreen" . avoidStruts . noBorders . fullscreenFull) Full
    accordion  = (named "Accordion" . avoidStruts . noBorders) Accordion

    nmaster = 1
    ratio = 1/2
    delta = 3/100

    addTopBar = noFrillsDeco shrinkText topBarTheme
    myGaps = lessBorders OnlyFloat
               . avoidStruts
               . smartSpacing gap
               . gaps [(U,gap), (D,gap), (R,gap), (L,gap)]
    gap = 5

-- ------------------------------------------------------------------------
-- Colors and borders
-- ------------------------------------------------------------------------

myNormalBorderColor = "#2F343F"
myFocusedBorderColor = "#B8CDD4"
myBorderWidth = 0

-- ------------------------------------------------------------------------
-- Key bindings
-- ------------------------------------------------------------------------

myModMask = mod4Mask
myKeys conf@XConfig {XMonad.modMask = modMask} = M.fromList $ [

    ((modMask, xK_Return), spawn $ XMonad.terminal conf),
    ((modMask .|. controlMask, xK_l), spawn myScreensaver),

    ((modMask, xK_a), namedScratchpadAction scratchpads "messenger"),
    ((modMask, xK_d), spawn myLauncher),
    ((modMask, xK_BackSpace), spawn mySystemMenu),
    ((modMask .|. shiftMask, xK_p), spawn mySelectScreenshot),
    ((modMask .|. controlMask .|. shiftMask, xK_p), spawn myScreenshot),

     -- Multimedia keys
    ((0, xF86XK_AudioMute), spawn $  "pactl set-sink-mute " ++ mySink ++ " toggle"),
    ((0, xF86XK_AudioLowerVolume), spawn $  "pactl set-sink-mute " ++ mySink ++ " false; pactl set-sink-volume " ++ mySink ++ " -5%"),
    ((0, xF86XK_AudioRaiseVolume), spawn $  "pactl set-sink-mute " ++ mySink ++ " false; pactl set-sink-volume " ++ mySink ++ " +5%"),
    ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 10"),
    ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 10"),
    ((0, xF86XK_AudioPrev), spawn "playerctl previous"),
    ((0, xF86XK_AudioPlay), spawn "playerctl play-pause"),
    ((0, xF86XK_AudioNext), spawn "playerctl next"),

    ((modMask .|. shiftMask, xK_Tab), moveToNextNonEmptyNoWrap),
    ((modMask .|. shiftMask .|. mod1Mask, xK_Tab), moveToPrevNonEmptyNoWrap),
    ((modMask, xK_e), moveTo Next EmptyWS),

    ((modMask .|. mod1Mask, xK_w), toggleHDMI),
    -- ((modm, xK_s), toggleSaveState),
    -- ((modm .|. shiftMask, xK_s), launchDocuments),
    ((modMask, xK_q), kill),
    ((modMask, xK_w), spawn "google-chrome"),
    ((modMask, xK_r), spawn "urxvt -e ranger"),
    ((modMask, xK_t), withFocused $ windows . W.sink),
    ((modMask, xK_u), windows W.swapDown),
    ((modMask, xK_i), windows W.swapUp),
    ((modMask, xK_p), sendMessage $ IncGap 5 R),
    ((modMask, xK_bracketright), sendMessage $ DecGap 5 R),
    ((modMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf),
    ((modMask, xK_n), refresh),
    ((modMask, xK_Tab), windows W.focusDown),
    ((modMask, xK_j), windows W.focusDown),
    ((modMask, xK_k), windows W.focusUp),
    ((modMask, xK_n), sendMessage NextLayout),
    ((modMask, xK_m), namedScratchpadAction scratchpads "spotify"),
    ((modMask .|. shiftMask, xK_m), windows W.focusMaster),
    ((modMask .|. shiftMask, xK_Return), windows W.swapMaster),
    ((modMask .|. shiftMask, xK_j), windows W.swapDown),
    ((modMask .|. shiftMask, xK_k), windows W.swapUp),
    ((modMask, xK_h), sendMessage Shrink),
    ((modMask, xK_l), sendMessage Expand),
    ((modMask, xK_x), spawn myLock),
    ((modMask, xK_comma), sendMessage (IncMasterN 1)),
    ((modMask, xK_period), sendMessage (IncMasterN (-1))),
    ((modMask .|. shiftMask, xK_F2), io exitSuccess),
    ((modMask, xK_F2), restart "xmonad" True)
  ]
  ++
  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  [((m .|. modMask, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])
      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

compareToCurrent :: X (WindowSpace -> Ordering)
compareToCurrent =
    do comp <- getWsCompare
       ws <- gets windowset
       let cur = W.workspace (W.current ws)
       return $ comp (W.tag cur) . W.tag

greaterNonEmptyWs =
    do comp <- compareToCurrent
       return (\w -> comp w == LT && isJust (W.stack w))

lessNonEmptyWs =
    do comp <- compareToCurrent
       return (\w -> comp w == GT && isJust (W.stack w))

moveToNextNonEmptyNoWrap = moveTo Next (WSIs greaterNonEmptyWs)
moveToPrevNonEmptyNoWrap = moveTo Prev (WSIs lessNonEmptyWs)

toggleHDMI = do
  count <- countScreens
  spawn $ "echo " ++ show count ++ " >> ~/test.txt"
  if count > 1
    then spawn "xrandr --output HDMI1 --off"
    else spawn "sleep 0.3; xrandr --output HDMI1 --auto --right-of eDP1"

-- ------------------------------------------------------------------------
-- Mouse bindings
-- ------------------------------------------------------------------------

-- myFocusFollowsMouse :: Bool
-- myFocusFollowsMouse = True

-- myMouseBindings XConfig {XMonad.modMask = modMask} = M.fromList
--   [
--     -- Set the window to floating mode and move by dragging
--     ((modMask, button1),
--      \w -> focus w >> mouseMoveWindow w),

--     -- Raise the window to the top of the stack
--     ((modMask, button2),
--        \w -> focus w >> windows W.swapMaster),

--     -- Set the window to floating mode and resize by dragging
--     ((modMask, button3),
--        \w -> focus w >> mouseResizeWindow w)
--   ]

-- ------------------------------------------------------------------------
-- Startup hook
-- ------------------------------------------------------------------------

myStartupHook = do
            spawn "source ~/.fehbg"
            -- spawn "compton --backend glx -f"
            -- spawn "$HOME/.config/polybar/start"
--             -- spawn "compton --backend glx --vsync opengl -fcCz -l -17 -t -17" --shadow-red 0.35 --shadow-green 0.92 --shadow-blue 0.93" --f
--             <+> setDefaultCursor xC_left_ptr
--             -- <+> spawn "xsetroot -solid '#F5F6F7'"
--             -- <+> spawn "xinput --set-prop 13 290 1"
--             -- <+> spawn "xinput --set-prop 13 302 0"
--             -- <+> spawn "~/bin/libinput-gestures"
--             -- <+> spawn "xrandr --output HDMI1 --off"
--             -- <+> spawn "xrandr --output HDMI1 --auto --right-of eDP1"
--             -- <+> setWMName "LG3D"
--             -- <+> spawn "firefox"

-- -----------------------------------------------------------------------
-- Log hook
-- -----------------------------------------------------------------------


myLogHook :: DC.Client -> PP
myLogHook dbus = def
    { ppOutput = dbusOutput dbus
    , ppCurrent = wrap ("%{F" ++ accent ++ "} ") " %{F-}"
    , ppVisible = wrap ("%{F" ++ blue ++ "} ") " %{F-}"
    , ppUrgent = wrap ("%{F" ++ red ++ "} ") " %{F-}"
    , ppHidden = wrap " " " "
    , ppWsSep = "  "
    , ppSep = "  |  "
    , ppTitle = \t -> ""
    }

-- Emit a DBus signal on log updates
dbusOutput :: DC.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal objectPath interfaceName memberName) {
            D.signalBody = [D.toVariant $ UTF8.decodeString str]
        }
    DC.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"

myAddSpaces :: Int -> String -> String
myAddSpaces len str = sstr ++ replicate (len - length sstr) ' '
  where
    sstr = shorten len str

-- main :: IO ()
-- main = do
--     dbus <- D.connectSession
--     -- Request access to the DBus name
--     D.requestName dbus (D.busName_ "org.xmonad.Log")
--         [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]


    -- xmonad $ def { logHook = dynamicLogWithPP (myLogHook dbus) }

-- -- Override the PP values as you would otherwise, adding colors etc depending
-- -- on  the statusbar used
-- myLogHook :: D.Client -> PP
-- myLogHook dbus = def { ppOutput = dbusOutput dbus }

-- -- Emit a DBus signal on log updates
-- dbusOutput :: D.Client -> String -> IO ()
-- dbusOutput dbus str = do
--     let signal = (D.signal objectPath interfaceName memberName) {
--             D.signalBody = [D.toVariant $ UTF8.decodeString str]
--         }
--     D.emit dbus signal
--   where
--     objectPath = D.objectPath_ "/org/xmonad/Log"
--     interfaceName = D.interfaceName_ "org.xmonad.Log"
--     memberName = D.memberName_ "Update"

-- ------------------------------------------------------------------------
-- Set defaults
-- ------------------------------------------------------------------------

defaults = def {
    terminal           = myTerminal,
--     focusFollowsMouse  = myFocusFollowsMouse,
    borderWidth        = myBorderWidth,
    modMask            = myModMask,
    workspaces         = myWorkspaces,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,
    keys               = myKeys,
--     mouseBindings      = myMouseBindings,
    layoutHook         = myLayout,
    manageHook         = myManageHook,
    startupHook        = myStartupHook
--     handleEventHook    = E.fullscreenEventHook
}

main = do
  dbus <- DC.connectSession
  xmobar defaultConfig { modMask = mod4Mask }
  xmonad $ docks defaults {
--      , startupHook = docksStartupHook <+> setWMName "LG3D"
      handleEventHook = docksEventHook,
      logHook = dynamicLogWithPP (myLogHook dbus) 
  }