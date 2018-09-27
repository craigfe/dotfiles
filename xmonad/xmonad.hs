import qualified Codec.Binary.UTF8.String            as UTF8
import           Data.Maybe
import           Graphics.X11.ExtraTypes
import           System.Exit
import           System.IO
import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Actions.SpawnOn
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.Accordion
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.Gaps
import           XMonad.Layout.IndependentScreens
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.Named
import           XMonad.Layout.NoBorders
import           XMonad.Layout.NoFrillsDecoration
import           XMonad.Layout.PerScreen
import           XMonad.Layout.Reflect
import           XMonad.Layout.ShowWName
import           XMonad.Layout.Simplest
import           XMonad.Layout.Spacing
import           XMonad.Layout.SubLayouts
import           XMonad.Layout.Tabbed
import           XMonad.Layout.WindowNavigation
import           XMonad.Util.Cursor
import           XMonad.Util.EZConfig
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run                     (spawnPipe)
import           XMonad.Util.WorkspaceCompare

-- import qualified XMonad.Hooks.EwmhDesktops as E
import qualified Data.Map                            as M
import qualified XMonad.StackSet                     as W

import qualified DBus                                as D
import qualified DBus.Client                         as DC

-- ------------------------------------------------------------------------
-- Application Names
-- ------------------------------------------------------------------------

myTerminal = "alacritty"
myScreensaver = "/usr/bin/gnome-screensaver-command --lock"
mySelectScreenshot = "screenshot_clipboard"
myWebBrowser = "google-chrome --force-device-scale-factor=1.25"
myScreenshot = "screenshot"
myLauncher = "/home/craigfe/repos/config/rofi/menu/run"
mySystemMenu = "/home/craigfe/repos/config/rofi/menu/system"
myLock = "i3lock -c 000000"
mySink = "alsa_output.pci-0000_00_1f.3.analog-stereo"

-- ------------------------------------------------------------------------
-- Workspaces
-- ------------------------------------------------------------------------

myWorkspaces = map show ([1..9] ++ [0])

-- ------------------------------------------------------------------------
-- Scratchpads
-- ------------------------------------------------------------------------

spotifyCommand = "spotify"
messengerCommand = "google-chrome --app=https://www.messenger.com/"
youtubeCommand = "google-chrome --app=https://www.youtube.com/"

isSpotify   = (className =? "Spotify")
isMessenger = (appName =? "www.messenger.com")
isYoutube = (appName =? "www.youtube.com")

scratchpads =
  [ NS "spotify" spotifyCommand isSpotify (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
  , NS "messenger" messengerCommand isMessenger (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
  , NS "youtube" youtubeCommand isYoutube (customFloating $ W.RationalRect (31/48) (1/24) (8/24) (9/24))
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

background = "#121212"
backgroundText = "#a5a5a5"

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

myTabTheme accent = def
    { fontName              = "xft:Alte DIN 1451 Mittelschrift:style=Regular:size=11"
    , activeColor           = accent
    , activeTextColor       = base03
    , activeBorderColor     = accent
    , inactiveColor         = background
    , inactiveTextColor     = backgroundText
    , inactiveBorderColor   = background
	, decoHeight            = 30
    }

myTopBarTheme accent = def
    { fontName              = "mono 10"
    , inactiveBorderColor   = background
    , inactiveColor         = background
    , inactiveTextColor     = background
    , activeBorderColor     = accent
    , activeColor           = accent
    , activeTextColor       = accent
    , urgentBorderColor     = red
    , urgentTextColor       = yellow
    , decoHeight            = 7
    }

addTopBar accent = noFrillsDeco shrinkText (myTopBarTheme accent)

tiled = \accent -> named "Tiled"
	$ avoidStruts
	$ addTopBar accent
	$ lessBorders OnlyFloat
	$ smartSpacing gap
	$ gaps [(U,gap), (D,gap), (R,gap), (L,gap)]
	$ Tall nmaster delta ratio
	where
		gap = 5
		nmaster = 1
		ratio = 1/2
		delta = 3/100

{-flex = \accent -> named "Flex"-}
	{-$ avoidStruts-}
	{-$ addTopBar accent-}
	{-$ windowNavigation-}
	{-$ addTabs shrinkText (myTabTheme accent)-}
	{-$ subLayout [] (Simplest ||| Accordion)-}
	{-$ Simplest-}

fullscreen = named "Fullscreen"
	$ avoidStruts
	$ noBorders
	$ fullscreenFull
	$ Full

accordion = \accent -> named "Accordion"
	$ avoidStruts
	$ addTopBar accent
	$ noBorders
	$ Accordion

tabs = \accent -> named "Tabs"
	$ addTopBar accent
	$ avoidStruts
	$ addTabs shrinkText (myTabTheme accent)
	$ Simplest

myLayout accent = mirrorToggle
	$ reflectToggle
	$ tiled accent ||| fullscreen ||| accordion accent ||| tabs accent
	where
		mirrorToggle  = mkToggle (single MIRROR)
		reflectToggle = mkToggle (single REFLECTX)

-- ------------------------------------------------------------------------
-- Colors and borders
-- ------------------------------------------------------------------------

myNormalBorderColor = "#2F343F"
myBorderWidth = 0

-- ------------------------------------------------------------------------
-- Key bindings
-- ------------------------------------------------------------------------

myModMask = mod4Mask
myKeys = \c -> mkKeymap c $
  [ ("M-<Return>", spawn $ XMonad.terminal c)
  , ("M-S-<Return>", windows W.swapMaster)
  , ("M-S-<Space>", setLayout $ XMonad.layoutHook c)
  , ("M-C-l", spawn myScreensaver)
  , ("M-a", namedScratchpadAction scratchpads "messenger")
  , ("M-d", spawn myLauncher)
  , ("M-<Backspace>", spawn mySystemMenu)
  , ("M-p", spawn mySelectScreenshot)
  , ("M-S-p", spawn myScreenshot)
  , ("M-<Tab>", windows W.focusDown)
  , ("M-S-<Tab>", moveToNextNonEmptyNoWrap)
  , ("M-e", moveTo Next EmptyWS)
  , ("M-q", kill)
  , ("M-w", spawn myWebBrowser)
  , ("M-S-w", spawn (myWebBrowser ++ " --incognito"))
  , ("M-r", spawn "alacritty -e ranger")
  , ("M-t", withFocused $ windows . W.sink)
  , ("M-y", namedScratchpadAction scratchpads "youtube")
  , ("M-u", windows W.swapDown)
  , ("M-i", windows W.swapUp)
  , ("M-[", sendMessage $ IncGap 5 R)
  , ("M-]", sendMessage $ DecGap 5 R)
  , ("M-j", windows W.focusDown)
  , ("M-k", windows W.focusUp)
  , ("M-n", sendMessage NextLayout)
  , ("M-S-n", toSubl NextLayout)
  , ("M-m", namedScratchpadAction scratchpads "spotify")
  , ("M-S-m", spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause >> /dev/null")
  , ("M-S-j", windows W.swapDown)
  , ("M-S-k", windows W.swapUp)
  , ("M-h", sendMessage Shrink)
  , ("M-l", sendMessage Expand)
  , ("M-;", nextScreen)
  , ("M-S-;", shiftNextScreen)
  , ("M-x", spawn myLock)
  , ("M-,", sendMessage (IncMasterN 1))
  , ("M-.", sendMessage (IncMasterN (-1)))
  , ("M-/", sendMessage $ Toggle MIRROR)
  , ("M-S-<F2>", io exitSuccess)
  , ("M-<F2>", restart "xmonad" True)

     -- Multimedia keys
  , ("<XF86AudioMute>", spawn $  "pactl set-sink-mute " ++ mySink ++ " toggle")
  , ("<XF86AudioLowerVolume>", spawn $  "pactl set-sink-mute " ++ mySink ++ " false; pactl set-sink-volume " ++ mySink ++ " -5%")
  , ("<XF86AudioRaiseVolume>", spawn $  "pactl set-sink-mute " ++ mySink ++ " false; pactl set-sink-volume " ++ mySink ++ " +5%")
  , ("<XF86MonBrightnessDown>", spawn "~/.scripts/backlight --dec 5")
  , ("<XF86MonBrightnessUp>", spawn "~/.scripts/backlight --inc 5")
  , ("<XF86AudioPrev>", spawn "playerctl previous")
  , ("<XF86AudioPlay>", spawn "playerctl play-pause")
  , ("<XF86AudioNext>", spawn "playerctl next")
  ]
  ++
  -- mod-[0..9], Switch to workspace N
  -- mod-shift-[0..9], Move client to workspace N
  [(m ++ k, windows $ f w)
  	| (w, k) <- zip (XMonad.workspaces c) myWorkspaces
  	, (m, f) <- [("M-",W.view), ("M-S-",W.shift)]]

	++ zipM "M-C-" dirKeys dirs (sendMessage . pullGroup)
	where

	zipM  mod keys actions f   = zipWith (\key action -> (mod ++ key, f action)) keys actions
	zipM' mod keys actions f b = zipWith (\key action -> (mod ++ key, f action b)) keys actions

	notSP = (return $ ("SP" /=) . W.tag) :: X (WindowSpace -> Bool)
	dirKeys = ["j","k","h","l"]
	dirs = [D,U,L,R]


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

-- toggleHDMI = do
--   count <- countScreens
--   spawn $ "echo " ++ show count ++ " >> ~/test.txt"
--   if count > 1
--     then spawn "xrandr --output HDMI1 --off"
--     else spawn "sleep 0.3; xrandr --output HDMI1 --auto --right-of eDP1"

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
			<+> spawn "~/repos/config/polybar/start"
            -- spawn "compton --backend glx -f"
            -- spawn "$HOME/.config/polybar/start" -- spawn "compton --backend glx --vsync opengl -fcCz -l -17 -t -17" --shadow-red 0.35 --shadow-green 0.92 --shadow-blue 0.93" --f
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

myLogHook :: String -> DC.Client -> PP
myLogHook accent dbus = def
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

defaults accent = def {
    terminal           = myTerminal,
--     focusFollowsMouse  = myFocusFollowsMouse,
    borderWidth        = myBorderWidth,
    modMask            = myModMask,
    workspaces         = myWorkspaces,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = accent,
    keys               = myKeys,
--     mouseBindings      = myMouseBindings,
    layoutHook         = myLayout accent,
    manageHook         = myManageHook,
    startupHook        = myStartupHook
--     handleEventHook    = E.fullscreenEventHook
}

main = do
	accentFile <- readFile "/home/craigfe/repos/config/colours/out/theme"
	dbus <- DC.connectSession
	xmobar defaultConfig { modMask = mod4Mask }
	xmonad $ docks (defaults (init accentFile)) {
		workspaces = myWorkspaces,
		handleEventHook = docksEventHook,
		logHook = dynamicLogWithPP (myLogHook (init accentFile) dbus)
	}
