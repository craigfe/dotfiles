{-# LANGUAGE LambdaCase #-}

module Bindings where

import Config
import Control.Arrow
import Data.Function
import Data.Maybe
import Projects
import System.Exit
import System.FilePath.Posix
import System.IO
import System.Process
import Text.Printf
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.Navigation2D
import XMonad.Actions.WorkspaceNames
import XMonad.Hooks.ManageDocks
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Gaps
import XMonad.Layout.MultiToggle
import XMonad.Layout.Reflect
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare

reverseDir :: Direction2D -> Direction2D
reverseDir L = R
reverseDir R = L
reverseDir U = D
reverseDir D = U

myWorkspaces = map show ([1 .. 9] ++ [0]) ++ projectsWorkspaces

compareToCurrent :: X (WindowSpace -> Ordering)
compareToCurrent =
  do
    comp <- getWsCompare
    ws <- gets windowset
    let cur = W.workspace (W.current ws)
    return $ comp (W.tag cur) . W.tag

greaterNonEmptyWs :: X (WindowSpace -> Bool)
greaterNonEmptyWs =
  do
    comp <- compareToCurrent
    return (\w -> comp w == LT && isJust (W.stack w))

lessNonEmptyWs :: X (WindowSpace -> Bool)
lessNonEmptyWs =
  do
    comp <- compareToCurrent
    return (\w -> comp w == GT && isJust (W.stack w))

moveToNextNonEmptyNoWrap = moveTo Next (WSIs greaterNonEmptyWs)

moveToPrevNonEmptyNoWrap = moveTo Prev (WSIs lessNonEmptyWs)

incGap :: Int -> X ()
incGap n = sendMessage $ ModifyGaps $ map (\(d, s) -> (d, s + n))

decGap :: Int -> X ()
decGap n = sendMessage $ ModifyGaps $ map (\(d, s) -> (d, s - n))

myModMask :: KeyMask
myModMask = mod4Mask

spawnLocal :: X ()
spawnLocal = spawn $ printf "alacritty --command $SHELL -i -c 'tmux'"

spawnRemote :: X ()
spawnRemote = spawn $ printf "alacritty --command $SHELL -i -c 'ssh %s'" myRemoteMachine

myKeys = \c ->
  mkKeymap c $
    [ ("M-<Return>", spawnLocal),
      ("M-S-<Return>", spawnRemote),
      ("M-<Backspace>", spawn mySystemMenu),
      ("M-S-<Backspace>", spawn "systemctl suspend"),
      -- , ("M-<Space>", unicodePrompt "" xpConfig)
      ("<XF86MonBrightnessUp>", spawn "~/.scripts/backlight --inc 5"),
      ("M-<Space>", spawn myUnicodePrompt),
      ("M-S-<Space>", setLayout $ XMonad.layoutHook c),
      ("M-<Tab>", sendMessage $ Toggle REFLECTX),
      ("M-S-<Tab>", moveToNextNonEmptyNoWrap),
      ("<Print>", spawn mySelectScreenshot),
      ("S-<Print>", spawn myScreenshot),
      -- Main keys
      ("M--", incWindowSpacing 10 >> incGap 5),
      ("M-S--", incWindowSpacing 2 >> incGap 1),
      ("M-=", decWindowSpacing 10 >> decGap 5),
      ("M-S-=", decWindowSpacing 2 >> decGap 1),
      ("M-q", kill),
      ("M-w", spawn myWebBrowser),
      ("M-S-w", spawn myPrivateWebBrowser),
      ("M-e", spawn (myEditor ++ " ~")),
      ("M-r", spawn "alacritty -e ranger"),
      ("M-t", withFocused $ windows . W.sink),
      ("M-M1-u", withFocused (sendMessage . UnMerge)),
      -- These bindings are reflected horizontally and vertically from what a
      -- Vim-user would expect. A better solution would be to dynamically check
      -- whether the toggle is active, but the `XMonad.Layout.Reflect` layout
      -- does not make this easy.
      ("M-y", (sendMessage Shrink) >> (sendMessage $ ExpandTowards $ reverseDir L)),
      ("M-u", sendMessage $ ExpandTowards $ reverseDir D),
      ("M-i", sendMessage $ ExpandTowards $ reverseDir U),
      ("M-o", (sendMessage Expand) >> (sendMessage $ ExpandTowards $ reverseDir R)),
      ("M-[", onGroup W.focusUp'),
      ("M-]", onGroup W.focusDown'),
      ("M-S-o", moveTo Next EmptyWS),
      -- , ("M-p", toggleOrView projectWorkspace)
      -- , ("M-S-p", windows $ W.shift projectWorkspace)

      ("M-a", toggleOrView socialWorkspace),
      ("M-S-a", windows $ W.shift socialWorkspace),
      ("M-s", toggleOrView musicWorkspace),
      ("M-S-s", spotifyPause),
      ("M-d", spawn myLauncher),
      -- , ("M-S-d", windows $ W.shift dissWorkspace)

      ("M-f", sendMessage Rotate),
      ("M-S-f", sendMessage Swap),
      ("M-g", sendMessage ToggleStruts),
      ("M-;", nextScreen),
      ("M-S-;", shiftNextScreen),
      ("M-'", setWSName ()),
      -- , ("M-#", )
      ("M-n", sendMessage NextLayout),
      ("M-S-n", toSubl NextLayout),
      ("M-M1-m", withFocused (sendMessage . MergeAll)),
      ("M-z", spawn myPdfViewer),
      ("M-x", spawn myLock),
      ("M-c", spawn myCI),
      ("M-,", sendMessage (IncMasterN 1)),
      ("M-.", sendMessage (IncMasterN (-1))),
      -- Function keys
      ("M-<F2>", restart "xmonad" True),
      ("M-S-<F2>", io exitSuccess),
      -- Multimedia keys
      ("<XF86AudioMute>", spawn $ "pactl set-sink-mute " ++ mySink ++ " toggle"),
      ("<XF86AudioLowerVolume>", spawn $ "pactl set-sink-mute " ++ mySink ++ " false; pactl set-sink-volume " ++ mySink ++ " -5%"),
      ("<XF86AudioRaiseVolume>", spawn $ "pactl set-sink-mute " ++ mySink ++ " false; pactl set-sink-volume " ++ mySink ++ " +5%"),
      ("<XF86MonBrightnessDown>", spawn "~/.scripts/backlight --dec 5"),
      ("<XF86MonBrightnessUp>", spawn "~/.scripts/backlight --inc 5"),
      ("<XF86AudioPrev>", spawn "playerctl previous"),
      ("<XF86AudioPlay>", spawn "playerctl play-pause"),
      ("<XF86AudioNext>", spawn "playerctl next")
      -- , ("M-M1-1", setLayout fullscreen)
      -- , ("M-M1-2", sendMessage $ JumpToLayout bspName)
    ]
      -- mod-[0..9]      , Switch to workspace N
      -- mod-shift-[0..9], Move client to workspace N
      ++ [ (m ++ k, windows $ f w)
           | (w, k) <- zip (XMonad.workspaces c) myWorkspaces,
             (m, f) <- [("M-", W.view), ("M-S-", W.shift)]
         ]
      -- mod-[h,j,k,l],       Switch focused window
      -- mod-shift-[h,j,k,l], Move focused window
      -- mod-alt-[h,j,k,l],   Push focused window into sublayout
      ++ zipM "M-" dirKeys dirs (\dir -> windowGo dir False)
      ++ zipM "M-S-" dirKeys dirs (\dir -> windowSwap dir False)
      ++ zipM "M-M1-" dirKeys dirs (sendMessage . pushGroup)
  where
    setWSName () =
      runProcessWithInput (myHome ++ "/.config/rofi/menu/print") [] ""
        >>= setCurrentWorkspaceName

    spotifyPause = spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause >> /dev/null"
    zipM mod keys actions f = zipWith (\key action -> (mod ++ key, f action)) keys actions
    zipM' mod keys actions f b = zipWith (\key action -> (mod ++ key, f action b)) keys actions
    notSP = (return $ ("SP" /=) . W.tag) :: X (WindowSpace -> Bool)
    dirKeys = ["j", "k", "h", "l"]
    dirs = [D, U, L, R]
