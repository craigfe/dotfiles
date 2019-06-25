module Layouts where

import           XMonad
import           XMonad.Layout.NoFrillsDecoration
import           XMonad.Layout.Accordion
import           XMonad.Layout.BinarySpacePartition
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.Gaps
import           XMonad.Layout.IndependentScreens
import           XMonad.Layout.LayoutCombinators     (JumpToLayout (..))
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.Named
import           XMonad.Layout.NoBorders
import           XMonad.Layout.NoFrillsDecoration
import           XMonad.Layout.PerScreen
import           XMonad.Layout.Reflect
import           XMonad.Layout.Reflect
import           XMonad.Layout.ShowWName
import           XMonad.Layout.Simplest
import           XMonad.Layout.Spacing
import           XMonad.Layout.SubLayouts
import           XMonad.Layout.Tabbed
import           XMonad.Layout.ThreeColumns
import           XMonad.Layout.WindowNavigation
import           XMonad.Hooks.ManageDocks

import Colours

myTabTheme accent = def
    { fontName              = "xft:Alte DIN 1451 Mittelschrift:style=Regular:size=9"
    , activeColor           = accent
    , activeTextColor       = base03
    , activeBorderColor     = accent
    , inactiveColor         = background
    , inactiveTextColor     = backgroundText
    , inactiveBorderColor   = background
    , decoHeight            = 20
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
    , decoHeight            = 8
    }

addTopBar accent = noFrillsDeco shrinkText (myTopBarTheme accent)

fullscreenName = "\xf2d0"
fullscreen = named fullscreenName
  $ avoidStrutsOn []
  $ noBorders
  $ fullscreenFull
  $ Full

threeColName = "\xf279"
threeCol = \accent ->
  named threeColName
  $ addTopBar accent
  $ avoidStruts
  $ windowNavigation
  $ addTabs shrinkText (myTabTheme accent)
  $ subLayout [0,1] (Simplest ||| Accordion)
  $ mkToggle (single REFLECTX)
  $ smartSpacing gap
  $ gaps [(U,gap), (D,gap), (R,gap), (L,gap)]
  $ tcm
  where
    gap = 5
    tcm = ThreeColMid 1 (1/20) (1/2)

bspName = "B"
bsp = \accent -> named bspName
  $ addTopBar accent
  $ avoidStruts
  $ windowNavigation
  $ addTabs shrinkText (myTabTheme accent)
  $ subLayout [0,1] (Simplest ||| Accordion)
  $ mkToggle (single REFLECTX)
  $ smartSpacing gap
  $ gaps [(U,gap), (D,gap), (R,gap), (L,gap)]
  $ emptyBSP
  where
    gap = 5

myLayout accent = mirrorToggle
  (fullscreen ||| threeCol accent ||| bsp accent)
  {-ifWider smallMonResWidth wideLayouts standardLayouts-}
  where
    {-wideLayouts = (fullscreen ||| threeCol accent)-}
    {-standardLayouts = (fullscreen ||| tiled accent ||| accordion accent ||| tabs accent)-}
    mirrorToggle = mkToggle (single MIRROR)
    {-smallMonResWidth = 1920-}
