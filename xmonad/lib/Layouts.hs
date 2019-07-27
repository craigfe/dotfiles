module Layouts where

import           XMonad
import           XMonad.Actions.Navigation2D
import           XMonad.Layout.NoFrillsDecoration
import           XMonad.Layout.Accordion
import           XMonad.Layout.BinarySpacePartition
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.Named
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Reflect
import           XMonad.Layout.Simplest
import           XMonad.Layout.Spacing
import           XMonad.Layout.SubLayouts
import           XMonad.Layout.Tabbed
import           XMonad.Layout.ThreeColumns
import           XMonad.Layout.WindowNavigation
import           XMonad.Hooks.ManageDocks

import Colours

myTabTheme :: String -> Theme
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

myTopBarTheme :: String -> Theme
myTopBarTheme accent = def
    { fontName              = "xft:Alte DIN 1451 Mittelschrift:style=Regular:size=9"
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

spacingModifier :: l a -> ModifiedLayout Spacing l a
spacingModifier = spacingRaw smartBorder screenBorder screenBorderEnabled windowBorder windowBorderEnabled
  where
    smartBorder = True
    screenBorder = Border 10 10 10 10
    screenBorderEnabled = False
    windowBorder = Border 10 10 10 10
    windowBorderEnabled = True

fullscreenName :: String
fullscreenName = "\xf2d0"

fullscreen = named fullscreenName
  $ avoidStrutsOn []
  $ noBorders
  $ fullscreenFull
  $ Full

threeColName :: String
threeColName = "\xf279"

threeCol = \accent ->
  named threeColName
  $ addTopBar accent
  $ avoidStruts
  $ windowNavigation
  $ addTabs shrinkText (myTabTheme accent)
  $ subLayout [0,1] (Simplest ||| Accordion)
  $ mkToggle (single REFLECTX)
  $ spacingModifier
  $ ThreeColMid 1 (1/20) (1/2)

bspName :: String
bspName = "B"

bsp = \accent ->
  named bspName
  $ addTopBar accent
  $ avoidStruts
  $ windowNavigation
  $ addTabs shrinkText (myTabTheme accent)
  $ subLayout [0,1] (Simplest ||| Accordion)
  $ mkToggle (single REFLECTX)
  $ spacingModifier
  $ emptyBSP

myLayout accent = (bsp accent ||| fullscreen ||| threeCol accent)

my2DNavigation :: XConfig a -> XConfig a
my2DNavigation = withNavigation2DConfig def {
          layoutNavigation   = [ (threeColName, centerNavigation)
                               , (bspName, centerNavigation)
                               , (fullscreenName, centerNavigation) ]
        , unmappedWindowRect = [ (fullscreenName, singleWindowRect) ]
        }
