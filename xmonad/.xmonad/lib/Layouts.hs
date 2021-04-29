module Layouts where

import Colours
import Control.Category ((>>>))
import XMonad
import XMonad.Actions.Navigation2D
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Accordion
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Fullscreen
-- Spacing around/between windows
import XMonad.Layout.Gaps
import XMonad.Layout.LayoutModifier
import XMonad.Layout.MultiToggle
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.Reflect
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.WindowNavigation

myTabTheme :: String -> Theme
myTabTheme accent =
  def
    { fontName = "xft:Alte DIN 1451 Mittelschrift:style=Regular:size=9",
      activeColor = accent,
      activeTextColor = base03,
      activeBorderColor = accent,
      inactiveColor = background,
      inactiveTextColor = backgroundText,
      inactiveBorderColor = background,
      decoHeight = 20
    }

myTopBarTheme :: String -> Theme
myTopBarTheme accent =
  def
    { fontName = "xft:Alte DIN 1451 Mittelschrift:style=Regular:size=9",
      inactiveBorderColor = background,
      inactiveColor = background,
      inactiveTextColor = background,
      activeBorderColor = accent,
      activeColor = accent,
      activeTextColor = accent,
      urgentBorderColor = red,
      urgentTextColor = yellow,
      decoHeight = 3
    }

addTopBar accent = noFrillsDeco shrinkText (myTopBarTheme accent)

-- Apply spacing between / around windows
spacingModifier :: LayoutClass l a => l a -> ModifiedLayout Gaps (ModifiedLayout Spacing l) a
spacingModifier =
  spacingRaw smartBorder screenBorder screenBorderEnabled windowBorder windowBorderEnabled
    >>> gaps [(L, 5), (U, 5), (R, 5), (D, 5)]
  where
    windowBorder = Border 10 10 10 10
    windowBorderEnabled = True

    -- Not used as we have Gaps
    smartBorder = False
    screenBorder = Border 10 10 10 10
    screenBorderEnabled = False

fullscreenName :: String
fullscreenName = "\xf2d0"

fullscreen =
  named fullscreenName $
    avoidStrutsOn [] $
      noBorders $
        fullscreenFull $
          Full

threeColName :: String
threeColName = "\xf279"

threeCol = \accent ->
  named threeColName $
    addTopBar accent $
      avoidStruts $
        windowNavigation $
          addTabs shrinkText (myTabTheme accent) $
            subLayout [0, 1] (Simplest ||| Accordion) $
              mkToggle (single REFLECTX) $
                spacingModifier $
                  ThreeColMid 1 (1 / 20) (1 / 2)

bspName :: String
bspName = "B"

bsp = \accent ->
  named bspName $
    addTopBar accent $
      avoidStruts $
        windowNavigation $
          addTabs shrinkText (myTabTheme accent) $
            subLayout [0, 1] (Simplest ||| Accordion)
            -- Use reflections to ensure that new windows appear on RIGHT and BELOW
            -- preferentially. The defaults are weird (and the opposite of i3), at least
            -- to an English user.
            $
              reflectHoriz $
                reflectVert $
                  mkToggle (single REFLECTX) $
                    spacingModifier $
                      emptyBSP

myLayout accent = (bsp accent ||| fullscreen)

my2DNavigation :: XConfig a -> XConfig a
my2DNavigation =
  withNavigation2DConfig
    def
      { layoutNavigation =
          [ (threeColName, centerNavigation),
            (bspName, centerNavigation),
            (fullscreenName, centerNavigation)
          ],
        unmappedWindowRect = [(fullscreenName, singleWindowRect)]
      }
