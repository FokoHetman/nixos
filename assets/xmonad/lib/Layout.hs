module Layout where


import XMonad.Hooks.ScreenCorners
import XMonad
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances

import Definitions (myBorderWidth)
import XMonad.Layout.NoBorders (withBorder, noBorders, smartBorders)
import XMonad.Layout.Decoration (Theme(fontName, activeColor, inactiveColor, activeBorderColor, inactiveBorderColor, activeTextColor, inactiveTextColor), shrinkText)
import XMonad.Layout.Tabbed (tabbed, addTabs)
import XMonad.Layout.Renamed (renamed, Rename (Replace))
import XMonad.Layout.ResizableTile (ResizableTall(ResizableTall))
import XMonad.Layout.LimitWindows (limitWindows)
import XMonad.Layout.SubLayouts (subLayout)
import XMonad.Layout.Simplest (Simplest(Simplest))
import XMonad.Layout.Spacing (Spacing, Border (Border), spacingRaw)
import qualified XMonad.Layout.Decoration as XMonad.Layout.LayoutModifier
import XMonad.Layout.Grid
import XMonad.Layout.BoringWindows (boringWindows)
import XMonad.Layout.Magnifier (magnifiercz)

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

spacedTall = renamed [Replace "gtall"] 
--           $ smartBorders
           $ mySpacing 4 
           $ ResizableTall 1 (3/100) (1/2) []
tabs     = renamed [Replace "tabs"] $ tabbed shrinkText myTabTheme

-- setting colors for tabs layout and tabs sublayout.
myTabTheme = def 
-- { fontName            = font
--                 , activeColor         = "#46d9ff"
--                 , inactiveColor       = "#313846"
--                 , activeBorderColor   = "#46d9ff"
--                 , inactiveBorderColor = "#282c34"
--                 , activeTextColor     = "#282c34"
--                 , inactiveTextColor   = "#d0d0d0"
--                 }


myLayout = screenCornerLayoutHook $ mkToggle (NOBORDERS ?? NBFULL ?? EOT) $ avoidStruts myDefaultLayout
  where
    myDefaultLayout = boringWindows $ withBorder myBorderWidth spacedTall ||| Tall 1 (3/100) (1/2) ||| noBorders tabs
