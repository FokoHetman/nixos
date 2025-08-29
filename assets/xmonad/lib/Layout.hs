module Layout where


import XMonad.Hooks.ScreenCorners
import XMonad (XConfig(layoutHook),def)
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances


myLayout = screenCornerLayoutHook $ mkToggle (NOBORDERS ?? NBFULL ?? EOT) $ avoidStruts $ layoutHook def

