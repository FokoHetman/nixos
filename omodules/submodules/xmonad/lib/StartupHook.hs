module StartupHook where

import XMonad (X)
import XMonad.Util.SpawnOnce
import XMonad.Actions.SpawnOn

import Definitions
import XMonad.Hooks.ScreenCorners

myStartupHook :: X ()
myStartupHook = do 
  --spawnOnce "lwpwlp" -- it's quite unoptimised  I'd say
  --addScreenCorner SCBottom $ namedScratchpadAction scratchpads "notes"

  spawnOnce "quickshell -c control-panel"
  spawnOnce "udiskie"
  spawnOnce "xcompmgr"
  spawnOnce "fcitx5 -d"
  spawnOnce "xhost +SI:localuser:$(whoami)"
  spawnOnce "dunst"

  spawnOn "etc" term
  spawnOn "www" $ browser <> " " <> hetmanat <> roadmapPath
  spawnOn "dev" (term ++ " " ++ multiplexer)
  spawnOn "etc" discord
  spawnOnce "quickshell -c notifications"
