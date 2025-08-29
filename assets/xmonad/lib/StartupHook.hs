module StartupHook where

import XMonad (X)
import XMonad.Util.SpawnOnce
import XMonad.Actions.SpawnOn

import Definitions
import XMonad.Hooks.ScreenCorners
import XMonad.Util.NamedScratchpad (namedScratchpadAction)


myStartupHook :: X ()
myStartupHook = do 
  --spawnOnce "lwpwlp" -- it's quite unoptimised  I'd say
  addScreenCorner SCBottom $ namedScratchpadAction scratchpads "notes"
  spawnOnce "udiskie -c \"$HOME/.config/udiskie/config.yml\""
  spawnOnce "xcompmgr"
  spawnOnce "xhost +SI:localuser:$(whoami)"

  spawnOn "etc" term
  spawnOn "www" browser
  spawnOn "dev" (term ++ " " ++ multiplexer)
  spawnOn "etc" discord
