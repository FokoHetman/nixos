import XMonad
import Xmobar
import XMonad.Util.NamedScratchpad

import XMonad.Util.EZConfig
import XMonad.Util.ClickableWorkspaces
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks

import qualified Data.Map as M

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.ManageHelpers

import qualified XMonad.StackSet as W
import XMonad.Actions.GridSelect

import Data.Tree
import qualified XMonad.Actions.TreeSelect as TS


import XMonad.Hooks.WorkspaceHistory
import XMonad.Actions.SpawnOn

import EventHandling
import Definitions
import TrueFullscreen
import StartupHook
import Layout (myLayout)


main :: IO ()

mySB = statusBarProp "custom-xmobar" (clickablePP bar)
main = do
  --mySB <- statusBarPipe "xmobar /etc/xmobar/xmobar.hs" (pure bar) --if this works (config), you can move to withSB
  
  --xmproc <- myXmobar

  xmonad . ewmhFullscreen . ewmh 
     . withEasySB mySB defToggleStrutsKey
    $ docks conf


conf = def 
  { modMask     = mod4Mask 
    , terminal    = term
    , manageHook  = manageDocks <+> namedScratchpadManageHook scratchpads <+> manageSpawn <+> management
    , layoutHook  = myLayout
    , startupHook = myStartupHook
    , workspaces = TS.toWorkspaces myWorkspaces
    , logHook = workspaceHistoryHook
    , handleEventHook = myEventHook
    --handleEventHook = myHandleEventHook

--    logHook = dynamicLogWithPP bar
--      { ppOutput = \x -> hPutStrLn mySB
--
--      }
  } 
  `additionalKeysP`
    [ ("<Print>", spawn "scrot --select -e 'xclip -selection clipboard -t image/png -i $f'")
      , ("M-f", spawn browser)
      --, ("M-t", spawn "vesktop")
      , ("M-r", spawn "rofi -show drun -show-icons")
      , ("M-x", restart "/run/current-system/sw/bin/xmonad" True)
      , ("M-q", spawn $ terminal conf)
      , ("M-c", kill)
      , ("M1-<Tab>", windows W.focusDown)
      , ("M-C-l", spawn "i3lock 20 pixel")
      , ("M-v", withFocused toggleFloat)
      , ("M-n", namedScratchpadAction scratchpads "notes")
      , ("M-e", namedScratchpadAction scratchpads "term")
      , ("M-C-e", namedScratchpadAction scratchpads "nixos")
      , ("M-t", spawnSelected def commonApps)
      , ("M-<Tab>", TS.treeselectWorkspace myTSConfig myWorkspaces W.greedyView)
      , ("M-z", treeselectAction myTSConfig)
      , ("M-M1-<Space>", spawn "layout-sw")
      , ("F11", toggleFullscreen)
      --("M-c", 
    ]
    where
      toggleFloat w = windows (\s -> if M.member w (W.floating s)
        then W.sink w s
        else (W.float w (W.RationalRect (1/3) (1/4) (1/2) (4/5)) s))



management :: ManageHook
management = composeAll
    [ className =? "lwpwlp" --> doLower
    , isDialog            --> doFloat
    ]



bar :: PP
bar = def 
  { ppSep             = sep " * ",
    ppTitleSanitize   = xmobarStrip,
    ppCurrent         = wrap (active "[") (active "]") . active . xmobarBorder "Top" "#8be9fd" 2,
    ppHidden          = inactive . wrap " " " ",
    ppUrgent          = red . wrap (yellow "!") (yellow "!"),
    ppOrder           = \[ws, l, _, wins] -> [ws, l, wins],
    ppExtras          = [logTitles formatFocused formatUnfocused]
  }
  where
    formatFocused   = wrap (white    "[") (white    "]") . focused . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . unfocused    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, green, yellow :: String -> String
    magenta  = xmobarColor "#B16286" ""
    blue     = xmobarColor "#458588" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#D79921" ""
    red      = xmobarColor "#CC241D" ""
    lowWhite = xmobarColor "#bbbbbb" ""
    green    = xmobarColor "#98971A" ""

    sep = magenta

    active = green
    inactive = lowWhite

    focused = active
    unfocused = yellow
