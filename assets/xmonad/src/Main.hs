import XMonad

import XMonad.Util.NamedScratchpad
import XMonad.ManageHook

import XMonad.Util.EZConfig 
import XMonad.Util.Ungrab
import XMonad.Util.ClickableWorkspaces
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Actions.GroupNavigation
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run (spawnPipe, hPutStrLn)

import qualified Data.Map as M

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.ManageHelpers

import XMonad.Hooks.ManageDocks (avoidStruts, docks)

import Data.Semigroup
import XMonad.Hooks.DynamicProperty

import qualified XMonad.StackSet as W

import XMonad.Actions.GridSelect

import Data.Tree
import qualified XMonad.Actions.TreeSelect as TS
import XMonad.Hooks.WorkspaceHistory


main :: IO ()

mySB = statusBarProp "xmobar" (clickablePP bar)
main = do
  --mySB <- statusBarPipe "xmobar /etc/xmobar/xmobar.hs" (pure bar) --if this works (config), you can move to withSB
  xmonad . ewmhFullscreen . ewmh 
    . withEasySB mySB defToggleStrutsKey
    $ docks $ conf

term :: String
term = "kitty"



myWorkspaces :: Forest String
myWorkspaces = [ Node "etc" []
               , Node "www" []
               , Node "dev" -- for all your programming needs
                    [
                      Node "tmp"    [] -- documentation
                    ]
               , Node "art" []
               , Node "misc" 
                    [
                        Node "1" []
                      , Node "2" []
                      , Node "3" []
                    ]
               ]


treeselectAction :: TS.TSConfig (X ()) -> X ()
treeselectAction a = TS.treeselectAction a
   [ Node (TS.TSNode "Hello"    "displays hello"      (spawn "xmessage hello!")) []
   , Node (TS.TSNode "Shutdown" "Poweroff the system" (spawn "shutdown")) []
   , Node (TS.TSNode "Brightness" "Sets screen brightness using xbacklight" (return ()))
       [ Node (TS.TSNode "Bright" "FULL POWER!!"            (spawn "xbacklight -set 100")) []
       , Node (TS.TSNode "Normal" "Normal Brightness (50%)" (spawn "xbacklight -set 50"))  []
       , Node (TS.TSNode "Dim"    "Quite dark"              (spawn "xbacklight -set 10"))  []
       ]
   ]

myTSConfig = TS.TSConfig { TS.ts_hidechildren = True
                           , TS.ts_background   = 0x282828
                           , TS.ts_font         = "xft:Sans-16"
                           , TS.ts_node         = (0xff000000, 0xff50d0db)
                           , TS.ts_nodealt      = (0xff000000, 0xff10b8d6)
                           , TS.ts_highlight    = (0xffffffff, 0xffff0000)
                           , TS.ts_extra        = 0xff000000
                           , TS.ts_node_width   = 200
                           , TS.ts_node_height  = 30
                           , TS.ts_originX      = 0
                           , TS.ts_originY      = 0
                           , TS.ts_indent       = 80
                           , TS.ts_navigate     = TS.defaultNavigation
                           }


commonApps = ["kitty","blender","krita","drawio","godot4.4","prismlauncher", "obs", "zathura", "nix run nixpkgs#legcord"]


scratchpads = [
    NS "term" (term ++ " --class term") findTerm manageNotes,
    NS "notes" spawnNotes findNotes manageNotes,
    NS "nixos" spawnNixosEdit findNixosEdit manageNotes
              ] 
              where 
              role = stringProperty "WM_WINDOW_ROLE"
              
              findTerm = resource =? "term"
              findNotes = resource =? "notepad"
              findNixosEdit = resource =? "nixos"

              spawnNotes = term ++ " --class notepad nvim -c \":Neorg workspace notes\""
              spawnNixosEdit = term ++ " --class nixos /etc/nixos"
              manageNotes = customFloating $ W.RationalRect (1/12) (1/12) (5/6) (5/6)


conf = def 
  { modMask     = mod4Mask 
    , terminal    = term
    , manageHook  = manageDocks <+> namedScratchpadManageHook scratchpads <+> management
    , layoutHook  = avoidStruts $ layoutHook def
    , startupHook = startup
    , workspaces = TS.toWorkspaces myWorkspaces
    , logHook = workspaceHistoryHook

    --handleEventHook = myHandleEventHook

--    logHook = dynamicLogWithPP bar
--      { ppOutput = \x -> hPutStrLn mySB
--
--      }
  } 
  `additionalKeysP`
    [ ("<Print>", spawn "scrot --select -e 'xclip -selection clipboard -t image/png -i $f'")
      , ("M-f", spawn "librewolf")
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
      --("M-c", 
    ]
    where
      toggleFloat w = windows (\s -> if M.member w (W.floating s)
        then W.sink w s
        else (W.float w (W.RationalRect (1/3) (1/4) (1/2) (4/5)) s))

startup :: X ()
startup = do 
  --spawnOnce "lwpwlp" -- it's quite unoptimised  I'd say
  spawnOnce "udiskie -c \"$HOME/.config/udiskie/config.yml\""
  spawnOnce "xcompmgr"
  spawnOnce "xhost +SI:localuser:$(whoami)"




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
