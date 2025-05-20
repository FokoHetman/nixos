import XMonad

import XMonad.Util.EZConfig 
import XMonad.Util.Ungrab
import XMonad.Util.ClickableWorkspaces
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Actions.GroupNavigation
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run (spawnPipe, hPutStrLn)

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.ManageHelpers

import XMonad.Hooks.ManageDocks (avoidStruts, docks)

import qualified XMonad.StackSet as W

main :: IO ()

mySB = statusBarProp "xmobar" (clickablePP bar)
main = do
  --mySB <- statusBarPipe "xmobar /etc/xmobar/xmobar.hs" (pure bar) --if this works (config), you can move to withSB
  xmonad . ewmhFullscreen . ewmh 
    . withEasySB mySB defToggleStrutsKey
    $ docks $ conf


conf = def 
  { modMask     = mod4Mask, 
    terminal    = "kitty",
    manageHook  = manageDocks <+> management,
    layoutHook  = avoidStruts $ layoutHook def,
    startupHook = startup
--    logHook = dynamicLogWithPP bar
--      { ppOutput = \x -> hPutStrLn mySB
--
--      }
  } 
  `additionalKeysP`
    [ ("<Print>", spawn "scrot --select -e 'xclip -selection clipboard -t image/png -i $f'"),
      ("M-f", spawn "librewolf"),
      ("M-t", spawn "vesktop"),
      ("M-r", spawn "rofi -show drun -show-icons"),
      ("M-x", restart "/run/current-system/sw/bin/xmonad" True),
      ("M-q", spawn $ terminal conf),
      ("M-c", kill),
      ("M1-<Tab>", windows W.focusDown),
      ("M-C-l", spawn "i3lock 20 pixel")
      --("M-c", 
    ]

startup :: X ()
startup = do 
  spawnOnce "lwpwlp"
  spawnOnce "udiskie -c \"$HOME/.config/udiskie/config.yml\""
  spawnOnce "xcompmgr"

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
