import XMonad

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Actions.GroupNavigation
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe, hPutStrLn)

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Util.Loggers
import XMonad.Hooks.ManageHelpers

import XMonad.Hooks.ManageDocks (avoidStruts, docks)

import qualified XMonad.StackSet as W

main :: IO ()

main = do 
  mySB <- statusBarPipe "xmobar /etc/xmobar/xmobar.hs" (pure bar) --if this works (config), you can move to withSB
  xmonad . ewmhFullscreen . ewmh 
    . withSB mySB
    $ docks $ conf


conf = def 
  { modMask     = mod4Mask, 
    terminal    = "kitty",
    manageHook  = manageDocks <+> management,
    layoutHook  = avoidStruts $ layoutHook def
  } 
  `additionalKeysP`
    [ ("<Print>", spawn "scrot --select -e 'xclip -selection clipboard -t image/png -i $f'"),
      ("M-f", spawn "librewolf"),
      ("M-t", spawn "vesktop"),
      ("M-r", spawn "rofi -show drun -show-icons"),
      ("M-x", restart "/run/current-system/sw/bin/xmonad" True),
      ("M-q", spawn $ terminal def),
      ("M-c", kill),
      ("M1-<Tab>", windows W.focusDown)
      --("M-c", 
    ]

management :: ManageHook
management = composeAll
    [ className =? "xmobar" --> doFloat
    , isDialog            --> doFloat
    ]



bar:: PP
bar = def 
  { ppSep             = magenta " * ",
    ppTitleSanitize   = xmobarStrip,
    ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2,
    ppHidden          = white . wrap " " "",
    ppUrgent          = red . wrap (yellow "!") (yellow "!"),
    ppOrder           = \[ws, l, _, wins] -> [ws, l, wins],
    ppExtras          = [logTitles formatFocused formatUnfocused]
  }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""
