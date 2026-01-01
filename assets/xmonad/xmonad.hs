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
import XMonad.Layout.BoringWindows (focusUp, focusDown, focusMaster)
import XMonad.Hooks.WallpaperSetter (wallpaperSetter, defWallpaperConf, WallpaperConf (wallpapers), defWPNames, WallpaperList (WallpaperList), defWPNamesPng, Wallpaper (WallpaperDir))
import System.Directory (getHomeDirectory)
import XMonad.Actions.MouseGestures (mouseGesture)
import XMonad.Actions.ToggleFullFloat (toggleFullFloatEwmhFullscreen)
import GHC.IORef (newIORef, IORef)

import qualified Vimization as Vim
import Foreign.ScreenCast (screencast)

main :: IO ()

main = do
  mode <- newIORef Vim.Normal
  let mySB = statusBarProp "systemctl --user start xmobar.service" (clickablePP $ bar mode)

  home <- getHomeDirectory

  xmonad . toggleFullFloatEwmhFullscreen . ewmhFullscreen . ewmh . withEasySB mySB defToggleStrutsKey
    . Vim.vimize mode
    $ docks (myConf home)


myConf home = def 
  { modMask     = mod4Mask 
    , terminal    = term
    , manageHook  = manageDocks <+> namedScratchpadManageHook scratchpads <+> manageSpawn <+> management
    , layoutHook  = myLayout
    , startupHook = myStartupHook
    , workspaces = TS.toWorkspaces myWorkspaces
    , logHook = let 
        defWP = home ++ "/.config/wallpapers/def"
      in wallpaperSetter defWallpaperConf {
                               wallpapers = defWPNamesPng (TS.toWorkspaces myWorkspaces)
                                         <> WallpaperList [
                                              ("etc",    WallpaperDir defWP)
                                            , ("www",    WallpaperDir defWP)
                                            , ("dev",    WallpaperDir defWP)
                                            , ("dev.tmp",WallpaperDir defWP)
                                            , ("art",    WallpaperDir defWP)
                                          ]
                            } <> workspaceHistoryHook

    , handleEventHook = myEventHook
    , normalBorderColor = myBorderColor
    , focusedBorderColor  = myFocusedBorderColor
    --handleEventHook = myHandleEventHook

--    logHook = dynamicLogWithPP bar
--      { ppOutput = \x -> hPutStrLn mySB
--
--      }
  } 
  `additionalKeys`
    concat [[
      ((0,key), spawn "scrot --select -e 'xclip -selection clipboard -t image/png -i $f'")
    , ((mod4Mask, key), screencast [])
    , ((mod4Mask .|. shiftMask, key), screencast ["-a"])] | key<-[xK_F12,xK_Print]]
  `additionalKeys`
        {--- capture
        ((0,xK_F12), spawn "scrot --select -e 'xclip -selection clipboard -t image/png -i $f'") -- F6
      , ((0,xK_Print), spawn "scrot --select -e 'xclip -selection clipboard -t image/png -i $f'") -- <Print>
      , ((mod4Mask, xK_Print), screencast [])
      , ((mod4Mask .|. shiftMask, xK_Print), screencast ["-a"])-}
    [
      ( (mod4Mask, xK_f), spawn browser) -- M-f
      --, ("M-t", spawn "vesktop")
      , ((mod4Mask, xK_r), spawn "rofi -show drun -show-icons") -- M-r
      , ((mod4Mask, xK_x), restart "/run/current-system/sw/bin/xmonad" True) -- M-x
      , ((mod4Mask, xK_q), spawn $ terminal $ myConf home) -- M-q
      , ((mod4Mask, xK_c), kill) -- M-c
      , ((mod1Mask, xK_Tab), windows W.focusDown) -- M1-<Tab>
      , ((mod4Mask .|. controlMask, xK_l), spawn "i3lock 20 pixel") -- M-C-l
      , ((mod4Mask, xK_v), withFocused toggleFloat)
      , ((mod4Mask, xK_n), namedScratchpadAction scratchpads "notes") -- M-n
      , ((mod4Mask, xK_e), namedScratchpadAction scratchpads "term") -- M-e
      , ((mod4Mask .|. controlMask, xK_e), namedScratchpadAction scratchpads "nixos") -- M-C-e
      , ((mod4Mask, xK_t), spawnSelected def commonApps) -- M-t
      , ((mod4Mask, xK_Tab), TS.treeselectWorkspace myTSConfig myWorkspaces W.greedyView) -- M-<Tab>
      , ((mod4Mask, xK_z), treeselectAction myTSConfig) -- M-z
      , ((mod4Mask .|. mod1Mask, xK_space), spawn "layout-sw") -- M-M1-<Space>
      , ((0, xK_F11), toggleFullscreen)
      , ((0, xK_KP_Add), focusUp)
      , ((0, xK_KP_Subtract), focusDown)
      , ((0, xK_KP_Enter), focusMaster)
      --, ((mod4Mask, xK_h), hooglePrompt myXPConfig "hoogle") moved to Vimization
      , ((mod1Mask, xK_q), spawn $ term ++ " ssh hetman.at")
      --("M-c", 
    ] `additionalMouseBindings` [
        ((shiftMask, button3), mouseGesture gestures)
    ]
    where
      toggleFloat w = windows (\s -> if M.member w (W.floating s)
        then W.sink w s
        else W.float w (W.RationalRect (1/3) (1/4) (1/2) (4/5)) s)


management :: ManageHook
management = composeAll
    [ className =? "lwpwlp" --> doLower
    , className =? "quickshell" --> doFloat
    , className =? "quickshell" --> doIgnore
    , isDialog            --> doFloat
    ]



bar :: IORef Vim.Mode -> PP
bar modeRef = def 
  { ppSep             = sep " * ",
    ppTitleSanitize   = xmobarStrip,
    ppCurrent         = wrap (active "[") (active "]") . active,-- . xmobarBorder "Top" myGreen 2,
    ppHidden          = inactive . wrap " " " ",
    ppUrgent          = red . wrap (yellow "!") (yellow "!"),
    ppOrder           = \(ws:l:_:vim:win:extras) -> extras ++ [vim, ws, l, win],
    ppExtras          = [Vim.modeLogger modeRef, logTitles formatFocused formatUnfocused]
  }
  where
    formatFocused   = wrap (white    "[") (white    "]") . focused . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . unfocused    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, green, yellow :: String -> String
    magenta  = xmobarColor myMagenta ""
    blue     = xmobarColor "#458588" ""
    white    = xmobarColor myWhite ""
    yellow   = xmobarColor myYellow ""
    red      = xmobarColor myRed ""
    lowWhite = xmobarColor "#bbbbbb" ""
    green    = xmobarColor myGreen ""

    sep = magenta

    active = green
    inactive = lowWhite

    focused = active
    unfocused = yellow
