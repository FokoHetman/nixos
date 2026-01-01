module Definitions where


import Data.Monoid
import qualified Data.Map as M


import XMonad
import XMonad.Util.NamedScratchpad

import qualified XMonad.StackSet as W
import Data.Tree
import qualified XMonad.Actions.TreeSelect as TS
import Data.Word (Word32)

import System.Random
import System.Directory (getDirectoryContents)
import XMonad.Prompt.Input
import XMonad.Prompt (XPConfig(font, bgColor, borderColor, promptBorderWidth, fgColor, position, height, historySize, historyFilter, defaultText, autoComplete, showCompletionOnTab, searchPredicate, alwaysHighlight, maxComplRows), XPPosition (Top))
import XMonad.Util.Run (runProcessWithInput)
import Data.Char (isSpace)
import XMonad.Prelude (isPrefixOf)

import XMonad.Actions.Commands
import XMonad.Actions.MouseGestures
import TrueFullscreen (toggleFullscreen)
------------------------------------------------------------------------
-- Definitions
--

gestures = M.fromList
         [ ([L, R], const kill)
         , ([R, D], const toggleFullscreen)
         , ([U], \w -> focus w >> windows W.swapUp)
         , ([D], \w -> focus w >> windows W.swapDown)
         , ([D, L], \_ -> sendMessage NextLayout)
         ]

treeselectAction :: TS.TSConfig (X ()) -> X ()
treeselectAction a = TS.treeselectAction a
   [ 
     Node (TS.TSNode "kitty" "kitty"              (spawn term)) []
   , Node (TS.TSNode "hello"    "displays hello"            (spawn ", xmessage hello!")) []
   , Node (TS.TSNode "status" "shutdown/reboot" (return())) 
    [
        Node (TS.TSNode "shutdown" "Shutdown the system"       (spawn "shutdown now")) []
      , Node (TS.TSNode "reboot" "Reboot the system" (spawn "reboot")) []
    ]
   ]


commonApps = [term,browser,"blender","krita","drawio","godot4.4","prismlauncher", "obs", "zathura", discord]



term, multiplexer, browser, discord, hetmanat, roadmapPath :: String
term = "kitty"
multiplexer = "tmux"
browser = "librewolf"
discord = "nix run nixpkgs#legcord"
hetmanat = "https://hetman.at/"
roadmapPath = "roadmap/2026/"

myFont = "xft:FiraCode Medium:size=9"

myBorderWidth :: Word32
myBorderWidth = 2
myBorderColor = myGreen
myFocusedBorderColor = "#FB4934"
myBackground = "#282828"

myLightGreen, myGreen, myBlue, myRed, myYellow, myOrange, myWhite, myTeal, myMagenta :: String
myLightGreen = "#50FA7B"
myGreen = "#98971A"
myBlue =  "#89b4fa"
myRed   = "#CC241D"
myYellow= "#fab387"
myOrange= "#FE8019"
myWhite = "#f8f8f2"
myTeal = "#94e2d5"
myMagenta = "#B16286"

myWorkspaces :: Forest String
myWorkspaces = [ Node "etc" []
               , Node "www" []
               , Node "dev"
                    [
                      Node "tmp" []
                    ]
               , Node "art" []
               , Node "misc" 
                    [
                        Node "1" []
                      , Node "2" []
                      , Node "3" []
                    ]
               ]

myTSConfig = TS.TSConfig { TS.ts_hidechildren = True
                           , TS.ts_background   = 0xc0282828
                           , TS.ts_font         = "xft:Sans-16"
                           , TS.ts_node         = (0xfffbf1c7, 0xff282828)
                           , TS.ts_nodealt      = (0xfffbf1c7, 0xff32302f)
                           , TS.ts_highlight    = (0xfffbf1c7, 0xff90971a)
                           , TS.ts_extra        = 0xffa89984
                           , TS.ts_node_width   = 260
                           , TS.ts_node_height  = 40
                           , TS.ts_originX      = 0
                           , TS.ts_originY      = 0
                           , TS.ts_indent       = 80
                           , TS.ts_navigate     = TS.defaultNavigation
                           }

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

--setRandomWallpaper :: X ()
--setRandomWallpaper = do
--  let wallpapers = getDirectoryContents "~/.config/wallpapers"
--
--  let roll = uniformR (1, 6)
--  let rolls = unfoldr (Just . roll)
--  let pureGen = mkStdGen 42
--  let wp_index = head $ rolls pureGen


myXPConfig :: XPConfig
myXPConfig = def 
  {
      font                  = myFont
    , bgColor               = myBackground
    , fgColor               = "#f2f2f2"
    , borderColor           = myFocusedBorderColor
    , promptBorderWidth     = 1
    , position              = Top
    , height                = 20 
    , historySize          = 256
    , historyFilter         = id
    , defaultText           = []
    , autoComplete          = Just 100000
    , showCompletionOnTab   = False
    , searchPredicate       = isPrefixOf
    , alwaysHighlight       = True
    , maxComplRows          = Nothing
    
  }

mkPrompt :: String -> (XPConfig -> String -> X ())
mkPrompt s = \c ans -> inputPrompt c (trim ans) ?+ \input -> do 
    _ <- runProcessWithInput browser [s ++ input] ""
    --hooglePrompt c ans
    pure ()
  where 
    trim = f . f
      where f = reverse . dropWhile isSpace

hooglePrompt :: XPConfig -> String -> X ()
hooglePrompt = mkPrompt "hoogle.haskell.org/?hoogle="


nooglePrompt :: XPConfig -> String -> X ()
nooglePrompt = mkPrompt "noogle.dev/?term="
