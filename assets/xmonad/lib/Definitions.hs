module Definitions where


import Data.Monoid
import XMonad

import Data.Tree
import qualified XMonad.Actions.TreeSelect as TS

------------------------------------------------------------------------
-- Definitions
--


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




term, multiplexer, browser, discord :: String
term = "kitty"
multiplexer = "tmux"
browser = "librewolf"
discord = "nix run nixpkgs#legcord"

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
