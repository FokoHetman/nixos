{-# LANGUAGE LambdaCase #-}
------------------------------------------------------------------------------
-- |
-- Copyright: (c) 2018, 2019, 2022 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: portable
-- Created: Sat Nov 24, 2018 21:03
--
--
-- An example of a Haskell-based xmobar. Compile it with
--   ghc --make -- xmobar.hs
-- with the xmobar library installed or simply call:
--   xmobar /path/to/xmobar.hs
-- and xmobar will compile and launch it for you and
------------------------------------------------------------------------------

import Xmobar

import Network.Socket
import Data.Time.Clock
import Control.Exception (SomeException (SomeException), try)
import Data.Word


split :: Char -> String -> [String]
split at = split' at ""
split' :: Char -> String -> String -> [String] 
split' at left (s:ource) = if s==at then left:split at ource else split' at (left++[s]) ource
split' _ l [] = [l]


type Host = String
measure :: Host -> IO (Either SomeException Double)
measure hostr = do
  let [host, port] = split ':' hostr
  start <- getCurrentTime
  result <- try $ do
    sock <- socket AF_INET Stream defaultProtocol
    addr <- getAddrInfo Nothing (Just host) (Just port)
    connect sock $ addrAddress $ head addr
    close sock
  end <- getCurrentTime

  pure $ fmap (\_ -> realToFrac (diffUTCTime end start) * 1000) result

data Hetmanat = Hetmanat String Int
    deriving (Read, Show)

instance Exec Hetmanat where
    alias (Hetmanat _ _) = "hat"
    rate (Hetmanat _ x) = x
    run (Hetmanat host _) = do
      result <- measure host >>= \case
            Right d | d < 50 -> pure "<fc=green>\57665 "  --  
                  | d <= 250  -> pure "<fc=yellow>\57669 " --  
                  | otherwise -> pure "<fc=orange>\57631 " --  
            Left _ -> pure "<fc=red>\57620 "               --  
      pure $ result ++ "</fc>"

-- todo: hetmanat status
-- todo: make this the launched xmobar config


-- Configuration, using predefined monitors as well as our HelloWorld
-- plugin:

config :: Config
config = defaultConfig { --overrideRedirect = True
       --, 
         font     = "FiraCode Medium 9"
       , bgColor  = "#282828"
       , fgColor  = "#ffffff"
       , textOffset = 0
       , position = TopH 24
       , lowerOnStart = True
       , overrideRedirect = True
       --, border = BottomB
       --, borderColor = "#000000"
       --, iconRoot = "/etc/xmobar/icons"
       , commands = [ Run $ Weather "EGPF"
                        [ "--template", "<weather> <tempC>°C"
                        , "-L", "0"
                        , "-H", "25"
                        , "--low"   , "lightblue"
                        , "--normal", "#f8f8f2"
                        , "--high"  , "red"
                        ] 36000
                    , Run $ Cpu
                        [ "-L", "3"
                        , "-H", "50"
                        , "--high"  , "red"
                        , "--normal", "green"
                        ] 10
                    , Run $ Alsa "default" "Master"
                        [ "--template", "<volumestatus>"
                        , "--suffix"  , "True"
                        , "--"
                        , "--on", ""
                        ]
                    , Run $ Com "playerctl" ["metadata", "title"] "playing" 25
                    , Run $ Memory ["--template", "Mem: <usedratio>%"] 10
                    , Run $ Swap [] 10
                    , Run $ Date "%a %Y-%m-%d <fc=#8be9fd>%H:%M</fc>" "date" 10
                    , Run $ Kbd [("pl", "PL"), ("us", "US"), ("ru", "RU")]
                    , Run $ Hetmanat "62.21.6.136:443" 600
                    , Run UnsafeXMonadLog
                    ]
       , sepChar  = "%"
       , alignSep = "}{"
       , template = "%UnsafeXMonadLog% <fc=#ddbf27>\57673 </fc> " ++
          "}<action=playerctl play-pause button=play>[ %playing% \57615 ]</action>{" ++
          "%kbd% %hat% <fc=#9edd27>\57670 </fc> <action=amixer set Master toggle>%alsa:default:Master%</action> | %cpu% | %memory% <*> %swap% | %EGPF% | %date% <fc=#dd3d27>\57688 </fc>"
       , additionalFonts = [ "RainWorldSymbols", "xft:FiraCode Medium:pixelsize=9"]
       }

--{
--  font = "xft:Sans Mono-9"
--  , additionalFonts = []
--  , borderColor = "black"
--  , border = TopB
--  , bgColor = "black"
--  , fgColor = "grey"
--  , alpha = 255
--  , position = Top
--  , textOffset = -1
--  , iconOffset = -1
--  , lowerOnStart = True
--  , pickBroadest = False
--  , persistent = False
--  , hideOnStart = False
--  , iconRoot = "."
--  , allDesktops = True
--  , overrideRedirect = True
--  , textOutputFormat = Ansi
--  , commands = [ Run $ Weather "EGPH" ["-t","<station>: <tempC>C",
--                                        "-L","18","-H","25",
--                                        "--normal","green",
--                                        "--high","red",
--                                      "--low","lightblue"] 36000
--               , Run $ Network "wlp5s0" ["-L","0","-H","32",
--                                      "--normal","green","--high","red"] 10
--               , Run $ Cpu ["-L","3","-H","50",
--                           "--normal","green","--high","red"] 10
--               , Run $ Memory ["-t","Mem: <usedratio>%"] 10
--               , Run $ Swap [] 10
--               , Run $ Com "uname" ["-s","-r"] "" 36000
--               , Run $ Date "%a %b %_d %Y %H:%M:%S" "date" 10
--              , Run HelloWorld
--              ]
--  , sepChar = "%"
--  , alignSep = "}{"
--  , template = "%cpu% | %memory% * %swap% | %eth0% - %eth1% }\
--               \ %hw% { <fc=#ee9a00>%date%</fc>| %EGPH% | %uname%"
--}

main :: IO ()
main = configFromArgs config >>= xmobar

