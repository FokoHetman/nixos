Config { overrideRedirect = False
       , font     = "FiraCode Medium 9"
       , bgColor  = "#282828"
       , fgColor  = "#ffffff"
       , position = TopH 24
       --, border = BottomB
       --, borderColor = "#000000"
       --, iconRoot = "/etc/xmobar/icons"
       , commands = [ Run Weather "EGPF"
                        [ "--template", "<weather> <tempC>°C"
                        , "-L", "0"
                        , "-H", "25"
                        , "--low"   , "lightblue"
                        , "--normal", "#f8f8f2"
                        , "--high"  , "red"
                        ] 36000
                    , Run Cpu
                        [ "-L", "3"
                        , "-H", "50"
                        , "--high"  , "red"
                        , "--normal", "green"
                        ] 10
                    , Run Alsa "default" "Master"
                        [ "--template", "<volumestatus>"
                        , "--suffix"  , "True"
                        , "--"
                        , "--on", ""
                        ]
                    , Run Com "playerctl" ["metadata", "title"] "playing" 50
                    , Run Memory ["--template", "Mem: <usedratio>%"] 10
                    , Run Swap [] 10
                    , Run Date "%a %Y-%m-%d <fc=#8be9fd>%H:%M</fc>" "date" 10
                    , Run UnsafeXMonadLog
                    ]
       , sepChar  = "%"
       , alignSep = "}{"
       , template = "%UnsafeXMonadLog% }{  [ %playing% <action=playerctl play-pause button=play></action> ]  %alsa:default:Master% | %cpu% | %memory% <*> %swap% | %EGPF% | %date%  "
       , additionalFonts = [ "RainWorldSymbols"]
       }

