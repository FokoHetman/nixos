Config { overrideRedirect = False
       , font     = "FiraCode Medium 9"
       , bgColor  = "#282828"
       , fgColor  = "#ffffff"
       , position = TopH 24
       , lowerOnStart = False
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
                    , Run Kbd [("se", "SE"), ("us", "US")]
                    , Run UnsafeXMonadLog
                    ]
       , sepChar  = "%"
       , alignSep = "}{"
       , template = "%UnsafeXMonadLog% <fc=#ddbf27> </fc> }<action=playerctl play-pause button=play>[ %playing%  ]</action>{ %kbd% <fc=#9edd27> </fc> <action=amixer set Master toggle>%alsa:default:Master%</action> | %cpu% | %memory% <*> %swap% | %EGPF% | %date% <fc=#dd3d27> </fc>"
       , additionalFonts = [ "RainWorldSymbols"]
       }

