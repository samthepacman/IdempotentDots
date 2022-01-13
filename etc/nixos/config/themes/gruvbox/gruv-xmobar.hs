-- http://projects.haskell.org/xmobar/
-- install xmobar with these flags: --flags="with_alsa" --flags="with_mpd" --flags="with_xft"  OR --flags="all_extensions"
-- you can find weather location codes here: http://weather.noaa.gov/index.html

Config { font    = "xft:Iosevka Slab:weight=Medium:pixelsize=12:antialias=true:hinting=true"
       , additionalFonts = [ "xft:Droid Sans:pixelsize=11:antialias=true:hinting=true"
                           , "xft:Iosevka Nerd Font:pixelsize=16:antialias=true:hinting=true"
                           , "xft:FontAwesome:pixelsize=13"
                           ]
       , textOffset = 18
       , bgColor = "#282828"
       , fgColor = "#ebdbb2"
       , position = Static { xpos = 0, ypos = 0, width = 1366, height = 28 }
       , lowerOnStart = True
       , hideOnStart = False
       , allDesktops = True
       , persistent = True
       , iconRoot = "/home/shiva/.config/nixpkgs/role/x11/xpms/"  -- default: "."
       , commands = [ 
                      -- Time and date
                      Run Date "<fn=1></fn>Date : %b %d %Y" "date" 50
                    , Run Date "<fn=1></fn>Time : %H:%M" "time" 50
                    , Run Network "wlp2s0" ["-t", "<fn=0>U :</fn> <rx>kb <fn=0> :</fn> <tx>kb"] 20
                    , Run Cpu ["-t", "<fn=1></fn>Cpu : <total>%","-H","70","--high","red"] 20
                    , Run Memory ["-t", "<fn=1>\xf233</fn> mem: <used>M (<usedratio>%)"] 20
                    , Run DiskU [("/", " H : <free> free")] [] 60
                    , Run MPD ["-t", "<state>: <artist> - <track>"] 10
                    , Run Com "uname" ["-r"] "" 3600 

                      -- Prints out the left side items such as workspaces, layout, etc.
                      -- The workspaces are 'clickable' in my configs.
                    , Run UnsafeStdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " %UnsafeStdinReader% }{<fc=#50fa7b>  %cpu% </fc><fc=#abb2bf><fn=0> | </fn></fc><fc=#ffb86c> %date% </fc><fc=#abb2bf><fn=0> |</fn></fc><fc=#bd93f9>  %time%  </fc>"
       }
