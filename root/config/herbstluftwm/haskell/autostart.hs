-- This is a modularized config for herbstluftwm

import System.Process

import MyGMC
import MyConfig
import MyHelper
import MyStartup

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-- main

main = do
    -- background before wallpaper
    system $ "xsetroot -solid '" ++ myColor "blue500" ++ "'"

    -- Read the manual in $ man herbstluftwm
    hc "emit_hook reload"

    -- gap counter
    system $ "echo 35 > /tmp/herbstluftwm-gap"
    
    -- do not repaint until unlock
    hc "lock"

    -- standard
    -- remove all existing keybindings
    hc "keyunbind --all"
    hc "mouseunbind --all"
    hc "unrule -F"

    set_tags_with_name

    -- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
    -- do hash config

    do_config "keybind"   keybinds
    do_config "keybind"   tagskeybinds
    do_config "mousebind" mousebinds
    do_config "attr"      attributes
    do_config "set"       sets
    do_config "rule"      rules

    -- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
    -- finishing, some extra miles

    -- I'm not sure what in this is
    bind_cycle_layout

    -- example of custom layout
    let layout = "(split horizontal:0.5:0 "
              ++ "(clients vertical:0) (clients vertical:0))"
    hc("load " ++ show(tag_names !! 0) ++ " '" ++ layout ++ "'")

    -- tag number 5
    hc "floating 5 on"

    -- hc "set tree_style '╾│ ├└╼─┐'"
    hc "set tree_style '⊙│ ├╰»─╮'"

    -- unlock, just to be sure
    hc "unlock"

    -- load on startup
    startup_run
