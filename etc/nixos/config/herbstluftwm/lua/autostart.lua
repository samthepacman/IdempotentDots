#!/usr/bin/lua
-- This is a modularized config for herbstluftwm

local dirname  = debug.getinfo(1).source:match("@?(.*/)")
package.path = package.path .. ';' .. dirname .. '?.lua;'

local gmc     = require ".gmc"
local helper  = require(".helper")
local config  = require(".config")
local startup = require(".startup")

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-- main

-- background before wallpaper
os.execute("xsetroot -solid '" .. gmc.color["blue500"] .. "'")

-- Read the manual in $ man herbstluftwm
helper.hc('emit_hook reload')

-- gap counter
os.execute("echo 35 > /tmp/herbstluftwm-gap")

-- do not repaint until unlock
helper.hc("lock")

-- standard
-- remove all existing keybindings
helper.hc('keyunbind --all')
helper.hc('mouseunbind --all')
helper.hc('unrule -F')

helper.set_tags_with_name()

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
-- do hash config

helper.do_config('keybind',   config.keybinds)
helper.do_config('keybind',   config.tagskeybinds)
helper.do_config('mousebind', config.mousebinds)
helper.do_config('attr',      config.attributes)
helper.do_config('set',       config.sets)
helper.do_config('rule',      config.rules)

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
-- finishing, some extra miles

-- I'm not sure what in this is
helper.bind_cycle_layout()

-- example of custom layout
local layout = "(split horizontal:0.5:0 "
         .."(clients vertical:0) (clients vertical:0))"
helper.hc("load " .. config.tag_names[0] .. " '" .. layout .. "'")

-- tag number 5
helper.hc("floating 5 on")

-- hc("set tree_style '╾│ ├└╼─┐'")
helper.hc("set tree_style '⊙│ ├╰»─╮'")

-- unlock, just to be sure
helper.hc("unlock")

-- launch statusbar panel (e.g. dzen2 or lemonbar)
helper.do_panel()

-- load on startup
startup.run()
