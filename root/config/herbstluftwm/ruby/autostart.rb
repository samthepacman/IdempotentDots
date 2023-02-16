#!/usr/bin/ruby
# This is a modularized config for herbstluftwm

require_relative 'gmc'
require_relative 'helper'
require_relative 'config'
require_relative 'startup'

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# main

# background before wallpaper
system("xsetroot -solid '#{GMC::COLOR['blue500']}'")

# Read the manual in $ man herbstluftwm
hc('emit_hook reload')

# gap counter
system("echo 35 > /tmp/herbstluftwm-gap")

# do not repaint until unlock
hc("lock");

# standard
# remove all existing keybindings
hc('keyunbind --all')
hc("mouseunbind --all")
hc("unrule -F")

set_tags_with_name()

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# do hash config

do_config("keybind",   Config::Keybinds)
do_config("keybind",   Config::Tagskeybinds)
do_config("mousebind", Config::Mousebinds)
do_config("attr",      Config::Attributes)
do_config("set",       Config::Sets)
do_config("rule",      Config::Rules)

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# finishing, some extra miles

# I'm not sure what in this is
bind_cycle_layout()

# example of custom layout
layout  = "(split horizontal:0.5:0 " \
          "(clients vertical:0) (clients vertical:0))"
hc("load #{Config::Tag_names[0]} '#{layout}'")

# tag number 5
hc("floating 5 on")

# hc("set tree_style '╾│ ├└╼─┐'")
hc("set tree_style '⊙│ ├╰»─╮'")

# unlock, just to be sure
hc("unlock")

# launch statusbar panel (e.g. dzen2 or lemonbar)
do_panel()

# load on startup
startup_run
