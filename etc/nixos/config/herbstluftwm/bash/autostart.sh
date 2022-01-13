#!/usr/bin/env bash
# This is a modularized config for herbstluftwm

DIR=$(dirname "$0")

. ${DIR}/gmc.sh
. ${DIR}/config.sh
. ${DIR}/helper.sh
. ${DIR}/startup.sh

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# main

# background before wallpaper
xsetroot -solid "${color['blue500']}"

# Read the manual in $ man herbstluftwm
hc emit_hook reload

# gap counter
echo 35 > /tmp/herbstluftwm-gap

# do not repaint until unlock
hc lock

# standard
# remove all existing keybindings
hc keyunbind --all
hc mouseunbind --all
hc unrule -F

set_tags_with_name

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# do hash config
# hack associative array function argument passing using declare -p

do_config 'keybind'   "$(declare -p keybinds)"
do_config 'keybind'   "$(declare -p tagskeybinds)"
do_config 'mousebind' "$(declare -p mousebinds)"
do_config 'attr'      "$(declare -p attributes)"
do_config 'set'       "$(declare -p sets)"
do_config 'rule'      "$(declare -p rules)"

# avoid tilde problem, not using helper
hc rule windowtype~'_NET_WM_WINDOW_TYPE_(NOTIFICATION|DOCK|DESKTOP)' manage=off
hc rule windowtype~'_NET_WM_WINDOW_TYPE_(DIALOG|UTILITY|SPLASH)' pseudotile=on

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# finishing, some extra miles

# I'm not sure what in this is
#bind_cycle_layout

# example of custom layout
layout='(split horizontal:0.5:0 '
layout+='(clients vertical:0) (clients vertical:0))'
hc load ${tag_names[0]} "$layout"

# tag number 5
hc floating 5 on

# hc set tree_style '╾│ ├└╼─┐'
hc set tree_style '⊙│ ├╰»─╮'

# unlock, just to be sure
hc unlock

# launch statusbar panel (e.g. dzen2 or lemonbar)
do_panel

# load on startup
startup_run
