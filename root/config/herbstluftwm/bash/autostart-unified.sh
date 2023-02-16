#!/usr/bin/env bash
# ------------------------------------------------------------------
#
#     Description: unified config for herbstluftwm autostart
#     Created by: Epsi Nurwijayadi <epsi.nurwijayadi@gmail.com)
#
#     Source
#     https://github.com/epsi-rns/dotfiles/tree/master/herbstluftwm/bash
#
#     Blog
#     http://epsi-rns.github.io/desktop/2017/05/01/herbstlustwm-modularized-overview.html
#     http://epsi-rns.github.io/desktop/2017/05/02/herbstlustwm-modularized-bash.html
#
# ------------------------------------------------------------------

DIR=$(dirname "$0")
. ${DIR}/gmc.sh

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# config

# https://www.gnu.org/software/bash/manual/bashref.html#Brace-Expansion

# `seq 10` 0, or $(seq 10)
tag_names=( {1..9} )
tag_keys=( {1..9} 0 )

# keybindings

# if you have a super key you will be much happier with Mod set to Mod4
# Mod=Mod1    # Use alt as the main modifier
# Alt=Mod1
# Mod=Mod4   # Use the super key as the main modifier

# Modifier variables
s=Shift
c=Control
m=Mod4
a=Mod1

# resizing frames
resizestep=0.05

declare -A keybinds=(
  # session
    ["$m-$s-q"]='quit'
    ["$m-$s-r"]='reload'
    ["$m-$s-c"]='close'

  # use your $TERMINAL with xterm as fallback
    ["$m-Return"]="spawn ${TERMINAL:-xfce4-terminal}"

  # epsi
    ["$m-d"]='spawn dmenu_run_hlwm'
    ["$m-$s-d"]='spawn rofi -show run -opacity 90'
    ["$m-$s-x"]='spawn oblogout'

  # basic movement

  # focusing clients
    ["$m-Left"]='focus left'
    ["$m-Down"]='focus down'
    ["$m-Up"]='focus up'
    ["$m-Right"]='focus right'
    ["$m-h"]='focus left'
    ["$m-j"]='focus down'
    ["$m-k"]='focus up'
    ["$m-l"]='focus right'

  # moving clients
    ["$m-$s-Left"]='shift left'
    ["$m-$s-Down"]='shift down'
    ["$m-$s-Up"]='shift up'
    ["$m-$s-Right"]='shift right'
    ["$m-$s-h"]='shift left'
    ["$m-$s-j"]='shift down'
    ["$m-$s-k"]='shift up'
    ["$m-$s-l"]='shift right'
    
  # splitting frames
  # create an empty frame at the specified direction
    ["$m-u"]='split   bottom  0.5'
    ["$m-o"]='split   right   0.5'
  # let the current frame explode into subframes
    ["$m-$c-space"]='split explode'

  # resizing frames
    ["$m-$c-h"]="resize left  +$resizestep"
    ["$m-$c-j"]="resize down  +$resizestep"
    ["$m-$c-k"]="resize up    +$resizestep"
    ["$m-$c-l"]="resize right +$resizestep"
    ["$m-$c-Left"]="resize left  +$resizestep"
    ["$m-$c-Down"]="resize down  +$resizestep"
    ["$m-$c-Up"]="resize up    +$resizestep"
    ["$m-$c-Right"]="resize right +$resizestep"
    
    # MPC
    ["$m-$a-h"]='spawn mpc toggle'
    ["$m-$a-t"]='spawn mpc prev'
    ["$m-$a-n"]='spawn mpc next'
)

# tags


declare -A tagskeybinds=(
  # cycle through tags
    ["$m-period"]='use_index +1 --skip-visible'
    ["$m-comma"]='use_index -1 --skip-visible'

  # layouting
    ["$m-r"]='remove'
    ["$m-s"]='floating toggle'
    ["$m-f"]='fullscreen toggle'
    ["$m-p"]='pseudotile toggle'

  # focus
    ["$m-BackSpace"]='cycle_monitor'
    ["$m-Tab"]='cycle_all +1'
    ["$m-$s-Tab"]='cycle_all -1'
    ["$m-c"]='cycle'
    ["$m-i"]='jumpto urgent'
)

declare -A mousebinds=(
  # mouse    
    ["$m-Button1"]='move'
    ["$m-Button2"]='zoom'
    ["$m-Button3"]='resize'
)

# theme

declare -A attributes=(
    ["theme.tiling.reset"]='1'
    ["theme.floating.reset"]='1'

    ["theme.active.color"]="${color['red500']}"
    ["theme.normal.color"]="${color['grey200']}"
    ["theme.urgent.color"]="${color['pink500']}"

    ["theme.inner_width"]='0'
    ["theme.inner_color"]='black'

    ["theme.border_width"]='2'
    ["theme.floating.border_width"]='4'
    ["theme.floating.outer_width"]='1'
    ["theme.floating.outer_color"]='black'

    ["theme.active.inner_color"]='#3E4A00'
    ["theme.active.outer_color"]='#3E4A00'
    ["theme.background_color"]='#141414'
)

declare -A sets=(
    ["frame_border_active_color"]="${color['grey200']}"
    ["frame_bg_active_color"]="${color['yellow900']}"

    ["frame_border_normal_color"]="${color['grey50']}"
    ["frame_bg_normal_color"]="${color['red500']}"

    ["frame_border_width"]='0'
    ["always_show_frame"]='0'
    ["frame_bg_transparent"]='1'
    ["frame_transparent_width"]='2'
    ["frame_gap"]='20'

    ["window_gap"]='0'
    ["frame_padding"]='0'
    ["smart_window_surroundings"]='0'
    ["smart_frame_surroundings"]='1'
    ["mouse_recenter_gap"]='0'
)

# rules

declare -A rules=(
  # normally focus new clients
    ["focus=on"]=''

  # zero based array
    ["class=Firefox"]="tag=${tag_names[1]}"
    ["class=Chromium"]="tag=${tag_names[1]}"
    ["class=Geany"]="tag=${tag_names[2]}"
    ["class=Thunar"]="tag=${tag_names[3]}"
    ["class=gimp"]="tag=${tag_names[4]} pseudotile=on"

    ["class=Oblogout"]='fullscreen=on'
    
    ["class~'(.*[Rr]xvt.*|.*[Tt]erm|Konsole)'"]='focus=on'

  # ["windowtype~'_NET_WM_WINDOW_TYPE_(DIALOG|UTILITY|SPLASH)'"]='pseudotile=on'
  # ["windowtype='_NET_WM_WINDOW_TYPE_DIALOG'"]='focus=on'
    ["windowtype='_NET_WM_WINDOW_TYPE_DIALOG'"]='fullscreen=on'
  # ["windowtype~'_NET_WM_WINDOW_TYPE_(NOTIFICATION|DOCK|DESKTOP)'"]='manage=off'
)

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# helper

function hc() {
    # http://www.thegeekstuff.com/2010/05/bash-shell-special-parameters/
    herbstclient "$@"
}

function do_config()
{
    local command="${1}"
    shift
    
    # associative array hack
    eval "declare -A hash="${1#*=}

    # loop over hash    
    for key in "${!hash[@]}"; do
        local value=${hash[$key]}        
        hc $command $key $value
        
        # uncomment to debug in terminal
        # echo $command $key $value
    done
}

function set_tags_with_name() {
    hc rename default "${tag_names[0]}" 2>/dev/null || true
    
    for index in ${!tag_names[@]} ; do
        hc add "${tag_names[$index]}"
        
        # uncomment to debug in terminal
        # echo $index

        local key="${tag_keys[$index]}"
        if ! [ -z "$key" ] ; then
            hc keybind "$m-$key" use_index "$index"
            hc keybind "$m-Shift-$key" move_index "$index"
        fi
    done
}

# I don't understand what this is
function bind_cycle_layout() {
    # The following cycles through the available layouts
    # within a frame, but skips layouts, if the layout change 
    # wouldn't affect the actual window positions.
    # I.e. if there are two windows within a frame,
    # the grid layout is skipped.

    hc keybind $m-space                                       \
      or , and . compare tags.focus.curframe_wcount = 2       \
      . cycle_layout +1 vertical horizontal max vertical grid \
      , cycle_layout +1
}

# do multi monitor setup here, e.g.:
# hc set_monitors 1280x1024+0+0 1280x1024+1280+0
# or simply:
# hc detect_monitors

function do_panel() {
    local panel=$(dirname "$0")/panel-lemonbar.sh
    [ -x "$panel" ] || panel=/etc/xdg/herbstluftwm/panel.sh
    for monitor in $(herbstclient list_monitors | cut -d: -f1) ; do
        # start it on each monitor
        "$panel" $monitor &
    done
}

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# load on startup

startup_run() {
    command="silent new_attr bool my_not_first_autostart"
    
    if hc $command ; then
      # non windowed app
        compton &
        dunst &
        parcellite &
        nitrogen --restore &
        mpd &
    
      # windowed app
        xfce4-terminal &
        sleep 1 && firefox &
        sleep 2 && geany &
        sleep 2 && thunar &
    fi
}

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
