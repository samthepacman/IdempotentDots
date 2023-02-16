#!/usr/bin/env python3
# ------------------------------------------------------------------
#
#     Description: unified config for herbstluftwm autostart
#     Created by: Epsi Nurwijayadi <epsi.nurwijayadi@gmail.com)
#
#     Source
#     https://github.com/epsi-rns/dotfiles/tree/master/herbstluftwm/python
#
#     Blog
#     http://epsi-rns.github.io/desktop/2017/05/01/herbstlustwm-modularized-overview.html
#     http://epsi-rns.github.io/desktop/2017/05/04/herbstlustwm-modularized-python.html
#
# ------------------------------------------------------------------

import os
from gmc import color

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# config

tag_names = list(range(1, 10))
tag_keys  = list(range(1, 10)) + [0]

# keybindings

# if you have a super key you will be much happier with Mod set to Mod4
# Mod=Mod1    # Use alt as the main modifier
# Alt=Mod1
# Mod=Mod4    # Use the super key as the main modifier

# Modifier variables
s = 'Shift'
c = 'Control'
m = 'Mod4'
a = 'Mod1'

# resizing frames
resizestep = "0.05";

keybinds = {
  # session
    m+'-'+s+'-q' : 'quit',
    m+'-'+s+'-r' : 'reload',
    m+'-'+s+'-c' : 'close',

  # use your $TERMINAL with xterm as fallback
    m+'-Return'   : 'spawn ${TERMINAL:-xfce4-terminal}', 

  # epsi
    m+'-d'        : 'spawn dmenu_run_hlwm',
    m+'-'+s+'-d'     : 'spawn rofi -show run -opacity 90',
    m+'-'+s+'-x'     : 'spawn oblogout',

  # basic movement

  # focusing clients
    m+'-Left'     : 'focus left',
    m+'-Down'     : 'focus down',
    m+'-Up'       : 'focus up',
    m+'-Right'    : 'focus right',
    m+'-h'        : 'focus left',
    m+'-j'        : 'focus down',
    m+'-k'        : 'focus up',
    m+'-l'        : 'focus right',

  # moving clients
    m+'-'+s+'-Left'  : 'shift left',
    m+'-'+s+'-Down'  : 'shift down',
    m+'-'+s+'-Up'    : 'shift up',
    m+'-'+s+'-Right' : 'shift right',
    m+'-'+s+'-h'     : 'shift left',
    m+'-'+s+'-j'     : 'shift down',
    m+'-'+s+'-k'     : 'shift up',
    m+'-'+s+'-l'     : 'shift right',
    
  # splitting frames
  # create an empty frame at the specified direction
    m+'-u'        : 'split   bottom  0.5',
    m+'-o'        : 'split   right   0.5',
  # let the current frame explode into subframes
    m+'-'+c+'-space' : 'split explode',

  # resizing frames
    m+'-'+c+'-h'     : 'resize left  +' + resizestep,
    m+'-'+c+'-j'     : 'resize down  +' + resizestep,
    m+'-'+c+'-k'     : 'resize up    +' + resizestep,
    m+'-'+c+'-l'     : 'resize right +' + resizestep,
    m+'-'+c+'-Left'  : 'resize left  +' + resizestep,
    m+'-'+c+'-Down'  : 'resize down  +' + resizestep,
    m+'-'+c+'-Up'    : 'resize up    +' + resizestep,
    m+'-'+c+'-Right' : 'resize right +' + resizestep,
    
    # MPC
    m+'-'+a+'-h'     : 'spawn mpc toggle',
    m+'-'+a+'-t'     : 'spawn mpc prev',
    m+'-'+a+'-n'     : 'spawn mpc next'
}

# tags

tagskeybinds = {
  # cycle through tags
    m+'-period'   : 'use_index +1 --skip-visible',
    m+'-comma'    : 'use_index -1 --skip-visible',

  # layouting
    m+'-r'        : 'remove',
    m+'-s'        : 'floating toggle',
    m+'-f'        : 'fullscreen toggle',
    m+'-p'        : 'pseudotile toggle',

  # focus
    m+'-BackSpace' : 'cycle_monitor',
    m+'-Tab'       : 'cycle_all +1',
    m+'-'+s+'-Tab'    : 'cycle_all -1',
    m+'-c'         : 'cycle',
    m+'-i'         : 'jumpto urgent'
}

mousebinds = {
  # mouse    
    m+'-Button1'  : 'move',
    m+'-Button2'  : 'zoom',
    m+'-Button3'  : 'resize'
}

# theme

attributes = {
    'theme.tiling.reset'    : '1',
    'theme.floating.reset'  : '1',

    'theme.active.color'    : "'" + color['red500'] + "'",
    'theme.normal.color'    : "'" + color['grey200'] + "'",
    'theme.urgent.color'    : "'" + color['pink500'] + "'",

    'theme.inner_width'     : '0',
    'theme.inner_color'     : 'black',

    'theme.border_width'    : '2',
    'theme.floating.border_width' : '4',
    'theme.floating.outer_width'  : '1',
    'theme.floating.outer_color'  : "'black'",

    'theme.active.inner_color'    : "'#3E4A00'",
    'theme.active.outer_color'    : "'#3E4A00'",
    'theme.background_color'      : "'#141414'"
}

sets = {
    'frame_border_active_color' : "'" + color['grey200'] + "'",
    'frame_bg_active_color'     : "'" + color['yellow900'] + "'",

    'frame_border_normal_color' : "'" + color['grey50'] + "'",
    'frame_bg_normal_color'     : "'" + color['red500'] + "'",

    'frame_border_width'        : '0',
    'always_show_frame'         : '0',
    'frame_bg_transparent'      : '1',
    'frame_transparent_width'   : '2',
    'frame_gap'                 : '20',


    'window_gap'                : '0',
    'frame_padding'             : '0',
    'smart_window_surroundings' : '0',
    'smart_frame_surroundings'  : '1',
    'mouse_recenter_gap'        : '0'
}

# rules

rules = {
  # normally focus new clients
    'focus=on' : '',

  # zero based array
    'class=Firefox'  : 'tag=' + str(tag_names[1]),
    'class=Chromium' : 'tag=' + str(tag_names[1]),
    'class=Geany'    : 'tag=' + str(tag_names[2]),
    'class=Thunar'   : 'tag=' + str(tag_names[3]),
    'class=gimp'     : 'tag=' + str(tag_names[4]) + ' pseudotile=on',

    'class=Oblogout' : 'fullscreen=on',
    
    "class~'(.*[Rr]xvt.*|.*[Tt]erm|Konsole)'" : 'focus=on',

    "windowtype~'_NET_WM_WINDOW_TYPE_(DIALOG|UTILITY|SPLASH)'" : 'pseudotile=on',
  # "windowtype='_NET_WM_WINDOW_TYPE_DIALOG'" : 'focus=on',
    "windowtype='_NET_WM_WINDOW_TYPE_DIALOG'" : 'fullscreen=on ',
    "windowtype~'_NET_WM_WINDOW_TYPE_(NOTIFICATION|DOCK|DESKTOP)'" : 'manage=off'
}

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# helper

def hc(arguments):
    os.system("herbstclient "+arguments)

def do_config(command, dictionary):
    # loop over dictionary
    for key, value in dictionary.items():
        hc(command+' '+key+' '+value)

        # uncomment to debug in terminal
        # print(command+' '+key+' '+value)

# tags related

def set_tags_with_name():
    hc("rename default '" + str(tag_names[0]) + "' 2>/dev/null || true")
    
    for index, tag_name in enumerate(tag_names):
        hc("add '" + str(tag_names[index]) + "'")
        
        # uncomment to debug in terminal
        # print(index)

        key = tag_keys[index];
        if key:
            hc("keybind Mod4-" + str(key) 
                + " use_index '" + str(index) + "'")
            hc("keybind Mod4-Shift-" + str(key) 
                + " move_index '" + str(index) + "'")

# miscellanous

# I don't understand what this is
def bind_cycle_layout():
    # The following cycles through the available layouts
    # within a frame, but skips layouts, if the layout change 
    # wouldn't affect the actual window positions.
    # I.e. if there are two windows within a frame,
    # the grid layout is skipped.

    hc( "keybind Mod4-space " \
        "or , and . compare tags.focus.curframe_wcount = 2 " \
        ". cycle_layout +1 vertical horizontal max vertical grid " \
        ", cycle_layout +1 ")

# do multi monitor setup here, e.g.:
# hc("set_monitors 1280x1024+0+0 1280x1024+1280+0")
# or simply:
# hc("detect_monitors")

# find the panel

def do_panel():
    dirname = os.path.dirname(os.path.abspath(__file__))
    panel   = dirname + "/panel-lemonbar.py"

    if not os.path.isfile(panel) and os.access(panel, os.X_OK):
        panel = "/etc/xdg/herbstluftwm/panel.sh"
    
    raw = os.popen('herbstclient list_monitors | cut -d: -f1').read()
    monitors = raw.strip().split("\n")

    for monitor in (monitors):
        os.system(panel + ' ' + str(monitor) + ' &');

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# load on startup

def startup_run():
    command = 'silent new_attr bool my_not_first_autostart'
    exitcode = os.system('herbstclient ' + command)

    if exitcode == 0:
      # non windowed app
        os.system("compton &")
        os.system("dunst &")
        os.system("parcellite &")
        os.system("nitrogen --restore &")
        os.system("mpd &")

      # windowed app
        os.system("xfce4-terminal &")
        os.system("sleep 1 && firefox &")
        os.system("sleep 2 && geany &")
        os.system("sleep 2 && thunar &")


# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# main

# background before wallpaper
os.system("xsetroot -solid '"+color['blue500']+"'")

# Read the manual in $ man herbstluftwm
hc('emit_hook reload')

# gap counter
os.system("echo 35 > /tmp/herbstluftwm-gap");

# do not repaint until unlock
hc("lock");

# standard
# remove all existing keybindings
hc('keyunbind --all')
hc("mouseunbind --all")
hc("unrule -F")

set_tags_with_name()

# do hash config
do_config("keybind",   keybinds)
do_config("keybind",   tagskeybinds)
do_config("mousebind", mousebinds)
do_config("attr",      attributes)
do_config("set",       sets)
do_config("rule",      rules)

# finishing, some extra miles

# I'm not sure what in this is
bind_cycle_layout()

# example of custom layout
layout = "(split horizontal:0.5:0 " \
         "(clients vertical:0) (clients vertical:0))"
hc("load " + str(tag_names[0]) + " '" + layout + "'")

# tag number 5
hc("floating 5 on")

# hc("set tree_style '╾│ ├└╼─┐'")
hc("set tree_style '⊙│ ├╰»─╮'")

# unlock, just to be sure
hc("unlock")

# launch statusbar panel (e.g. dzen2 or lemonbar)
do_panel()

# load on startup
startup_run()
