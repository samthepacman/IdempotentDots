import os
import os.path

import config
from config import tag_names, tag_keys

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# helpers

def hc(arguments):
    os.system("herbstclient "+arguments)

def do_config(command, dictionary):
    # loop over dictionary
    for key, value in dictionary.items():
        hc(command+' '+key+' '+value)

        # uncomment to debug in terminal
        # print(command+' '+key+' '+value)

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
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

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
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

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
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
