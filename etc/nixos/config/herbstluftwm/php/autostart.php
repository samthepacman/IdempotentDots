#!/usr/bin/php
<?php # using PHP7
# This is a modularized config for herbstluftwm

require_once('gmc.php');
require_once('helper.php');
require_once('config.php');
require_once('startup.php');

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# main

# background before wallpaper
system("xsetroot -solid '".COLOR['blue500']."'");

# Read the manual in $ man herbstluftwm
hc('emit_hook reload');

# gap counter
system("echo 35 > /tmp/herbstluftwm-gap");

# do not repaint until unlock
hc("lock");

# standard
# remove all existing keybindings
hc('keyunbind --all');
hc("mouseunbind --all");
hc("unrule -F");

set_tags_with_name();

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# do hash config

do_config("keybind",   $keybinds);
do_config("keybind",   $tagskeybinds);
do_config("mousebind", $mousebinds);
do_config("attr",      $attributes);
do_config("set",       $sets);
do_config("rule",      $rules);

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# finishing, some extra miles

# I'm not sure what in this is
bind_cycle_layout();

# example of custom layout
$layout = "(split horizontal:0.5:0 "
         ."(clients vertical:0) (clients vertical:0))";
hc("load $tag_names[0] '$layout'");

# tag number 5
hc("floating 5 on");

# hc("set tree_style '╾│ ├└╼─┐'");
hc("set tree_style '⊙│ ├╰»─╮'");

# unlock, just to be sure
hc("unlock");

# launch statusbar panel (e.g. dzen2 or lemonbar)
do_panel();

# load on startup
startup_run();


