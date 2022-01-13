#!/usr/bin/perl
# This is a modularized config for herbstluftwm

# See here for more sophisticated code
# https://github.com/ypnos/hlwm/blob/master/autostart

use warnings;
use strict;

use File::Basename;
use lib dirname(__FILE__);

use gmc;
use helper;
use config;
use startup;

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# main

# background before wallpaper
system("xsetroot -solid '$color{'blue500'}'");

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

helper::set_tags_with_name();

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# do hash config

helper::do_config("keybind",   %config::keybinds);
helper::do_config("keybind",   %config::tagskeybinds);
helper::do_config("mousebind", %config::mousebinds);
helper::do_config("attr",      %config::attributes);
helper::do_config("set",       %config::sets);
helper::do_config("rule",      %config::rules);

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# finishing, some extra miles

# I'm not sure what in this is
helper::bind_cycle_layout();

# example of custom layout
my $layout = "(split horizontal:0.5:0 "
            ."(clients vertical:0) (clients vertical:0))";
hc("load $tag_names[0] '$layout'");

# tag number 5
hc("floating 5 on");

# hc("set tree_style '╾│ ├└╼─┐'");
hc("set tree_style '⊙│ ├╰»─╮'");

# unlock, just to be sure
hc("unlock");

# launch statusbar panel (e.g. dzen2 or lemonbar)
helper::do_panel();

# load on startup
startup::run();
