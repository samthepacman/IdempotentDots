package helper;

use warnings;
use strict;

use config;

use Exporter;
our @ISA = 'Exporter';
our @EXPORT = qw(hc);

use File::Basename;
use lib dirname(__FILE__);

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# helpers

sub hc {
    system("herbstclient @_");
}

sub do_config($\%) {
    my ($command, $ref2hash) = @_;
    my %hash = %$ref2hash;

    # loop over hash
    while(my ($key, $value) = each %hash) { 
        hc("$command $key $value");
        
        # uncomment to debug in terminal
        # print("$command $key $value \n")
    }
}

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# tags related

sub set_tags_with_name() {
    hc("rename default '$tag_names[0]' 2>/dev/null || true");
    
    for my $index (0 .. $#tag_names) {
        hc("add '$tag_names[$index]'");
        
        # uncomment to debug in terminal
        # print $index."\n";

        my $key = $tag_keys[$index];
        if ("$key" ne "") {
            hc("keybind Mod4-$key use_index '$index'");
            hc("keybind Mod4-Shift-$key move_index '$index'");
        }
    }
}

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# miscellanous

# I don't understand what this is
sub bind_cycle_layout() {
    # The following cycles through the available layouts
    # within a frame, but skips layouts, if the layout change 
    # wouldn't affect the actual window positions.
    # I.e. if there are two windows within a frame,
    # the grid layout is skipped.

    hc( "keybind Mod4-space "
        ."or , and . compare tags.focus.curframe_wcount = 2 "
        .". cycle_layout +1 vertical horizontal max vertical grid "
        .", cycle_layout +1 ");
}

# do multi monitor setup here, e.g.:
# hc("set_monitors 1280x1024+0+0 1280x1024+1280+0");
# or simply:
# hc("detect_monitors");

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# find the panel

sub do_panel() {
    my $dirname = dirname(__FILE__);
    my $panel   = "$dirname/panel-lemonbar.pl";
    if (not -x $panel) { $panel = "/etc/xdg/herbstluftwm/panel.sh"; }

    my $monitor_qx = qx(herbstclient list_monitors | cut -d: -f1);
    my @monitors = split /\n/, $monitor_qx;

    for my $monitor (@monitors) {
        # start it on each monitor
        system("$panel $monitor &");
    }
}

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# end of perl module

1;
