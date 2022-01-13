package startup;

use warnings;
use strict;
use Exporter;

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# load on startup

sub run() {
    my $command = 'silent new_attr bool my_not_first_autostart';

    my $not_first_qx = qx(herbstclient $command);
    my $exitcode = $?;

    if ($exitcode == 0) {
      # non windowed app
        system("compton &");
        system("dunst &");
        system("parcellite &");
        system("nitrogen --restore &");
        system("mpd &");
    
      # windowed app
        system("xfce4-terminal &");
        system("sleep 1 && firefox &");
        system("sleep 2 && geany &");
        system("sleep 2 && thunar &");
    }
}

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# end of perl module

1;
