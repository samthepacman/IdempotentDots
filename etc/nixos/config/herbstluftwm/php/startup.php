<?php # using PHP7
# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# load on startup

function startup_run() 
{
    $command = 'silent new_attr bool my_not_first_autostart';
    system("herbstclient $command", $exitcode);

    # without redirection your app WILL HANG !
    if ( ! ((bool) trim($exitcode)) ) {
      # non windowed app
        system("compton > /dev/null &");
        system("dunst > /dev/null &");
        system("parcellite > /dev/null &");
        system("nitrogen --restore > /dev/null &");
        system("mpd > /dev/null &");

      # windowed app
        exec("xfce4-terminal > /dev/null 2>&1 &");
        exec("firefox > /dev/null 2>&1 &");
        exec("geany > /dev/null 2>&1 &");
        exec("thunar > /dev/null 2>&1 &");
    }
}
