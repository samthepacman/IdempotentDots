#!/usr/bin/env bash

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
