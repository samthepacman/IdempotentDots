#!/usr/bin/env bash

MUSIC_DIR="$(cat ~/.config/ncmpcpp/config | grep 'mpd_music_dir' | awk -F'"' '{print $2}')"
COVER=/tmp/cover.jpg

{
    album="$(mpc --format %album% current -p 6600)"
    file="$(mpc --format %file% current -p 6600)"
    album_dir="${file%/*}"
    [[ -z "$album_dir" ]] && exit 1
    album_dir="$MUSIC_DIR/$album_dir"
    
    covers="$(find "$album_dir" -type d -exec find {} -maxdepth 1 -type f -iregex ".*/.*\(${album}\|cover\|folder\|artwork\|front\).*[.]\(jpe?g\|png\|gif\|bmp\)" \; )"
    src="$(echo -n "$covers" | head -n1)"
    rm -f "$COVER" 
    
    # For Notifications
    if [[ -n "$src" ]] ; then
        # Resize the image's width to 70px
        convert "$src" -resize 70x "$COVER"
        ~/.scripts/notify/notify-send.sh -u normal -r 7777 -i ${COVER} "  Now Playing" "`mpc --format '[%artist%\n%title% | %file%]' current`"
    else
        # Fallback
        ~/.scripts/notify/notify-send.sh -u normal -r 7777 -i musique "Now Playing" "`mpc --format '[%artist%\n%title% | %file%]' current`"
    fi
} &
