#!/usr/bin/env bash
# This shell script is PUBLIC DOMAIN. You may do whatever you want with it.

TOGGLE=$HOME/.toggle

if [ ! -e $TOGGLE ]; then
    touch $TOGGLE
     lavalauncher -c ~/.config/nixpkgs/core/sway/recipes/lavalauncher/poweroff.conf
else
    rm $TOGGLE
    pkill lavalauncher
fi

