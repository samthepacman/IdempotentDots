#!/usr/bin/env bash
## resize {up,down,east,west} delta
#
delta=${2:-"30"}

case $1 in
   right)  dim=w; sign=+ ;;
   left)   dim=w; sign=- ;;
   up)     dim=h; sign=- ;;
   down)   dim=h; sign=+ ;;
   *) echo "Usage: resize {up,down,left,right} [delta]" && exit 1 ;;
 esac

 x=0; y=0;
 case $dim in
   w) x=$sign$delta;  dir=right;  falldir=left   ;;
   h) y=$sign$delta;  dir=top;    falldir=bottom ;;
 esac

 bspc node -z "$dir" "$x" "$y" || bspc node -z "$falldir" "$x" "$y";


