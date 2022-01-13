{ pkgs, ...}:
''
#!/usr/bin/env bash
USAGE=$(cat <<ENDSTR
Usage: swaymode <command> 
  Commands:

      1. windows /win                -  Windows 10 - like layout
      2. mac-os / mac                -  Mac OS like layout
      3. alucard / hellsing          -  My Favourite on so far.
      4. pop / cosmic                -  Pop OS Cosmic in SWAY-WM
      5. gnome / ubuntu              -  Gnome like layout
      6. modular / blocks            -  Block like Panel theme
      7. solo-leveling / leveling    -  Theme based on Solo-Leveling
      8. legend / dragon             -  A Old age style theme
      9. candy / eyecandy            -  Cool candy like theme
-
ENDSTR
)

command="$1"
shift
case "$command" in

 help|--help)
         echo $VERSION
  echo "$USAGE"
  ;;
#...........................................
windows|win)
  pkill swaybg && \
  pkill waybar && \
  . /etc/nixos/config/home/modes/windows/activate \
  && sleep 0.3;clear
;;

mac-os|mac)
  pkill swaybg && \
  pkill waybar && \
  . /etc/nixos/config/home/modes/mac-os/activate && \
  sleep 0.3;clear
;;

modular|block)
  pkill swaybg && \
  pkill waybar && \
  . /etc/nixos/config/home/modes/modular/activate && \
  sleep 0.3;clear
;;

cosmic|pop)
  pkill swaybg && \
  pkill waybar && \
  . /etc/nixos/config/home/modes/pop-os/activate && \
  sleep 0.3;clear
;;

gnome|ub)
  pkill swaybg && \
  pkill waybar && \
  . /etc/nixos/config/home/modes/gnome/activate && \
  sleep 0.3;clear
;;

min|minimal)
  pkill swaybg && \
  pkill waybar && \
  . /etc/nixos/config/home/modes/minimal/activate && \
  sleep 0.3;clear
;;

skull|retro)
  pkill swaybg && \
  pkill waybar && \
  . /etc/nixos/config/home/modes/retro-skull/activate && \
  sleep 0.3;clear
;;

alucard|hellsing)
  pkill swaybg && \
  pkill waybar && \
  . /etc/nixos/config/home/modes/alucard/activate && \
  sleep 0.3;clear
;;

legend|dragon)
  pkill swaybg && \
  pkill waybar && \
  . /etc/nixos/config/home/modes/dragon/activate && \
  sleep 0.3;clear
;;

solo-leveling|leveling)
  pkill swaybg && \
  pkill waybar && \
  . /etc/nixos/config/home/modes/solo-leveling/activate && \
  sleep 0.3;clear
;;

candy|eyecandy)
  pkill swaybg && \
  pkill waybar && \
  . /etc/nixos/config/home/modes/candy/activate && \
  sleep 0.3;clear
;;
#...........................................
	*)
		[ -z $command ] || echo Unknown command: $command
		echo "$USAGE"
		;;
  esac
  ''
