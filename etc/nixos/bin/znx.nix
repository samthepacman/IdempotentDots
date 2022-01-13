{ pkgs, ... }:

''
#! /usr/bin/env cached-nix-shell
#! nix-shell -i bash -p bash
USAGE=$(cat <<ENDSTR
Commands:
 
   # SYSTEM MANAGEMENT
   rebase / switch - rebuild NixOS
   upgrade - upgrade NixOS
   conf-edit / conf-ed - list all System Config

   # FLAKE MANAGEMENT
   flake-edit / flk-ed - edit Flake config    
   flake-lock / flk-lc - recreate system flake lock [ not available ]
   flake-update / flk-up - update root flake

   # MANUAL GARBAGE COLLECTIONS
   garbage-collect / gc - clean nix store home
   garbage-collect -d / gcd - clean nix store root

   # HOME-MANAGER ALIASES
   home-install / hm-in - go to Home-Manager Dir
   home-switch / hm-sw - rebuild Home-Manager configs
   home-gen / hm-gen - list hm generations
   home-remove  / hm-rm  remove hm generations
   home-expire / hm-ex  remove hm in a specific time gap

   # PACKAGE MANAGEMENT
   install / it - install package
   pull / pnr - update repo
   env - nix-env
   remove / rm - remove packages
 
ENDSTR
)
sudo=$(which sudo 2> /dev/null|cut -d ':' -f 2)
if [ $(id -u) -eq 0 ]; then sudo=""; fi
command="$1"
shift
case "$command" in
 help|--help)
         echo $VERSION
  echo "$USAGE"
  ;;
#...........................................
################# OS 
rebase | switch)
$sudo nixos-rebuild switch --flake /etc/nixos
;;
upgrade)
$sudo nixos-rebuild switch --upgrade --flake /etc/nixos
;;
conf-edit | conf-ed)
$sudo vim /etc/nixos
;;
################# FLAKE
flake-edit | flk-ed)
$sudo vim /etc/nixos/flake.nix
;;
flake-lock | flk-lc)
$sudo nix flake update /etc/nixos
;;
flake-update | flk-up )
$sudo nix flake lock /etc/nixos
;;
################## GC
garbage-collect | gc)
nix-collect-garbage
;;
garbage-collect-d | gcd)
nix-collect-garbage -d
;;
################### HOME-MANAGER
# home-install | hm-in)
# nix-shell '<home-manager>' -A install
# ;;
home-switch | hm-sw)
home-manager switch
;;
home-gen | hm-gen)
home-manager generations
;;
#home-remove | hm-rem)
#home-manager remove-generations
#;;
#home-expire | hm-ex)
#home
#;;
######################## PACKAGES
install | it)
nix-env -iA nixos."$@"
;;
pull | pnr)
nix-channel --update
;;
env)
nix-env "$@" 
;;
remove | rm)
nix-env -e "$@"
;;
list | lspkg)
nix-env -q
;;
#...........................................
 *)
  [ -z $command ] || echo Unknown command: $command
  echo "$USAGE"
  ;;
esac
''
