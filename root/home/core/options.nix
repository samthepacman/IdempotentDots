{ pkgs, config, lib, ... }:
let
 hm = import ./variables.nix;
  
  in
{

imports = [
          # hm.neomutt
          # hm.surf
          # hm.doom
          # hm.nur

        # SHELL
            hm.shell.nushell
            hm.shell.zsh
            hm.shell.fish

        # PROGRAMS
        #    hm.prog.neovim
            hm.prog.ncmpcpp

        # SHELL UTILITIES
            hm.shellutil.tmux
           # hm.shellutil.starship

        # APPS AND DIRECTORIES
            hm.misc.apps
            hm.misc.xdg
           
        # BROWSERS
            hm.web.qutebrowser
            hm.web.chromium

        # TERMINAL EMULATORS
            hm.term.foot
            hm.term.kitty

        # DESKTOP COMPONENTS
            hm.ui.sway
            hm.ui.waybar
            hm.notif.mako
     ];
   }
