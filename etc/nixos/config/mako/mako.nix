{ pkgs, lib, config, ... }:

{
    programs.mako = {
        enable = true;
        width = 280;
        margin = "4";
        padding = "8";
        borderSize = 1;
        borderRadius = 4;
        defaultTimeout = 5000;
        # borderColor = "#0b0b0d";
        borderColor = "#282c34";
        backgroundColor = "#282c34";
        anchor = "top-left";
        layer = "overlay";
        font = "Iosevka Nerd Font Mono 11";
      };
  }
