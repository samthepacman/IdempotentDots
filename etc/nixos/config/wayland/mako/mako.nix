{ pkgs, lib, config, ... }:

{
    programs.mako = {
        enable = true;
        width = 350;
        margin = "2";
        padding = "8";
        borderSize = 1;
        borderRadius = 0;
        defaultTimeout = 5000;
        # borderColor = "#0b0b0d";
        borderColor = "#212226";
        backgroundColor = "#0b0b0d";
        anchor = "top-right";
        layer = "overlay";
        font = "Iosevka Nerd Font Mono 11";
      };
  }
