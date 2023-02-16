{ pkgs, lib, config, ... }:

{
    programs.mako = {
        enable = true;
        width = 280;
        margin = "2";
        padding = "8";
        borderSize = 1;
        borderRadius = 0;
        defaultTimeout = 5000;
        # borderColor = "#0b0b0d";
        borderColor = "#1f1f1f";
        backgroundColor = "#1f1f1f";
        anchor = "top-right";
        layer = "overlay";
        font = "Iosevka Nerd Font Mono 11";
      };
  }
