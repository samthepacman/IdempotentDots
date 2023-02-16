{ pkgs, config, lib, ...}:
let
  clr = import ../themes/bijutsu/bijutsu.nix;
in
{
  programs.kitty = { 
    enable = true;
    font = { 
      name = "JetbrainsMono Nerd Font";
      size = 10;
    };
   
      settings = {
 
        shell = "zsh";
        foreground = clr.foreground;
        background = clr.background;
	confirm_os_window_close = "0";
        selection_foreground = "#2e3440";
        selection_background = "#d8dee9";
        url_color = "#0087BD";
        background_opacity = "1.0";
        cursor = "#abb2bf";
        window_padding_width = "0";
        enable_audio_bell = "no";


            color0 = clr.black;  # black
            color1 = clr.red;  # red
            color2 = clr.green;  # green
            color3 = clr.yellow;  # yellow
            color4 = clr.blue;  # blue
            color5 = clr.magenta;  # magenta
            color6 = clr.cyan;  # cyan
            color7 = clr.white;  # white
            
            color8 = clr.black-br;   # bright black
            color9 = clr.red-br;   # bright red
            color10 = clr.green-br;   # bright green
            color11 = clr.yellow-br;   # bright yellow
            color12 = clr.blue-br;   # bright blue
            color13 = clr.magenta-br;   # bright magenta
            color14 = clr.cyan-br;   # bright cyan
            color15 = clr.white-br;   # bright white
    };
  };
} 
