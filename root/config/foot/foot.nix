{ pkgs, config, lib, ...}:
let
  clr = import ../themes/onedark/onedark.nix;
in
{
  programs.foot = { 
        enable = true;
        server.enable = true;
        settings = {
         main = { 
           font = "Iosevka Nerd Font:pixelsize=14";
           font-bold = "Iosevka Nerd Font:pixelsize=14";
           pad = "10x10";
           shell = "zsh";
         };

         scrollback = { 
           lines = "4096";
           multiplier = "1";
         };

         cursor = {
           style = "block";
           blink = "no";
         };

         mouse = {
           hide-when-typing = "yes";
         };

         colors = { 

            alpha = "1.0";
            background = clr.bg; #00252e
            foreground = clr.fg;

       # ONE-DARK

            regular0 = clr.regular0;     # black
            regular1 = clr.regular1;       # red
            regular2 = clr.regular2;     # green
            regular3 = clr.regular3;    # yellow
            regular4 = clr.regular4;      # blue
            regular5 = clr.regular5;   # magenta
            regular6 = clr.regular6;      # cyan
            regular7 = clr.regular7;     # white
            
            bright0 = clr.bright0;   # bright black
            bright1 = clr.bright1;     # bright red
            bright2 = clr.bright2;   # bright green
            bright3 = clr.bright3;  # bright yellow
            bright4 = clr.bright4;    # bright blue
            bright5 = clr.bright5; # bright magenta
            bright6 = clr.bright6;    # bright cyan
            bright7 = clr.bright7;   # bright white 
          };

          tweak = {
           # allow-overflowing-double-width-glyphs = true;
          };
      };
   };
}
