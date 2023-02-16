{ config, pkgs, hostName, lib, ...}:
let
  clr = import ../../themes/generic.nix;
in
  {
  wayland.windowManager.sway = {
    enable = true;

    extraConfig = ''
         for_window [app_id="^launcher$"] floating enable, sticky enable, resize set 28 ppt 90 ppt, border pixel 6
         for_window [app_id="^file$"] floating enable, sticky enable, resize set 60 ppt 50 ppt, border pixel 6, move y -10px
         for_window [app_id="^connman$"] floating enable, sticky enable, resize set 60 ppt 50 ppt, border pixel 0, opacity 0.9
         for_window [app_id="^imv$"] fullscreen enable, border pixel 0, opacity 0.95
'';

    config = {
       down = "j";
       up = "k";
       left = "h";
       right = "l";
       modifier = "Mod4";

       startup = [{ command = "mpd"; }
                  # { command = "foot --server"; }
                  { command = "/home/sam/.swayrc"; }
                 ];

       bars = [{ command = "none"; }];
       colors = {
         focused = {
               background = clr.selected.bgd;
               border = clr.selected.bor;
               text = clr.selected.txt;
               indicator = clr.selected.ind;
               childBorder = clr.selected.cbr;
            };
            focusedInactive = {
               background = clr.inactive.bgd;
               border = clr.inactive.bor;
               text = clr.inactive.txt;
               indicator = clr.inactive.ind;
               childBorder = clr.inactive.cbr;
            };
            placeholder = {
               background = clr.placeholder.bgd;
               border = clr.placeholder.bor;
               text = clr.placeholder.txt;
               indicator = clr.placeholder.ind;
               childBorder = clr.placeholder.cbr;
            };
            urgent = {
               background = clr.urgent.bgd;
               border = clr.urgent.bor;
               text = clr.urgent.txt;
               indicator = clr.urgent.ind;
               childBorder = clr.urgent.cbr;
            };
            unfocused = {
               background = clr.unfocused.bgd;
               border = clr.unfocused.bor;
               text = clr.unfocused.txt;
               indicator = clr.unfocused.ind;
               childBorder = clr.unfocused.cbr;
            };
         };

       window.border = 4;
       focus.followMouse = true;
       gaps = {
         outer = 5;
         inner = 5;
         smartBorders = "off";
         smartGaps = false;
       };

       keybindings = let
         mod = "Mod4";
         term = "exec footclient";
         term-alt = "exec kitty";
         in lib.mkOptionDefault {
                  # DOUBLE KEY - BINDS
                  "${mod}+q" = "kill";
                  "${mod}+Return" = "exec wofi --show drun";
                  "${mod}+p" = "exec sh ~/.config/wofer/wofer wofi --dmenu";
                  "${mod}+d" = "exec foot --app-id=file ranger";

                  # MODES
                  "${mod}+m" = "mode open";
                  "${mod}+r" = "mode resize";

                  # TRIPLE KEY - BINDS
                  "${mod}+Shift+Return" = "exec footclient";
                  "${mod}+Grave" = "exec kitty";
                  "${mod}+Control+Alt+l" = "exec swaylock -C ~/.config/nixpkgs/core/sway/recipes/swaylock/lock.conf";

                  # ARROW KEY RESIZE
                  "${mod}+Control+Right" = "resize grow width 10 px";
                  "${mod}+Control+Left" = "resize shrink width 10 px";
                  "${mod}+Control+Down" = "resize grow height 10 px";
                  "${mod}+Control+Up" = "resize shrink height 10 px";

                  # VIM KEYS RESIZE
                  "${mod}+Control+h" = "resize shrink width 50 px";
                  "${mod}+Control+j" = "resize grow height 50 px";
                  "${mod}+Control+k" = "resize shrink height 50 px";
                  "${mod}+Control+l" = "resize grow width 50 px";

                  # SPLITS
                  "${mod}+a" = "splitv";
                  "${mod}+z" = "splith";
                  "${mod}+u" = "splitv";
                  "${mod}+o" = "splith";

};
       modes = {
         open  = {
                    # r = "exec wf-recorder";
                    # x = "exec pkill wf-recorder";
                    f = "exec footclient --app-id=file ranger";
                    b = "exec firefox";
                    q = "exec qutebrowser";
                    m = "exec element-desktop";
                    t = "exec kitty";
                    n = "exec connman-gtk --no-icon";
                    Escape = "mode default";
                  };

         resize  = {
                  h = "resize shrink width 50 px";
                  j = "resize grow height 50 px";
                  k = "resize shrink height 50 px";
                  l = "resize grow width 50 px";
                  Escape = "mode default";
                  };
                };
              };
            };
}
