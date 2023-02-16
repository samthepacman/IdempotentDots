{ config, pkgs, ...}:
{
  programs.waybar = {
    systemd.enable = false;
    enable = true;
    settings = [
      {
        layer = "top";
        position = "bottom";
        height = 24;
        modules-left = [ "sway/workspaces" "custom/sep" "sway/window" ];
        # modules-center = [ "custom/sep" "memory" ];
        # modules-right = [ "mpd" "memory" "cpu" "network" "temperature" "pulseaudio" "clock" ];
        # modules-right = [ "mpd" "custom/space" "memory" "custom/space" "cpu" "custom/space" "network" "custom/space" "temperature" "custom/space" "pulseaudio" "custom/space" "clock" ];
        modules-right = [ "memory" "custom/sep" "cpu" "custom/sep" "mpd" "custom/sep" "network" "custom/sep" "pulseaudio" "custom/sep" "clock" ];
         modules = {
       "sway/workspaces" = {
            disable-scroll = false;
            all-outputs = true;
            format = "{icon}";
            persistent_workspaces = {
               "1" = [];
               "2" = [];
               "3" = [];
               "4" = [];
               "5" = [];
               "6" = [];
               "7" = [];
               "8" = [];
               "9" = [];
            };
           "format-icons" = {
             "1" = "Ter";
             "2" = "Mus";
             "3" = "Fil";
             "4" = "Dev";
             "5" = "Env";
             "6" = "Web";
             "7" = "Mda";
             "8" = "Cnf";
             "9" = "Msc";

            #   "1" = "";
  	        #   "2" = "";
  	        #   "3" = "";
  	        #   "4" = "";
  	        #   "5" = "";
            #   "6" = "";
            #   "7" = "";
            #   "8" = "";
            #   "9" = "";


               "urgent" = " ";
               "focused" = " ";
               "default" = " ";
           };
        };
       "clock" = {
           tooltip-format = "<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>";
           format = "Time : {:%H:%M}  ";
           format-alt = "Date : {:%Y-%m-%d}  ";
       };
       "network" = {
           interface = "wlp2*";
           format-wifi = "Net : {signalStrength}";
           format-ethernet = "{ifname}: {ipaddr}/{cidr} ";
           format-linked = "{ifname} (No IP) ";
           format-disconnected = "N:Disconnected";
           format-alt = "{ifname}: {ipaddr}/{cidr}";
           tooltip-format = "{essid}";
       };

       "custom/sep" = {
           format = " | ";
           tooltip = false;
       };

#       "custom/sys" = {
#           format = "  ";
#           tooltip-format = "App Menu";
#           on-click = "wofi --show drun";
#       };

       "mpd" = {
           format = "Mpd : {title} {stateIcon} ";
           format-disconnected = "Mpd : Disconnected";
           format-stopped = "{consumeIcon}{randomIcon}{repeatIcon}{singleIcon}Stopped ";
           unknown-tag = "N/A";
           interval = 2;
           consume-icons = {
               on = " ";
             };

           random-icons = {
               off = "<span color=\"#f53c3c\"> </span> ";
               on = "  ";
             };

           repeat-icons = {
               on = " ";
             };

           single-icons = {
               on = "1 ";
             };

           state-icons = {
               playing = " ";
               paused = " ";
             };

           tooltip-format = "MPD (connected)";
           tooltip-format-disconnected = "MPD (disconnected)";
        };

        "cpu" = {
           format = "Cpu : {usage}%";
           interval = 1;
        };

      #  "temperature" = {
      #   critical-threshold = 80;
      #   format = " {icon} {temperatureC}°C";
      #   format-icons = [" " " " " "];
      #   };
      #
      # "custom/space" = {
      #     format = "  ";
      #     tooltip = false;
      # };
      
        "memory" = {
            format = "Mem : {}%";
            interval = 1;
        };

       "pulseaudio" = {
           format = "{icon} {volume}";
           format-bluetooth = "{volume}% {icon} {format_source}";
           format-bluetooth-muted = " {icon} {format_source}";
           format-muted = " {format_source}";
           format-source = "{volume}% ";
           format-source-muted = "";
           format-icons = {
               default = ["Vol :"];
           };
           on-click = "exec foot pulsemixer";
       };
     };
    }
   ];
    style = ''
       * {
              border: transparent;
              border-radius: 0px;
              font-family: Iosevka Nerd Font Mono, Font Awesome, Arial, sans-serif;
              font-weight: 400;
              font-size: 14px;
              min-height: 0;
          }

          window#waybar {
              background-color: #0b0b0d;
              color: #abb2bf;
          }

          #custom-activities {
              background-color: transparent;
              color: #abb2bf;
          }

          #workspaces {
              padding: 0 0px;
              color: #839496;
              margin: 1px 1px 1px 1px;
              color: #1e2127;
          }

          #workspaces button {
              padding: 0px 6px;
              color: #323439;
          }

          #workspaces button.focused {
              padding: 0px 6px;
              color: #2ea690;
          }

          #workspaces button.occupied {
              padding: 0px 6px;
              color: #E95379;
          }

          #clock {
            color: #657dd1;
          }

          #window {
            color: #d47979;
          }

          #pulseaudio {
            color: #22e788;
          }

          #network {
            color: #e8cd97;
          }

          #custom-sep {
            color: #4e5159;
          }

          #cpu {
            color: #81a2be;
          }

          #memory {
            color: #B877DB;
          }

          #mpd {
            color: #d162cd;
          }

          #temperature {
            color: #E9436F;
          }

          tooltip {
            color: #abb2bf;
            background-color: #0b0b0d;
            text-shadow: none;
          }
          
          tooltip * {
            color: #abb2bf;
            text-shadow: none;
          }
          '';
    };
}
