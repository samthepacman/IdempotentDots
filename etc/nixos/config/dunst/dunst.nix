{ pkgs, ... }:
{
  services.dunst = {
    enable = true;

    iconTheme = {
      name = "Paper";
      package = pkgs.paper-icon-theme;
    };

    settings = {
      global = {
        font = "Iosevka 11";
        markup = "full";
        # format = "<b><span foreground='${colours.base11}'>%a</span></b>\n<b>%s</b>\n<span style='font-style=italic;'>%b</span>";
        sort = "yes";
        indicate_hidden = "yes";
        alignment = "left";
        bounce_freq = 5;
        show_age_threshold = 60;
        word_wrap = "no";
        ignore_newline = "no";
        geometry = "460x6-10+0";
        transparency = 0;
        idle_threshold = 120;
        sticky_history = "yes";
        icon_position = "left";
        max_icon_size = 128;
        line_height = 8;
        separator_height = 2;
        padding = 8;
        horizontal_padding = 8;
        separator_color = "frame";
        startup_notification = false;
        show_indicators = "yes";
        frame_width = 1;
        corner_radius = 0;

        shadow-exclude = [
          "name = 'Notification'"
          "class_g ?= 'Dunst'"
          # disable shadows for hidden windows:
          "_NET_WM_STATE@:32a *= '_NET_WM_STATE_HIDDEN'"
          #  "_GTK_FRAME_EXTENTS@:c",
          # disables shadows on sticky windows:
          "_NET_WM_STATE@:32a *= '_NET_WM_STATE_STICKY'"
        ];
      };

      shortcuts = {
        close = "ctrl+space";
        close_all = "ctrl+shift+space";
        history = "ctrl+grave";
        context = "ctrl+shift+period";
      };

      urgency_low = {
        background = "#16161C";
        foreground = "#ffffff";
        frame_color = "#1c1c1c";
        timeout = 4;
      };

      urgency_normal = {
        background = "#16161C";
        foreground = "#ffffff";
        frame_color = "#1c1c1c";
        timeout = 6;
      };

      urgency_critical = {
        background = "#16161C";
        foreground = "#ffffff";
        frame_color = "#1c1c1c";
        timeout = 0;
        frame_width = 1;
      };
    };
  };
}

