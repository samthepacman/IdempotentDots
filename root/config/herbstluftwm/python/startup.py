import os

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# load on startup

def run():
    command = 'silent new_attr bool my_not_first_autostart'
    exitcode = os.system('herbstclient ' + command)

    if exitcode == 0:
      # non windowed app
        os.system("systemctl --user restart picom &")
        os.system("dunst &")
        os.system("parcellite &")
        os.system("sh ~/.fehbg")
        os.system("sh ~/.config/polybar/launch.sh")
        # os.system("sh ~/.config/herbstluftwm/scripts/lmms.sh")
        os.system("mpd &")

      # windowed app
       # os.system("xfce4-terminal &")
       # os.system("sleep 1 && firefox &")
       # os.system("sleep 2 && geany &")
       # os.system("sleep 2 && thunar &")
