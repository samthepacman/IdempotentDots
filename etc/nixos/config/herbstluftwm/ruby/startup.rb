# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# load on startup

def startup_run()
  command = 'silent new_attr bool my_not_first_autostart'
  system("herbstclient #{command}")
  exitcode = $?.exitstatus

  if (exitcode == 0)
  # non windowed app
    system("compton &")
    system("dunst &")
    system("parcellite &")
    system("nitrogen --restore &")
    system("mpd &")

  # windowed app
    system("xfce4-terminal &")
    system("sleep 1 && firefox &")
    system("sleep 2 && geany &")
    system("sleep 2 && thunar &")
  end
end

