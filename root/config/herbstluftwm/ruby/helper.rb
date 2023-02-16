require_relative 'config'

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# helpers

def hc(arguments)
  system("herbstclient #{arguments}")
end

def do_config(command, hash)
  # loop over hash   
  hash.each do |key, value|
    hc(command+' '+key+' '+value)

    # uncomment to debug in terminal
    # puts(command+' '+key+' '+value)
  end
end

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# tags related

def set_tags_with_name()
  tag_names = Config::Tag_names
  tag_keys = Config::Tag_keys

  hc("rename default '#{tag_names[0]}' 2>/dev/null || true")
    
  tag_names.each_with_index do |tag_name, index|
    hc("add '#{tag_names[index]}'")
        
    # uncomment to debug in terminal
    # puts i

    key = tag_keys[index]                
    unless key.to_s.empty?
      hc("keybind Mod4-#{key} use_index '#{index}'");
      hc("keybind Mod4-Shift-#{key} move_index '#{index}'");
    end
  end
end

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# miscellanous

# I don't understand what this is
def bind_cycle_layout()
  # The following cycles through the available layouts
  # within a frame, but skips layouts, if the layout change 
  # wouldn't affect the actual window positions.
  # I.e. if there are two windows within a frame,
  # the grid layout is skipped.

  hc( "keybind Mod4-space " \
      "or , and . compare tags.focus.curframe_wcount = 2 " \
      ". cycle_layout +1 vertical horizontal max vertical grid " \
      ", cycle_layout +1 ")
end

# do multi monitor setup here, e.g.:
# hc("set_monitors 1280x1024+0+0 1280x1024+1280+0")
# or simply:
# hc("detect_monitors")

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# find the panel

def do_panel()
  panel = __dir__ + "/panel-lemonbar.rb"

  if not File.exist?(panel) and File.executable?(panel)
    panel = "/etc/xdg/herbstluftwm/panel.sh"
  end
   
  raw = IO.popen('herbstclient list_monitors | cut -d: -f1').read()
  monitors = raw.split("\n")

  for monitor in (monitors)
    system("#{panel} #{monitor} &")
  end
end
