#!/usr/bin/ruby
# ------------------------------------------------------------------
#
#     Description: unified config for herbstluftwm lemonbar
#     Created by: Epsi Nurwijayadi <epsi.nurwijayadi@gmail.com)
#
#     Source
#     https://github.com/epsi-rns/dotfiles/tree/master/standalone/lemon-hlwm/ruby
#
#     Blog
#     http://epsi-rns.github.io/desktop/2017/06/11/herbstlustwm-event-idle-overview.html
#     http://epsi-rns.github.io/desktop/2017/06/05/herbstlustwm-tag-status-ruby.html
#     http://epsi-rns.github.io/desktop/2017/06/15/herbstlustwm-event-idle-ruby.html
#
# ------------------------------------------------------------------

require_relative 'gmc'
include GMC

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# helper

# script arguments
def get_monitor(arguments)
  # ternary operator
  arguments.length > 0 ? arguments[0].to_i : 0
end

# geometry calculation

def get_geometry(monitor)
  raw = IO.popen('herbstclient monitor_rect '+ monitor.to_s).read()

  if raw.to_s.empty?
    print('Invalid monitor ' + monitor.to_s)
    exit(1)
  end
    
  raw.split(' ')
end

def get_top_panel_geometry(height, geometry)
  # geometry has the format X Y W H
  return geometry[0].to_i, geometry[1].to_i, 
         geometry[2].to_i, height
end

def get_bottom_panel_geometry(height, geometry)
  # geometry has the format X Y W H
  return geometry[0].to_i + 0, (geometry[3].to_i - height), 
         geometry[2].to_i - 0, height
end

# lemon Parameters

def get_params_top(monitor, panel_height)
  # calculate geometry
  geometry = get_geometry(monitor)
  xpos, ypos, width, height = get_top_panel_geometry(
    panel_height, geometry)

  # geometry: -g widthxheight+x+y
  geom_res = "#{width}x#{height}+#{xpos}+#{ypos}"

  # color, with transparency    
  bgcolor = "'#aa000000'"
  fgcolor = "'#ffffff'"

  # XFT: require lemonbar_xft_git 
  font_takaop  = "takaopgothic-9"
  font_symbol  = "PowerlineSymbols-11"
  font_awesome = "FontAwesome-9"

  parameters = " -g #{geom_res} -u 2" \
               " -B #{bgcolor} -F #{fgcolor}" \
               " -f #{font_takaop} -f #{font_awesome} -f #{font_symbol}"
end

def get_params_bottom(monitor, panel_height)
  # calculate geometry
  geometry = get_geometry(monitor)
  xpos, ypos, width, height = get_bottom_panel_geometry(
    panel_height, geometry)

  # geometry: -g widthxheight+x+y
  geom_res = "#{width}x#{height}+#{xpos}+#{ypos}"

  # color, with transparency    
  bgcolor = "'#aa000000'"
  fgcolor = "'#ffffff'"

  # XFT: require lemonbar_xft_git 
  font_mono    = "monospace-9"
  font_symbol  = "PowerlineSymbols-11"
  font_awesome = "FontAwesome-9"

  parameters = " -g #{geom_res} -u 2" \
               " -B #{bgcolor} -F #{fgcolor}" \
               " -f #{font_mono} -f #{font_awesome} -f #{font_symbol}"
end

def get_lemon_parameters(monitor, panel_height)
  get_params_top(monitor, panel_height)
end

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# output

# initialize

# assuming $ herbstclient tag_status
# 	#1	:2	:3	:4	.5	.6	.7	.8	.9

# initialize variable segment
segment_windowtitle = '' # empty string
tags_status = []         # empty array

# custom tag names
@TAG_SHOWS = ['一 ichi', '二 ni', '三 san', '四 shi', 
  '五 go', '六 roku', '七 shichi', '八 hachi', '九 kyū', '十 jū']

# initialize variable segment
@segment_windowtitle = '' # empty string
@tags_status = []         # empty array
@segment_datetime    = '' # empty string

# decoration

@SEPARATOR = "%{B-}%{F#{COLOR['yellow500']}}|%{B-}%{F-}"

# Powerline Symbol
@RIGHT_HARD_ARROW = ""
@RIGHT_SOFT_ARROW = ""
@LEFT_HARD_ARROW  = ""
@LEFT_SOFT_ARROW  = ""

# theme
@PRE_ICON    = "%{F#{COLOR['yellow500']}}"
@POST_ICON   = "%{F-}"

# main

def get_statusbar_text(monitor)
  text = ''

  # draw tags
  #text << '%{l}'
  @tags_status.each { |tag_status| 
    text << output_by_tag(monitor, tag_status) }

  # draw date and time
  text << '%{c}'
  text << output_by_datetime()
 
  # draw window title
  text << '%{r}'
  text << output_by_title()
end

# each segments

def output_by_tag(monitor, tag_status)
  tag_index  = tag_status[1..1]
  tag_mark   = tag_status[0..0]
  tag_name   = @TAG_SHOWS[tag_index.to_i - 1] # zero based

  # ----- pre tag
    
  case tag_mark
  when '#'
    text_pre = "%{B#{COLOR['blue500']}}%{F#{COLOR['black']}}" \
             + "%{U#{COLOR['white']}}%{+u}#{@RIGHT_HARD_ARROW}" \
             + "%{B#{COLOR['blue500']}}%{F#{COLOR['white']}}" \
             + "%{U#{COLOR['white']}}%{+u}"
  when '+'
    text_pre = "%{B#{COLOR['yellow500']}}%{F#{COLOR['grey400']}}"
  when ':'
    text_pre = "%{B-}%{F#{COLOR['white']}}" \
             + "%{U#{COLOR['red500']}}%{+u}"
  when '!'
    text_pre = "%{B#{COLOR['red500']}}%{F#{COLOR['white']}}" \
             + "%{U#{COLOR['white']}}%{+u}"
  else
    text_pre = "%{B-}%{F#{COLOR['grey600']}}%{-u}"
  end

  # ----- tag by number

  # clickable tags
  text_name = "%{A:herbstclient focus_monitor \"#{monitor}\" && " \
            + "herbstclient use \"#{tag_index}\":} #{tag_name} %{A} "
    
  # non clickable tags
  # text_name = " #{tag_name} "
    
  # ----- post tag

  if tag_mark == '#'
    text_post = "%{B-}%{F#{COLOR['blue500']}}" \
              + "%{U#{COLOR['red500']}}%{+u}" \
              + @RIGHT_HARD_ARROW;
  else
    text_post = ""        
  end
  
  text_clear = '%{B-}%{F-}%{-u}'
     
  text_pre + text_name + text_post + text_clear
end

def output_by_title()
  text = "#{@segment_windowtitle} #{@SEPARATOR}  ";
end

def output_by_datetime()
  @segment_datetime
end

# setting variables, response to event handler

def set_tag_value(monitor)
  raw = IO.popen('herbstclient tag_status ' + monitor.to_s).read()
  @tags_status = raw.strip.split("\t")
end

def set_windowtitle(windowtitle)
  icon = @PRE_ICON  + '' + @POST_ICON 

  @segment_windowtitle = " #{icon} %{B-}" \
    + "%{F#{COLOR['grey700']}} #{windowtitle}"
end

def set_datetime()
    date_icon = @PRE_ICON + '' + @POST_ICON
    localtime = Time.now

    date_format = '%a %b %d'
    date_str  = localtime.strftime(date_format)
    date_text = "#{date_icon} %{B-}%{F#{COLOR['grey700']}} #{date_str}"

    time_icon = @PRE_ICON + '' + @POST_ICON
    time_format = '%H:%M:%S'
    time_str  = localtime.strftime(time_format)
    time_text = "#{time_icon} %{B-}%{F#{COLOR['blue500']}} #{time_str}"

    @segment_datetime = "#{date_text}  #{time_text}"
end

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# pipe handler

def handle_command_event(monitor, event) 
  # find out event origin
  column = event.split("\t")
  origin = column[0]
    
  tag_cmds = ['tag_changed', 'tag_flags', 'tag_added', 'tag_removed']
  title_cmds = ['window_title_changed', 'focus_changed']

  case origin
  when 'reload'
    os.system('pkill lemonbar')
  when 'quit_panel'
    exit
  when *tag_cmds       # splat operator
    set_tag_value(monitor)
  when *title_cmds     # splat operator
    title = column.length > 2 ? column[2] : ''
    set_windowtitle(title)
  when 'interval'
    set_datetime()
  end
end

def content_init(monitor, lemon_stdin)
  # initialize statusbar before loop
  set_tag_value(monitor)
  set_windowtitle('')
  set_datetime()
      
  text = get_statusbar_text(monitor)
  lemon_stdin.puts(text)
end

def content_event_idle(cat_stdin)
  pid_idle = fork do 
    # start an io
    command_in = 'herbstclient --idle'
  
    IO.popen(command_in, "r") do |io_idle|
      while io_idle do 
         # read next event
        event = io_idle.gets
        cat_stdin.print(event)
      end
      io_idle.close()
    end
  end

  Process.detach(pid_idle)  
end

def content_event_interval(cat_stdin)
  pid_interval = fork do 
    while true do
      cat_stdin.print "interval\n"
      sleep(1)
    end
  end

  Process.detach(pid_interval)  
end

def content_walk(monitor, lemon_stdin)
  # note the r+ mode for bidirectional
  IO.popen('cat', 'r+') do |io_cat| 

    content_event_idle(io_cat)
    content_event_interval(io_cat)

    while io_cat do 
      # read next event, trim newline
      event = (io_cat.gets).strip
      handle_command_event(monitor, event)
        
      text = get_statusbar_text(monitor)
      lemon_stdin.puts(text)
    end
  
  io_cat.close()
  end
end

def run_lemon(monitor, parameters)
  command_out  = 'lemonbar ' + parameters

  # note the r+ mode
  IO.popen(command_out, 'r+') do |io_lemon| 

    pid_content = fork do 
      content_init(monitor, io_lemon)
      content_walk(monitor, io_lemon) # loop for each event
      
      io_lemon.close()
    end
    Process.detach(pid_content)

    # CPU hog caveat when using 'pkill lemonbar'
    # Abnormal lemonbar process termination, will make this loop go wild

    IO.popen('sh', 'w') do |io_sh|
      while io_lemon do
        io_sh.puts io_lemon.gets
      end
        
      io_sh.close()
    end
 
    io_lemon.close()
  end
end

def detach_lemon(monitor, parameters)
  # warning: Signal.trap is application wide
  Signal.trap("PIPE", "EXIT")
    
  pid_lemon = fork { run_lemon(monitor, parameters) }
  Process.detach(pid_lemon)
end

def detach_lemon_conky(parameters)
  # warning: Signal.trap is application wide
  Signal.trap("PIPE", "EXIT")
    
  pid_conky = fork do
    path    = __dir__+ "/../conky"
    cmd_in  = 'conky -c ' + path + '/conky-lemonbar.lua'
    cmd_out = 'lemonbar ' + parameters
  
    IO.popen(cmd_out, "w") do |io_lemon|
    IO.popen(cmd_in,  "r") do |io_conky| 
      while io_conky do
        io_lemon.puts io_conky.gets
      end

      io_conky.close()    
      io_lemon.close()
    end
    end
  end

  Process.detach(pid_conky)
end

def kill_zombie()
    system('pkill -x dzen2')
    system('pkill -x lemonbar')
    system('pkill -x cat')
    system('pkill conky')
    system('pkill herbstclient')
end

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# main

panel_height = 24
monitor = get_monitor(ARGV)

kill_zombie()
system("herbstclient pad #{monitor} #{panel_height} 0 #{panel_height} 0")

# run process in the background

params_top = get_params_top(monitor, panel_height)
detach_lemon(monitor, params_top)

params_bottom = get_params_bottom(monitor, panel_height)
detach_lemon_conky(params_bottom)
