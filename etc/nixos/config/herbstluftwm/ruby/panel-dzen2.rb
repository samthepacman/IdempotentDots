#!/usr/bin/ruby
# ------------------------------------------------------------------
#
#     Description: unified config for herbstluftwm dzen2 statusbar
#     Created by: Epsi Nurwijayadi <epsi.nurwijayadi@gmail.com)
#
#     Source
#     https://github.com/epsi-rns/dotfiles/tree/master/standalone/dzen2-hlwm/ruby
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

# dzen Parameters

def get_params_top(monitor, panel_height)
  geometry = get_geometry(monitor)
  xpos, ypos, width, height = get_top_panel_geometry(
    panel_height, geometry)
    
  bgcolor = '#000000'
  fgcolor = '#ffffff'
  font    = '-*-takaopgothic-medium-*-*-*-12-*-*-*-*-*-*-*'
   
  parameters  = "  -x #{xpos} -y #{ypos} -w #{width} -h #{height}" \
                " -ta l -bg '#{bgcolor}' -fg '#{fgcolor}'" \
                " -title-name dzentop" \
                " -fn '#{font}'"
end

def get_params_bottom(monitor, panel_height)
  geometry = get_geometry(monitor)
  xpos, ypos, width, height = get_bottom_panel_geometry(
    panel_height, geometry)
    
  bgcolor = '#000000'
  fgcolor = '#ffffff'
  font    = '-*-fixed-medium-*-*-*-11-*-*-*-*-*-*-*'
   
  parameters  = "  -x #{xpos} -y #{ypos} -w #{width} -h #{height}" \
                " -ta l -bg '#{bgcolor}' -fg '#{fgcolor}'" \
                " -title-name dzenbottom" \
                " -fn '#{font}'"
end

def get_dzen2_parameters(monitor, panel_height)
  get_params_top(monitor, panel_height)
end

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# output

# initialize

# assuming $ herbstclient tag_status
# 	#1	:2	:3	:4	.5	.6	.7	.8	.9

# custom tag names
@TAG_SHOWS = ['一 ichi', '二 ni', '三 san', '四 shi', 
  '五 go', '六 roku', '七 shichi', '八 hachi', '九 kyū', '十 jū']

# initialize variable segment
@segment_windowtitle = '' # empty string
@tags_status         = [] # empty array
@segment_datetime    = '' # empty string

# decoration

@SEPARATOR = "^bg()^fg(#{COLOR['black']})|^bg()^fg()"

# http://fontawesome.io/
@FONT_AWESOME = '^fn(FontAwesome-9)'

# Powerline Symbol
@RIGHT_HARD_ARROW = '^fn(powerlinesymbols-14)^fn()'
@RIGHT_SOFT_ARROW = '^fn(powerlinesymbols-14)^fn()'
@LEFT_HARD_ARROW  = '^fn(powerlinesymbols-14)^fn()'
@LEFT_SOFT_ARROW  = '^fn(powerlinesymbols-14)^fn()'

# theme
@PRE_ICON    = "^fg(#{COLOR['yellow500']})#{@FONT_AWESOME}"
@POST_ICON   = "^fn()^fg()"

# main

def get_statusbar_text(monitor)
  text = ''

  # draw tags
  @tags_status.each { |tag_status| 
    text << output_by_tag(monitor, tag_status) }

  # draw date and time
  text << output_by_datetime()
 
  # draw window title    
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
    text_pre = "^bg(#{COLOR['blue500']})^fg(#{COLOR['black']})" \
             + @RIGHT_HARD_ARROW \
             + "^bg(#{COLOR['blue500']})^fg(#{COLOR['white']})"
  when '+'
    text_pre = "^bg(#{COLOR['yellow500']})^fg(#{COLOR['grey400']})"
  when ':'
    text_pre = "^bg()^fg(#{COLOR['white']})"
  when '!'
    text_pre = "^bg(#{COLOR['red500']})^fg(#{COLOR['white']})"
  else
    text_pre = "^bg()^fg(#{COLOR['grey600']})"
  end
   
  # ----- tag by number
    
  # assuming using dzen2_svn
  # clickable tags if using SVN dzen
  text_name = "^ca(1,herbstclient focus_monitor \"#{monitor}\" && " \
            + "herbstclient use \"#{tag_index}\") #{tag_name} ^ca() "
    
  # ----- post tag

  if tag_mark == '#'
    text_post = "^bg(#{COLOR['black']})^fg(#{COLOR['blue500']})" \
              + @RIGHT_HARD_ARROW
  else
    text_post = ""        
  end
     
  text_pre + text_name + text_post
end

def output_by_title()
  text  = " ^r(5x0) #{@SEPARATOR} ^r(5x0) "  
  text << @segment_windowtitle
end

def output_by_datetime()
  text  = " ^r(5x0) #{@SEPARATOR} ^r(5x0) "  
  text << @segment_datetime
end

# setting variables, response to event handler

def set_tag_value(monitor)
  raw = IO.popen('herbstclient tag_status ' + monitor.to_s).read()
  @tags_status = raw.strip.split("\t")
end

def set_windowtitle(windowtitle)
  icon = @PRE_ICON + '' + @POST_ICON
      
  @segment_windowtitle = " #{icon} ^bg()" \
    + "^fg(#{COLOR['grey700']}) #{windowtitle}"
end

def set_datetime()
    date_icon = @PRE_ICON + '' + @POST_ICON
    localtime = Time.now

    date_format = '%a %b %d'
    date_str  = localtime.strftime(date_format)
    date_text = "#{date_icon} ^bg()^fg(#{COLOR['grey700']}) #{date_str}"

    time_icon = @PRE_ICON + '' + @POST_ICON
    time_format = '%H:%M:%S'
    time_str  = localtime.strftime(time_format)
    time_text = "#{time_icon} ^bg()^fg(#{COLOR['blue500']}) #{time_str}"

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
    os.system('pkill dzen2')
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

def content_init(monitor, dzen2_stdin)
  # initialize statusbar before loop
  set_tag_value(monitor)
  set_windowtitle('')
  set_datetime()
      
  text = get_statusbar_text(monitor)
  dzen2_stdin.puts(text)
end

def content_event_idle(cat_stdin)
  pid = fork do 
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

  Process.detach(pid)  
end


def content_event_interval(cat_stdin)
  pid = fork do 
    while true do
      cat_stdin.print "interval\n"

      sleep(1)
    end
  end

  Process.detach(pid)  
end

def content_walk(monitor, dzen2_stdin)
  # note the r+ mode for bidirectional
  IO.popen('cat', 'r+') do |io_cat| 

    content_event_idle(io_cat)
    content_event_interval(io_cat)

    while io_cat do 
      # read next event, trim newline
      event = (io_cat.gets).strip
      handle_command_event(monitor, event)
        
      text = get_statusbar_text(monitor)
      dzen2_stdin.puts(text)
    end
  
  io_cat.close()
  end
end

def run_dzen2(monitor, parameters)
  command_out  = 'dzen2 ' + parameters
  IO.popen(command_out, "w") do |io_dzen2| 
    content_init(monitor, io_dzen2)
    content_walk(monitor, io_dzen2) # loop for each event
        
    io_dzen2.close()    
  end
end

def detach_dzen2(monitor, parameters)
  # warning: Signal.trap is application wide
  Signal.trap("PIPE", "EXIT")
    
  pid_dzen2 = fork { run_dzen2(monitor, parameters) }
  Process.detach(pid_dzen2)
end

def detach_dzen2_conky(parameters)
  # warning: Signal.trap is application wide
  Signal.trap("PIPE", "EXIT")
    
  pid_conky = fork do
    path    = __dir__+ "/../conky"
    cmd_in  = 'conky -c ' + path + '/conky-dzen2.lua'
    cmd_out = 'dzen2 ' + parameters
  
    IO.popen(cmd_out, "w") do |io_dzen2|
    IO.popen(cmd_in,  "r") do |io_conky| 
      while io_conky do
        io_dzen2.puts io_conky.gets
      end

      io_conky.close()    
      io_dzen2.close()
    end
    end
  end

  Process.detach(pid_conky)
end

def detach_transset()
  pid_transset = fork do
    sleep(1)
    system('transset .8 -n dzentop    >/dev/null')        
    system('transset .8 -n dzenbottom >/dev/null')
  end
    
  Process.detach(pid_transset)
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
detach_dzen2(monitor, params_top)

params_bottom = get_params_bottom(monitor, panel_height)
detach_dzen2_conky(params_bottom)

# optional transparency
detach_transset()
