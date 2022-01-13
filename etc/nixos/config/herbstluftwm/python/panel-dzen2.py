#!/usr/bin/env python3
# ------------------------------------------------------------------
#
#     Description: unified config for herbstluftwm dzen2 statusbar
#     Created by: Epsi Nurwijayadi <epsi.nurwijayadi@gmail.com)
#
#     Source
#     https://github.com/epsi-rns/dotfiles/tree/master/standalone/dzen2-hlwm/python
#
#     Blog
#     http://epsi-rns.github.io/desktop/2017/06/11/herbstlustwm-event-idle-overview.html
#     http://epsi-rns.github.io/desktop/2017/06/04/herbstlustwm-tag-status-python.html
#     http://epsi-rns.github.io/desktop/2017/06/14/herbstlustwm-event-idle-python.html
#
# ------------------------------------------------------------------

import os
import sys
import subprocess

import datetime
import time

from gmc import color

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# helper

# script arguments
def get_monitor(arguments):
    # ternary operator
    monitor = int(arguments[1]) if (len(arguments) > 1) else 0

    return monitor

# geometry calculation

def get_geometry(monitor):
    raw = os.popen('herbstclient monitor_rect '+ str(monitor)).read()

    if not raw: 
        print('Invalid monitor ' + str(monitor))
        exit(1)
    
    geometry = raw.split(' ')
    
    return geometry

def get_top_panel_geometry(height, geometry):
    # geometry has the format X Y W H
    return (int(geometry[0]), int(geometry[1]),
            int(geometry[2]), height)

def get_bottom_panel_geometry(height, geometry):
    # geometry has the format X Y W H
    return (int(geometry[0]) + 0, int(geometry[3]) - height, 
            int(geometry[2]) - 0, height )

# dzen Parameters

def get_params_top(monitor, panel_height):  
    geometry = get_geometry(monitor)
    xpos, ypos, width, height = get_top_panel_geometry(
       panel_height, geometry)
    
    bgcolor = '#000000'
    fgcolor = '#ffffff'
    font    = '-*-takaopgothic-medium-*-*-*-12-*-*-*-*-*-*-*'

    parameters  = '  -x '+str(xpos)+' -y '+str(ypos) \
                + ' -w '+str(width)+' -h '+str(height) \
                +  " -ta l -bg '"+bgcolor+"' -fg '"+fgcolor+"'" \
                +  ' -title-name dzentop' \
                +  " -fn '"+font+"'"

    return parameters

def get_params_bottom(monitor, panel_height):  
    geometry = get_geometry(monitor)
    xpos, ypos, width, height = get_bottom_panel_geometry(
       panel_height, geometry)
    
    bgcolor = '#000000'
    fgcolor = '#ffffff'
    font    = '-*-fixed-medium-*-*-*-11-*-*-*-*-*-*-*'

    parameters  = '  -x '+str(xpos)+' -y '+str(ypos) \
                + ' -w '+str(width)+' -h '+str(height) \
                +  " -ta l -bg '"+bgcolor+"' -fg '"+fgcolor+"'" \
                +  ' -title-name dzenbottom' \
                +  " -fn '"+font+"'"

    return parameters

def get_dzen2_parameters(monitor, panel_height):
    return get_params_top(monitor, panel_height)

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# output

# initialize

# assuming $ herbstclient tag_status
# 	#1	:2	:3	:4	.5	.6	.7	.8	.9

# custom tag names
TAG_SHOWS = ['一 ichi', '二 ni', '三 san', '四 shi', 
    '五 go', '六 roku', '七 shichi', '八 hachi', '九 kyū', '十 jū']

# initialize variable segment
segment_windowtitle = '' # empty string
tags_status         = [] # empty list
segment_datetime    = '' # empty string

# decoration

SEPARATOR = '^bg()^fg(' + color['black'] + ')|^bg()^fg()'

# http://fontawesome.io/
FONT_AWESOME = '^fn(FontAwesome-9)'

# Powerline Symbol
RIGHT_HARD_ARROW = '^fn(powerlinesymbols-14)^fn()'
RIGHT_SOFT_ARROW = '^fn(powerlinesymbols-14)^fn()'
LEFT_HARD_ARROW  = '^fn(powerlinesymbols-14)^fn()'
LEFT_SOFT_ARROW  = '^fn(powerlinesymbols-14)^fn()'

# theme
PRE_ICON    = '^fg(' + color['yellow500'] + ')' + FONT_AWESOME
POST_ICON   = '^fn()^fg()'

# main

def get_statusbar_text(monitor):
    text = ''

    # draw tags
    for tag_status in tags_status:
        text += output_by_tag(monitor, tag_status)

    # draw date and time
    text += output_by_datetime()

    # draw window title
    text += output_by_title()
    
    return text

# each segments

def output_by_tag(monitor, tag_status):
    tag_index  = tag_status[1:2]
    tag_mark   = tag_status[0:1]
    tag_name   = TAG_SHOWS[int(tag_index) - 1] # zero based

    # ----- pre tag

    if tag_mark == '#':
        text_pre = '^bg(' + color['blue500'] + ')'   \
                   '^fg(' + color['black'] + ')' \
                 + RIGHT_HARD_ARROW \
                 + '^bg(' + color['blue500'] + ')'   \
                   '^fg(' + color['white'] + ')'
    elif tag_mark == '+':
        text_pre = '^bg(' + color['yellow500'] + ')' \
                   '^fg(' + color['grey400'] + ')'
    elif tag_mark == ':':
        text_pre = '^bg()^fg(' + color['white'] + ')'
    elif tag_mark == '!':
        text_pre = '^bg(' + color['red500'] + ')'    \
                   '^fg(' + color['white'] + ')'
    else:
        text_pre = '^bg()^fg(' + color['grey600'] + ')'

   
    # ----- tag by number
   
    # assuming using dzen2_svn
    # clickable tags if using SVN dzen
    text_name = '^ca(1,herbstclient focus_monitor "' \
              + str(monitor) + '" && ' + 'herbstclient use "' \
              + tag_index + '") ' + tag_name + ' ^ca() '
    
    # ----- post tag

    if tag_mark == '#':
        text_post = '^bg(' + color['black'] + ')' \
                    '^fg(' + color['blue500'] + ')' + RIGHT_HARD_ARROW
    else: 
        text_post = ''
     
    return (text_pre + text_name + text_post)

def output_by_title():
    text  = ' ^r(5x0) ' + SEPARATOR + ' ^r(5x0) '
    text += segment_windowtitle

    return text

def output_by_datetime():
    text  = ' ^r(5x0) ' + SEPARATOR + ' ^r(5x0) '
    text += segment_datetime

    return text

# setting variables, response to event handler

def set_tag_value(monitor):
    global tags_status

    raw = os.popen('herbstclient tag_status ' + str(monitor)).read()
    raw = raw.strip()
    tags_status = raw.split("\t")

def set_windowtitle(windowtitle):
    global segment_windowtitle
    icon = PRE_ICON + '' + POST_ICON
      
    segment_windowtitle = ' ' + icon + \
        ' ^bg()^fg(' + color['grey700'] + ') ' + windowtitle

def set_datetime():
    global segment_datetime
    now = datetime.datetime.now()
    
    date_icon   = PRE_ICON + '' + POST_ICON
    date_format = '{0:%Y-%m-%d}'
    date_str  = date_format.format(now)
    date_text = date_icon + ' ^bg()' \
              + '^fg(' + color['grey700'] + ') ' + date_str

    time_icon   = PRE_ICON + '' + POST_ICON    
    time_format = '{0:%H:%M:%S}'
    time_str  = time_format.format(now)
    time_text = time_icon +' ^bg()' \
              + '^fg(' + color['blue500'] + ') ' + time_str

    segment_datetime = date_text + '  ' + time_text

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# pipe handler

def handle_command_event(monitor, event):  
    # find out event origin
    column = event.split("\t")
    origin = column[0]
    
    tag_cmds = ['tag_changed', 'tag_flags', 'tag_added', 'tag_removed']
    title_cmds = ['window_title_changed', 'focus_changed']

    if origin == 'reload':
        os.system('pkill dzen2')
    elif origin == 'quit_panel':
        exit()
    elif origin in tag_cmds:
        set_tag_value(monitor)
    elif origin in title_cmds:
        title = column[2] if (len(column) > 2) else ''
        set_windowtitle(title)
    elif origin == 'interval':
        set_datetime()

def content_init(monitor, pipe_dzen2_out):
    # initialize statusbar before loop
    set_tag_value(monitor)
    set_windowtitle('')
    set_datetime()
        
    text = get_statusbar_text(monitor)
    pipe_dzen2_out.stdin.write(text + '\n')
    pipe_dzen2_out.stdin.flush()

def content_event_idle(pipe_cat_out):
    pid_idle = os.fork()
        
    if pid_idle == 0:
        try:
            # start a pipe
            command_in = 'herbstclient --idle'  
            pipe_idle_in = subprocess.Popen(
                    [command_in], 
                    stdout = subprocess.PIPE,
                    stderr = subprocess.STDOUT,
                    shell  = True,
                    universal_newlines = True
            )
            
            # wait for each event  
            for event in pipe_idle_in.stdout: 
                pipe_cat_out.stdin.write(event)
                pipe_cat_out.stdin.flush()
    
            pipe_idle_in.stdout.close()
        finally:
            import signal
            os.kill(pid_idle, signal.SIGTERM)

def content_event_interval(pipe_cat_out):
    pid_interval = os.fork()

    if pid_interval == 0:
        try:
            while True:
                pipe_cat_out.stdin.write('interval' + '\n')
                pipe_cat_out.stdin.flush()
    
                time.sleep(1)
        finally:
            import signal
            os.kill(pid_interval, signal.SIGTERM)

def content_walk(monitor, pipe_dzen2_out): 
    pipe_cat = subprocess.Popen(
            ['cat'], 
            stdin  = subprocess.PIPE,
            stdout = subprocess.PIPE,
            shell  = True,
            universal_newlines=True
        )

    content_event_idle(pipe_cat)
    content_event_interval(pipe_cat)

    # wait for each event, trim newline
    for event in pipe_cat.stdout:
        handle_command_event(monitor, event.strip())

        text = get_statusbar_text(monitor)
        pipe_dzen2_out.stdin.write(text + '\n')
        pipe_dzen2_out.stdin.flush()
 
    pipe_cat.stdin.close()
    pipe_cat.stdout.close()
    
def run_dzen2(monitor, parameters):  
    command_out  = 'dzen2 ' + parameters

    pipe_dzen2_out = subprocess.Popen(
            [command_out], 
            stdin  = subprocess.PIPE,
            shell  = True,
            universal_newlines=True
        )

    content_init(monitor, pipe_dzen2_out)
    content_walk(monitor, pipe_dzen2_out) # loop for each event

    pipe_dzen2_out.stdin.close()

def detach_dzen2_debug(monitor, parameters):
    run_lemon(monitor, parameters)

def detach_dzen2(monitor, parameters):
    # in case of debugging purpose, 
    # uncomment all the fork related lines.
    pid_dzen2 = os.fork()
    
    if pid_dzen2 == 0:
        try:
            run_dzen2(monitor, parameters)
            os._exit(1)
        finally:
            import signal
            os.kill(pid_dzen2, signal.SIGTERM)

def detach_dzen2_conky(parameters):
    pid_conky = os.fork()
    
    if pid_conky == 0:
        try:
            dirname  = os.path.dirname(os.path.abspath(__file__))
            path     = dirname + "/../conky"
            cmd_in   = 'conky -c ' + path + '/conky-dzen2.lua'

            cmd_out  = 'dzen2 ' + parameters

            pipe_out = subprocess.Popen(
                    [cmd_out], 
                    stdin  = subprocess.PIPE,
                    shell  = True,
                    universal_newlines=True
                )

            pipe_in  = subprocess.Popen(
                    [cmd_in], 
                    stdout = pipe_out.stdin,
                    stderr = subprocess.STDOUT,
                    shell  = True,
                    universal_newlines = True
                )

            pipe_out.stdin.close()
            outputs, errors = pipe_out.communicate()
    
            # avoid zombie apocalypse
            pipe_out.wait()

            os._exit(1)
        finally:
            import signal
            os.kill(pid_conky, signal.SIGTERM)

def detach_transset():
    pid_transset = os.fork()
    
    if pid_transset == 0:
        try:
            time.sleep(1)
            os.system('transset .8 -n dzentop    >/dev/null')
            os.system('transset .8 -n dzenbottom >/dev/null')
            os._exit(1)
        finally:
            import signal
            os.kill(pid_transset, signal.SIGTERM)

def kill_zombie():
    os.system('pkill -x dzen2')
    os.system('pkill -x lemonbar')
    os.system('pkill -x cat')
    os.system('pkill conky')
    os.system('pkill herbstclient')

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# main

panel_height = 24
monitor = get_monitor(sys.argv)

kill_zombie()
os.system('herbstclient pad ' + str(monitor) + ' ' 
    + str(panel_height) + ' 0 ' + str(panel_height) + ' 0')

# run process in the background

params_top    = get_params_top(monitor, panel_height)
detach_dzen2(monitor, params_top)

params_bottom = get_params_bottom(monitor, panel_height)
detach_dzen2_conky(params_bottom)

# optional transparency
detach_transset();
