#!/usr/bin/env bash
# ------------------------------------------------------------------
#
#     Description: unified config for herbstluftwm lemonbar
#     Created by: Epsi Nurwijayadi <epsi.nurwijayadi@gmail.com)
#
#     Source
#     https://github.com/epsi-rns/dotfiles/tree/master/standalone/lemon-hlwm/bash
#
#     Blog
#     http://epsi-rns.github.io/desktop/2017/06/11/herbstlustwm-event-idle-overview.html
#     http://epsi-rns.github.io/desktop/2017/06/02/herbstlustwm-tag-status-bash.html
#     http://epsi-rns.github.io/desktop/2017/06/12/herbstlustwm-event-idle-bash.html
#
# ------------------------------------------------------------------

# libraries

DIR=$(dirname "$0")
. ${DIR}/gmc.sh

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# helper

# script arguments
function get_monitor() {
    local argument=("$@")
    local num_args=${#argument[@]}

    # ternary operator
    [[ $num_args > 0 ]] && monitor=${argument[0]} || monitor=0
}

# geometry calculation

function get_geometry() {
    local monitor=$1;
    geometry=( $(herbstclient monitor_rect "$monitor") )
    if [ -z "$geometry" ] ;then
        echo "Invalid monitor $monitor"
        exit 1
    fi
}

function get_top_panel_geometry() {
   local panel_height=$1
   shift
   local geometry=("$@")
   
   # geometry has the format X Y W H
     xpos=${geometry[0]}
     ypos=${geometry[1]}
    width=${geometry[2]}
   height=$panel_height
}

function get_bottom_panel_geometry() {
   local panel_height=$1
   shift
   local geometry=("$@")
   
   # geometry has the format X Y W H
     xpos=$(( ${geometry[0]} + 0 ))
     ypos=$(( ${geometry[3]} - $panel_height ))
    width=$(( ${geometry[2]} - 0 ))
   height=$panel_height
}

# lemonbar Parameters

function get_params_top() {  
    # parameter: function argument
    local monitor=$1
    local panel_height=$2

    # calculate geometry
    get_geometry $monitor
    get_top_panel_geometry $panel_height "${geometry[@]}"
    
    # geometry: -g widthxheight+x+y
    geom_res="${width}x${height}+${xpos}+${ypos}"
    
    # color, with transparency
    local bgcolor="#aa000000"
    local fgcolor="#ffffff"
    
    # XFT: require lemonbar_xft_git 
    local font_takaop="takaopgothic-9"
    local font_symbol="PowerlineSymbols-11"
    local font_awesome="FontAwesome-9"

    # finally
    lemon_parameters="  -g $geom_res -u 2"
    lemon_parameters+=" -B $bgcolor -F $fgcolor" 
    lemon_parameters+=" -f $font_takaop -f $font_awesome -f $font_symbol" 
}

function get_params_bottom() {  
    # parameter: function argument
    local monitor=$1
    local panel_height=$2

    # calculate geometry
    get_geometry $monitor
    get_bottom_panel_geometry $panel_height "${geometry[@]}"
    
    # geometry: -g widthxheight+x+y
    geom_res="${width}x${height}+${xpos}+${ypos}"
    
    # color, with transparency
    local bgcolor="#aa000000"
    local fgcolor="#ffffff"
    
    # XFT: require lemonbar_xft_git 
    local font_mono="monospace-9"
    local font_symbol="PowerlineSymbols-11"
    local font_awesome="FontAwesome-9"

    # finally
    lemon_parameters="  -g $geom_res -u 2"
    lemon_parameters+=" -B $bgcolor -F $fgcolor" 
    lemon_parameters+=" -f $font_mono -f $font_awesome -f $font_symbol" 
}

function get_lemon_parameters() { 
    get_params_top $monitor $panel_height
}

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# output

# initialize

# assuming $ herbstclient tag_status
# 	#1	:2	:3	:4	.5	.6	.7	.8	.9

# custom tag names
readonly tag_shows=( "一 ichi" "二 ni" "三 san" "四 shi" 
  "五 go" "六 roku" "七 shichi" "八 hachi" "九 kyū" "十 jū")

# initialize variable segment
segment_windowtitle=''; # empty string
tags_status=();         # empty array
segment_datetime='';    # empty string

# decoration

readonly separator="%{B-}%{F${color['yellow500']}}|%{B-}%{F-}"

# Powerline Symbol
readonly right_hard_arrow=""
readonly right_soft_arrow=""
readonly  left_hard_arrow=""
readonly  left_soft_arrow=""

# theme
readonly  pre_icon="%{F${color['yellow500']}}"
readonly post_icon="%{F-}"

# main

function get_statusbar_text() {
    local monitor=$1
    local text=''

    # draw tags
    text+='%{l}'
    for tag_status in "${tags_status[@]}"
    do
        output_by_tag $monitor $tag_status
        text+=$buffer
    done

    # draw date and time
    text+='%{c}'
    output_by_datetime
    text+=$buffer

    # draw window title
    text+='%{r}'
    output_by_title    
    text+=$buffer
    
    buffer=$text
}

# each segments

function output_by_tag() {
    local    monitor=$1    
    local tag_status=$2    
        
    local  tag_index=${tag_status:1:1}
    local   tag_mark=${tag_status:0:1}
    local   tag_name=${tag_shows[$tag_index - 1]}; # zero based

    # ----- pre tag

    local text_pre=''
    case $tag_mark in
        '#') text_pre+="%{B${color['blue500']}}%{F${color['black']}}"
             text_pre+="%{U${color['white']}}%{+u}$right_hard_arrow"
             text_pre+="%{B${color['blue500']}}%{F${color['white']}}"
             text_pre+="%{U${color['white']}}%{+u}"
        ;;
        '+') text_pre+="%{B${color['yellow500']}}%{F${color['grey400']}}"
        ;;
        ':') text_pre+="%{B-}%{F${color['white']}}"
             text_pre+="%{U${color['red500']}}%{+u}"
        ;;
        '!') text_pre+="%{B${color['red500']}}%{F${color['white']}}"
             text_pre+="%{U${color['white']}}%{+u}"
        ;;
        *)   text_pre+="%{B-}%{F${color['grey600']}}%{-u}"
        ;;
    esac

    # ----- tag by number
    
    # clickable tags
    local text_name=''
    text_name+="%{A:herbstclient focus_monitor \"$monitor\" && "
    text_name+="herbstclient use \"$tag_index\":} $tag_name %{A} "
  
    # non clickable tags
    # local text_name=" $tag_name "
    
    # ----- post tag

    local text_post=''
    if [ $tag_mark = '#' ]
    then        
        text_post+="%{B-}%{F${color['blue500']}}"
        text_post+="%{U${color['red500']}}%{+u}${right_hard_arrow}";
    fi
    
    text_clear='%{B-}%{F-}%{-u}'
     
    buffer="$text_pre$text_name$text_post$text_clear"
}

function output_by_title() {
    local text="$segment_windowtitle $separator  "
    buffer=$text
}

function output_by_datetime() {
    buffer=$segment_datetime
}

# setting variables, response to event handler

function set_tag_value() {
  # http://wiki.bash-hackers.org/commands/builtin/read
  # http://wiki.bash-hackers.org/syntax/shellvars#ifs
  # http://www.tldp.org/LDP/abs/html/x17837.html#HERESTRINGSREF
  IFS=$'\t' read -ra tags_status <<< "$(herbstclient tag_status $monitor)"
}

function set_windowtitle() {
    local windowtitle=$1
    local icon="$pre_icon$post_icon"
    # "${segmentWindowtitle//^/^^}"
    
    segment_windowtitle=" $icon %{B-}%{F${color['grey700']}} $windowtitle"
}

function set_datetime() {
    local date_icon="$pre_icon$post_icon"
    local date_str=$(date +'%a %b %d')
    local date_text="$date_icon %{B-}%{F${color['grey700']}} $date_str"

    local time_icon="$pre_icon$post_icon"
    local time_str=$(date +'%H:%M:%S')
    local time_text="$time_icon %{B-}%{F${color['blue500']}} $time_str"

    segment_datetime="$date_text  $time_text"
}

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# pipe handler

function handle_command_event() {
    local monitor=$1
    shift
    local event=$@    
    
    # find out event origin
    IFS=$'\t' column=($event);
    origin=${column[0]}
    
    # find out event origin
    case $origin in
        reload)
            pkill lemonbar
            ;;
        quit_panel)
            exit
            ;;
        tag*)
            # http://www.tldp.org/LDP/abs/html/x17837.html#HERESTRINGSREF
            # echo "resetting tags" >&2
            set_tag_value $monitor
            ;;
        focus_changed|window_title_changed)
            [[ ${#column[@]} > 2 ]] && title=${column[2]} || title=''
            set_windowtitle "$title"
            ;;
        interval)
            set_datetime
            ;;
    esac 
}

function content_init() {
    monitor=$1

    # initialize statusbar before loop
    set_tag_value $monitor
    set_windowtitle ''
    set_datetime

    get_statusbar_text $monitor
    echo $buffer
}

function content_event_idle() {
    # wait for each event     
    herbstclient --idle
}

function content_event_interval() {
    # endless loop
    while :; do 
      echo "interval"
      sleep 1
    done
}

function content_walk() {
    monitor=$1
    
    {
        content_event_idle &
        pid_idle=$!
    
        content_event_interval &
        pid_interval=$!
    
    }  | while read event; do
            handle_command_event $monitor "$event"
        
            get_statusbar_text $monitor
            echo $buffer
        done
}

function run_lemon() { 
    monitor=$1
    shift
    parameters=$@
    
    command_out="lemonbar $parameters"
    
    {
       content_init $monitor
       content_walk $monitor # loop for each event
    } | $command_out | sh
}

function detach_lemon() { 
    monitor=$1
    shift
    parameters=$@
    
    run_lemon $monitor $parameters &
}

function detach_lemon_conky() {    
    parameters=$@

    command_out="lemonbar $parameters"
    
    {
        dirname=$(dirname $(readlink -f "$0"))
        path="$dirname/../conky"
        conky -c "$path/conky-lemonbar.lua"
    } | $command_out &
}

function kill_zombie() {
    pkill -x dzen2
    pkill -x lemonbar
    pkill -x cat
    pkill conky
    pkill herbstclient
}

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# main

panel_height=24
get_monitor ${@}

kill_zombie
herbstclient pad $monitor $panel_height 0 $panel_height 0

# run process in the background

get_params_top $monitor $panel_height
detach_lemon $monitor $lemon_parameters

get_params_bottom $monitor $panel_height
detach_lemon_conky $lemon_parameters
