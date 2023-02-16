#!/usr/bin/php 
<?php # using PHP7
# ------------------------------------------------------------------
#
#     Description: unified config for herbstluftwm lemonbar
#     Created by: Epsi Nurwijayadi <epsi.nurwijayadi@gmail.com)
#
#     Source
#     https://github.com/epsi-rns/dotfiles/tree/master/standalone/lemon-hlwm/php
#
#     Blog
#     http://epsi-rns.github.io/desktop/2017/06/11/herbstlustwm-event-idle-overview.html
#     http://epsi-rns.github.io/desktop/2017/06/06/herbstlustwm-tag-status-php.html
#     http://epsi-rns.github.io/desktop/2017/06/16/herbstlustwm-event-idle-php.html
#
# ------------------------------------------------------------------

require_once(__DIR__.'/gmc.php');

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# helper

// script arguments
function get_monitor($arguments)
{
    // ternary operator
    return count($arguments) > 0 ? (int)$arguments[0] : 0;
}

# geometry calculation

function get_geometry($monitor)
{
    $raw = shell_exec('herbstclient monitor_rect '.$monitor);

    if (empty($raw)) {
        print('Invalid monitor '.$monitors);
        exit(1);
    }
    
    return explode(' ', trim($raw));
}

function get_top_panel_geometry($height, $geometry)
{
    // geometry has the format X Y W H
    return array(
        $geometry[0], $geometry[1], $geometry[2], $height);
}

function get_bottom_panel_geometry($height, $geometry)
{
    // geometry has the format X Y W H
    return array(
        $geometry[0] + 0, ($geometry[3] - $height), 
        $geometry[2] - 0, $height);
}

# lemon Parameters

function get_params_top($monitor, $panel_height)
{
    # calculate geometry
    $geometry = get_geometry($monitor);
    list($xpos, $ypos, $width, $height) = get_top_panel_geometry(
        $panel_height, $geometry);


    # geometry: -g widthxheight+x+y
    $geom_res = "${width}x${height}+${xpos}+${ypos}";
    
    # color, with transparency    
    $bgcolor = "'#aa000000'";
    $fgcolor = "'#ffffff'";

    # XFT: require lemonbar_xft_git 
    $font_takaop  = "takaopgothic-9";
    $font_symbol  = "PowerlineSymbols-11";
    $font_awesome = "FontAwesome-9";

    # finally  
    $parameters  = " -g $geom_res -u 2"
                 . " -B $bgcolor -F $fgcolor"
                 . " -f $font_takaop -f $font_awesome -f $font_symbol"; 

    return $parameters;
}

function get_params_bottom($monitor, $panel_height)
{
    # calculate geometry
    $geometry = get_geometry($monitor);
    list($xpos, $ypos, $width, $height) = get_bottom_panel_geometry(
        $panel_height, $geometry);


    # geometry: -g widthxheight+x+y
    $geom_res = "${width}x${height}+${xpos}+${ypos}";
    
    # color, with transparency    
    $bgcolor = "'#aa000000'";
    $fgcolor = "'#ffffff'";

    # XFT: require lemonbar_xft_git 
    $font_mono    = "monospace-9";
    $font_symbol  = "PowerlineSymbols-11";
    $font_awesome = "FontAwesome-9";

    # finally  
    $parameters  = " -g $geom_res -u 2"
                 . " -B $bgcolor -F $fgcolor"
                 . " -f $font_mono -f $font_awesome -f $font_symbol"; 

    return $parameters;
}

function get_lemon_parameters($monitor, $panel_height)
{   return get_params_top($monitor, $panel_height); }

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# output

# initialize

// assuming $ herbstclient tag_status
// 	#1	:2	:3	:4	.5	.6	.7	.8	.9

// custom tag names
const TAG_SHOWS = ['一 ichi', '二 ni', '三 san', '四 shi', 
  '五 go', '六 roku', '七 shichi', '八 hachi', '九 kyū', '十 jū'];

// initialize variable segment
$segment_windowtitle = ''; # empty string
$tags_status         = []; # empty array
$segment_datetime    = ''; # empty string

# decoration

const SEPARATOR = "%{B-}%{F".COLOR['yellow500']."}|%{B-}%{F-}";

// Powerline Symbol
const RIGHT_HARD_ARROW = "";
const RIGHT_SOFT_ARROW = "";
const LEFT_HARD_ARROW  = "";
const LEFT_SOFT_ARROW  = "";

// theme
const PRE_ICON    = "%{F".COLOR['yellow500']."}";
const POST_ICON   = "%{F-}";

# main

function get_statusbar_text($monitor)
{
    global $tags_status;
    $text = '';
    
    // draw tags
    $text .= '%{l}';
    foreach ($tags_status as $tag_status) {
        $text .= output_by_tag($monitor, $tag_status);
     }

    # draw date time
    $text .= '%{c}';
    $text .= output_by_datetime();

    // draw window title
    $text .= '%{r}';
    $text .= output_by_title();

    return $text;
}

# each segments

function output_by_tag($monitor, $tag_status)
{
    $tag_index  = substr($tag_status, 1, 1);
    $tag_mark   = substr($tag_status, 0, 1);
    $tag_name   = TAG_SHOWS[(int)$tag_index - 1]; # zero based

    # ----- pre tag

    switch ($tag_mark) {
    case "#":
        $text_pre = "%{B".COLOR['blue500']."}%{F".COLOR['black']."}"
                  . "%{U".COLOR['white']."}%{+u}".RIGHT_HARD_ARROW
                  . "%{B".COLOR['blue500']."}%{F".COLOR['white']."}"
                  . "%{U".COLOR['white']."}%{+u}";
        break;
    case "+":
        $text_pre = "%{B".COLOR['yellow500']."}%{F".COLOR['grey400']."}";
        break;
    case ":":
        $text_pre = $text_pre = "%{B-}%{F".COLOR['white']."}"
                  . "%{U".COLOR['red500']."}%{+u}";
        break;
    case "!":
        $text_pre = "%{B".COLOR['red500']."}%{F".COLOR['white']."}"
                  . "%{U".COLOR['white']."}%{+u}";
        break;
    default:
        $text_pre = "%{B-}%{F".COLOR['grey600']."}%{-u}";
    }

    # ----- tag by number
    
    // clickable tags
    $text_name = "%{A:herbstclient focus_monitor \"${monitor}\" && " 
               . "herbstclient use \"${tag_index}\":} ${tag_name} %{A}";
    
    # non clickable tags
    #$text_name = " $tag_name ";

    # ----- post tag

    if ($tag_mark == '#')
        $text_post = "%{B-}%{F".COLOR['blue500']."}"
                   . "%{U".COLOR['red500']."}%{+u}"
                   . RIGHT_HARD_ARROW;
    else
        $text_post = "";

    $text_clear = '%{B-}%{F-}%{-u}';
     
    return $text_pre . $text_name . $text_post . $text_clear;
}

function output_by_title()
{
    global $segment_windowtitle;    
    $text  = "$segment_windowtitle ".SEPARATOR."  ";
    
    return $text;
}

function output_by_datetime()
{
    global $segment_datetime; 
    return $segment_datetime;
}

# setting variables, response to event handler

function set_tag_value($monitor)
{
    global $tags_status;

    $raw = shell_exec("herbstclient tag_status $monitor");
    $tags_status = explode("\t", trim($raw));
}

function set_windowtitle($windowtitle)
{
    global $segment_windowtitle;
    $icon = PRE_ICON."".POST_ICON;    
      
    $segment_windowtitle = " ${icon} %{B-}"
        . "%{F".COLOR['grey700']."} $windowtitle";
}

function set_datetime() {
    global $segment_datetime;

    $date_icon = PRE_ICON."".POST_ICON;
    $date_format = '%a %b %d';
    $date_str  = strftime($date_format);
    $date_text = "$date_icon %{B-}%{F".COLOR['grey700']."} $date_str";

    $time_icon = PRE_ICON."".POST_ICON;
    $time_format = '%H:%M:%S';
    $time_str = strftime($time_format);
    $time_text = "$time_icon %{B-}%{F".COLOR['blue500']."} $time_str";

    $segment_datetime = "$date_text  $time_text";
}

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# pipe handler

function handle_command_event($monitor, $event)
{
    // find out event origin
    $column = explode("\t", $event);
    $origin = $column[0];

    switch($origin) {
    case 'reload':
        system('pkill lemonbar');
        break;
    case 'quit_panel':
        exit(1);
    case 'tag_changed':
    case 'tag_flags':
    case 'tag_added':
    case 'tag_removed':
        set_tag_value($monitor);
        break;
    case 'window_title_changed':
    case 'focus_changed':
        $title = count($column) > 2 ? $column[2] : '';
        set_windowtitle($title);
        break;
    case 'interval':
        set_datetime();
    }
}

function content_init($monitor, $pipe_lemon_stdin)
{   
    // initialize statusbar before loop
    set_tag_value($monitor);
    set_windowtitle('');
    set_datetime();

    $text = get_statusbar_text($monitor);
    fwrite($pipe_lemon_stdin, $text."\n");
    flush();
}

function content_event_idle($pipe_cat_stdin) 
{
    $pid_idle = pcntl_fork();

    switch($pid_idle) {         
    case -1 : // fork errror         
        die('could not fork');
    case 0  : // we are the child
        // start a pipe
        $command_in    = 'herbstclient --idle';
        $pipe_idle_in  = popen($command_in,  'r'); // handle
    
        while(!feof($pipe_idle_in)) {
            # read next event
            $event = fgets($pipe_idle_in);
            fwrite($pipe_cat_stdin, $event);
            flush();
        }
    
        pclose($pipe_idle_in);

        break;
    default : // we are the parent
        // do nothing
        return $pid_idle;
    } 
}

function content_event_interval($pipe_cat_stdin) 
{
    date_default_timezone_set("Asia/Jakarta");
    $pid_interval = pcntl_fork();

    switch($pid_interval) {         
    case -1 : // fork errror         
        die('could not fork');
    case 0  : // we are the child
        do {
            fwrite($pipe_cat_stdin, "interval\n");
            flush();
            sleep(1);
        } while (true);
        
        break;
    default : // we are the parent
        // do nothing
        return $pid_interval;
    } 
}

function content_walk($monitor, $pipe_lemon_stdin)
{       
    $descriptorspec = array(
        0 => array('pipe', 'r'),  // stdin
        1 => array('pipe', 'w'),  // stdout
        2 => array('pipe', 'w',)  // stderr
    );
    
    $proc_cat = proc_open('cat', $descriptorspec, $pipe_cat);

    content_event_idle($pipe_cat[0]);
    content_event_interval($pipe_cat[0]);

    while(!feof($pipe_cat[1])) {
        $event = trim(fgets($pipe_cat[1]));
        handle_command_event($monitor, $event);
        
        $text = get_statusbar_text($monitor);
        fwrite($pipe_lemon_stdin, $text."\n");
        flush();
    }

    pclose($pipe_cat[1]);
}

function run_lemon($monitor, $parameters) 
{ 
    $descriptorspec = array(
        0 => array('pipe', 'r'),  // stdin
        1 => array('pipe', 'w'),  // stdout
        2 => array('pipe', 'w',)  // stderr
    );
    
    $command_out  = "lemonbar $parameters";
    $proc_lemon = proc_open($command_out, $descriptorspec, $pipe_lemon);
    $proc_sh    = proc_open('sh', $descriptorspec, $pipe_sh);
    
    $pid_content = pcntl_fork();
    
    switch($pid_content) {         
    case -1 : // fork errror         
        die('could not fork');
    case 0  : // we are the child
        content_init($monitor, $pipe_lemon[0]);
        content_walk($monitor, $pipe_lemon[0]); // loop for each event
        break;
    default : // we are the parent
        while(!feof($pipe_lemon[1])) {
            $buffer = fgets($pipe_lemon[1]);
            fwrite($pipe_sh[0], $buffer);
        }
        return $pid_content;
    } 

    pclose($pipe_lemon[0]);
}

function detach_lemon($monitor, $parameters)
{ 
    $pid_lemon = pcntl_fork();
    
    switch($pid_lemon) {         
    case -1 : // fork errror         
        die('could not fork');
    case 0  : // we are the child
        run_lemon($monitor, $parameters); 
        break;
    default : // we are the parent             
        return $pid_lemon;
    }
}

function detach_lemon_conky($parameters)
{ 
    $pid_conky = pcntl_fork();

    switch($pid_conky) {         
    case -1 : // fork errror         
        die('could not fork');
    case 0  : // we are the child
        $cmd_out  = 'lemonbar '.$parameters;
        $pipe_out = popen($cmd_out, "w");

        $path     = __dir__."/../conky";
        $cmd_in   = 'conky -c '.$path.'/conky-lemonbar.lua';
        $pipe_in  = popen($cmd_in,  "r");
    
        while(!feof($pipe_in)) {
            $buffer = fgets($pipe_in);
            fwrite($pipe_out, $buffer);
            flush();
        }
    
        pclose($pipe_in);
        pclose($pipe_out);

        break;
    default : // we are the parent             
        return $pid_conky;
    }  
}

function kill_zombie()
{
    system('pkill -x dzen2');
    system('pkill -x lemonbar');
    system('pkill -x cat');
    system('pkill conky');
    system('pkill herbstclient');
}

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# main

$panel_height = 24;
$monitor = get_monitor($argv);

kill_zombie();
system("herbstclient pad $monitor $panel_height 0 $panel_height 0");

// run process in the background

$params_top = get_params_top($monitor, $panel_height);
detach_lemon($monitor, $params_top);

$params_bottom = get_params_bottom($monitor, $panel_height);
detach_lemon_conky($params_bottom);
