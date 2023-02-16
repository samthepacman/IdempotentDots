#!/usr/bin/php 
<?php # using PHP7
# ------------------------------------------------------------------
#
#     Description: unified config for herbstluftwm dzen2 statusbar
#     Created by: Epsi Nurwijayadi <epsi.nurwijayadi@gmail.com)
#
#     Source
#     https://github.com/epsi-rns/dotfiles/tree/master/standalone/dzen2-hlwm/php
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

# dzen Parameters

function get_params_top($monitor, $panel_height)
{
    $geometry = get_geometry($monitor);
    list($xpos, $ypos, $width, $height) = get_top_panel_geometry(
        $panel_height, $geometry);
    
    $bgcolor = '#000000';
    $fgcolor = '#ffffff';
    $font    = '-*-takaopgothic-medium-*-*-*-12-*-*-*-*-*-*-*';
  
    $parameters  = "  -x $xpos -y $ypos -w $width -h $height"
                 . " -ta l -bg '$bgcolor' -fg '$fgcolor'"
                 . " -title-name dzentop"
                 . " -fn '$font'";

    return $parameters;
}

function get_params_bottom($monitor, $panel_height)
{
    $geometry = get_geometry($monitor);
    list($xpos, $ypos, $width, $height) = get_bottom_panel_geometry(
        $panel_height, $geometry);
    
    $bgcolor = '#000000';
    $fgcolor = '#ffffff';
    $font    = '-*-fixed-medium-*-*-*-11-*-*-*-*-*-*-*';
  
    $parameters  = "  -x $xpos -y $ypos -w $width -h $height"
                 . " -ta l -bg '$bgcolor' -fg '$fgcolor'"
                 . " -title-name dzenbottom"
                 . " -fn '$font'";

    return $parameters;
}

function get_dzen2_parameters($monitor, $panel_height)
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

const SEPARATOR = "^bg()^fg(".COLOR['black'].")|^bg()^fg()";

// http://fontawesome.io/
const FONT_AWESOME = '^fn(FontAwesome-9)';

// Powerline Symbol
const RIGHT_HARD_ARROW = '^fn(powerlinesymbols-14)^fn()';
const RIGHT_SOFT_ARROW = '^fn(powerlinesymbols-14)^fn()';
const LEFT_HARD_ARROW  = '^fn(powerlinesymbols-14)^fn()';
const LEFT_SOFT_ARROW  = '^fn(powerlinesymbols-14)^fn()';

// theme
const PRE_ICON    = "^fg(".COLOR['yellow500'].")".FONT_AWESOME;
const POST_ICON   = "^fn()^fg()";

# main

function get_statusbar_text($monitor)
{
    global $tags_status;
    $text = '';
    
    // draw tags
    foreach ($tags_status as $tag_status) {
        $text .= output_by_tag($monitor, $tag_status);
     }
    
    //# draw date and time
    $text .= output_by_datetime();
    
    // draw window title    
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
        $text_pre = "^bg(".COLOR['blue500'].")^fg(".COLOR['black'].")"
                  . RIGHT_HARD_ARROW
                  . "^bg(".COLOR['blue500'].")^fg(".COLOR['white'].")";
        break;
    case "+":
        $text_pre = "^bg(".COLOR['yellow500'].")"
                  . "^fg(".COLOR['grey400'].")";       
        break;
    case ":":
        $text_pre = "^bg()^fg(".COLOR['white'].")";
        break;
    case "!":
        $text_pre = "^bg(".COLOR['red500'].")"
                  . "^fg(".COLOR['white'].")";
        break;
    default:
        $text_pre = "^bg()^fg(".COLOR['grey600'].")";
    }

    # ----- tag by number
    
    // assuming using dzen2_svn
    // clickable tags if using SVN dzen
    $text_name = "^ca(1,herbstclient focus_monitor \"${monitor}\" && " 
               . "herbstclient use \"${tag_index}\") ${tag_name} ^ca()";

    # ----- post tag

    if ($tag_mark == '#')
        $text_post = "^bg(".COLOR['black'].")^fg(".COLOR['blue500'].")" 
                   . RIGHT_HARD_ARROW;
    else
        $text_post = "";
     
    return $text_pre . $text_name . $text_post;
}

function output_by_title()
{
    global $segment_windowtitle;
    
    $text  = " ^r(5x0) ".SEPARATOR." ^r(5x0) ";
    $text .= $segment_windowtitle;
    
    return $text;
}

function output_by_datetime()
{
    global $segment_datetime; 
    
    $text  = " ^r(5x0) ".SEPARATOR." ^r(5x0) ";
    $text .= $segment_datetime;
    
    return $text;
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
      
    $segment_windowtitle = " ${icon} ^bg()"
        . "^fg(".COLOR['grey700'].") $windowtitle";
}

function set_datetime() {
    global $segment_datetime;

    $date_icon = PRE_ICON."".POST_ICON;
    $date_format = '%a %b %d';
    $date_str  = strftime($date_format);
    $date_text = "$date_icon ^bg()^fg(".COLOR['grey700'].") $date_str";

    $time_icon = PRE_ICON."".POST_ICON;
    $time_format = '%H:%M:%S';
    $time_str = strftime($time_format);
    $time_text = "$time_icon ^bg()^fg(".COLOR['blue500'].") $time_str";

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
        system('pkill dzen2');
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

function content_init($monitor, $pipe_dzen2_out)
{   
    // initialize statusbar before loop
    set_tag_value($monitor);
    set_windowtitle('');
    set_datetime();
        
    $text = get_statusbar_text($monitor);
    fwrite($pipe_dzen2_out, $text."\n");
    flush();
}

function content_event_idle($pipe_cat_stdin) 
{
    $pid = pcntl_fork();

    switch($pid) {         
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
        return $pid;
    } 
}

function content_event_interval($pipe_cat_stdin) 
{
    date_default_timezone_set("Asia/Jakarta");
    $pid = pcntl_fork();

    switch($pid) {         
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
        return $pid;
    } 
}

function content_walk($monitor, $pipe_dzen2_stdin)
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
        fwrite($pipe_dzen2_stdin, $text."\n");
        flush();
    }

    pclose($pipe_cat[1]);
}

function run_dzen2($monitor, $parameters) 
{ 
    $command_out    = "dzen2 $parameters";
    $pipe_dzen2_out = popen($command_out, 'w');

    content_init($monitor, $pipe_dzen2_out);
    content_walk($monitor, $pipe_dzen2_out); // loop for each event

    pclose($pipe_dzen2_out);
}

function detach_dzen2($monitor, $parameters)
{ 
    $pid_dzen2 = pcntl_fork();
    
    switch($pid_dzen2) {         
    case -1 : // fork errror         
        die('could not fork');
    case 0  : // we are the child
        run_dzen2($monitor, $parameters); 
        break;
    default : // we are the parent             
        return $pid_dzen2;
    }    
}


function detach_dzen2_conky($parameters)
{ 
    $pid_conky = pcntl_fork();

    switch($pid_conky) {         
    case -1 : // fork errror         
        die('could not fork');
    case 0  : // we are the child
        $cmd_out  = 'dzen2 '.$parameters;
        $pipe_out = popen($cmd_out, "w");

        $path     = __dir__."/../conky";
        $cmd_in   = 'conky -c '.$path.'/conky-dzen2.lua';
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

function detach_transset() 
{ 
    $pid_transset = pcntl_fork();
    if ($pid_transset == 0) { 
        sleep(1);
        system('transset .8 -n dzentop    >/dev/null');
        system('transset .8 -n dzenbottom >/dev/null');
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
detach_dzen2($monitor, $params_top);

$params_bottom = get_params_bottom($monitor, $panel_height);
detach_dzen2_conky($params_bottom);

// optional transparency
detach_transset();
