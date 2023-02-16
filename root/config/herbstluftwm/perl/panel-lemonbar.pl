#!/usr/bin/perl
# ------------------------------------------------------------------
#
#     Description: unified config for herbstluftwm lemonbar
#     Created by: Epsi Nurwijayadi <epsi.nurwijayadi@gmail.com)
#
#     Source
#     https://github.com/epsi-rns/dotfiles/tree/master/standalone/lemon-hlwm/perl
#
#     Blog
#     http://epsi-rns.github.io/desktop/2017/06/11/herbstlustwm-event-idle-overview.html
#     http://epsi-rns.github.io/desktop/2017/06/03/herbstlustwm-tag-status-perl.html
#     http://epsi-rns.github.io/desktop/2017/06/13/herbstlustwm-event-idle-perl.html
#
# ------------------------------------------------------------------

use warnings;
use strict;

use Time::Piece;

# for tutorial purpose, we use two libraries
use IO::Pipe;   # unidirectional
use IPC::Open2; #  bidirectional

use File::Basename;
use lib dirname(__FILE__);

use gmc;

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# helper

# script arguments
sub get_monitor {
    my @arguments = @_; 
    my $num_args  = $#arguments;
   
    # ternary operator
    my $monitor = ($num_args > 0) ? $arguments[0] : 0;
    
    return $monitor;
}

# geometry calculation

sub get_geometry {
    my $monitor = shift;

    my $geometry_qx = qx(herbstclient monitor_rect "$monitor");
    if ($geometry_qx eq "") { 
        print "Invalid monitor $monitor\n";
        exit 1
    }
    
    my @geometry = split / /, $geometry_qx;
    
    return @geometry;
}

sub get_top_panel_geometry {
    my $height = shift;
    my @geometry = @_;

    # geometry has the format X Y W H
    return ($geometry[0], $geometry[1], 
            $geometry[2], $height);
}

sub get_bottom_panel_geometry {
    my $height = shift;
    my @geometry = @_;

    # geometry has the format X Y W H
    return ($geometry[0] + 0, $geometry[3] - $height, 
            $geometry[2] - 0, $height);
}

# lemon Parameters

sub get_params_top {
    # parameter: function argument
    my $monitor = shift;
    my $panel_height = shift;

    # calculate geometry
    my @geometry = get_geometry($monitor);
    my ($xpos, $ypos, $width, $height) = 
       get_top_panel_geometry($panel_height, @geometry); 

    # geometry: -g widthxheight+x+y
    my $geom_res = "${width}x${height}+${xpos}+${ypos}";
    
    # color, with transparency
    my $bgcolor = '#aa000000';
    my $fgcolor = '#ffffff';
    
    # XFT: require lemonbar_xft_git 
    my $font_takaop  = "takaopgothic-9";
    my $font_symbol  = "PowerlineSymbols-11";
    my $font_awesome = "FontAwesome-9";

    # finally
    my $parameters = "  -g $geom_res -u 2"
                   . " -B $bgcolor -F $fgcolor"
                   . " -f $font_takaop -f $font_awesome -f $font_symbol";

    return $parameters;
}

sub get_params_bottom {
    # parameter: function argument
    my $monitor = shift;
    my $panel_height = shift;

    # calculate geometry
    my @geometry = get_geometry($monitor);
    my ($xpos, $ypos, $width, $height) = 
       get_bottom_panel_geometry($panel_height, @geometry); 

    # geometry: -g widthxheight+x+y
    my $geom_res = "${width}x${height}+${xpos}+${ypos}";
    
    # color, with transparency
    my $bgcolor = '#aa000000';
    my $fgcolor = '#ffffff';
    
    # XFT: require lemonbar_xft_git 
    my $font_mono    = "monospace-9";
    my $font_symbol  = "PowerlineSymbols-11";
    my $font_awesome = "FontAwesome-9";

    # finally
    my $parameters = "  -g $geom_res -u 2"
                   . " -B $bgcolor -F $fgcolor"
                   . " -f $font_mono -f $font_awesome -f $font_symbol";

    return $parameters;
}

sub get_lemon_parameters {   
    # parameter: function argument
    my $monitor = shift;
    my $panel_height = shift;

    return get_params_top($monitor, $panel_height);
}

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# output

# initialize

# assuming $ herbstclient tag_status
# 	#1	:2	:3	:4	.5	.6	.7	.8	.9

# custom tag names
use constant TAG_SHOWS => ['一 ichi', '二 ni', '三 san', '四 shi', 
    '五 go', '六 roku', '七 shichi', '八 hachi', '九 kyū', '十 jū'];

# initialize variable segment
my $segment_windowtitle = ''; # empty string
my @tags_status         = []; # empty array
my $segment_datetime    = ''; # empty string

# decoration

use constant SEPARATOR => "%{B-}%{F$color{'yellow500'}}|%{B-}%{F-}";

# Powerline Symbol
use constant RIGHT_HARD_ARROW => "";
use constant RIGHT_SOFT_ARROW => "";
use constant LEFT_HARD_ARROW  => "";
use constant LEFT_SOFT_ARROW  => "";

# theme
use constant PRE_ICON  => "%{F$color{'yellow500'}}";
use constant POST_ICON => "%{F-}";

# main

sub get_statusbar_text {
    my $monitor = shift;   
    my $text = '';

    # draw tags
    $text .= '%{l}';
    foreach my $tag_status (@tags_status) {
        $text .= output_by_tag($monitor, $tag_status);
    }

    # draw date and time
    $text .= '%{c}';
    $text .= output_by_datetime();
    
    # draw window title
    $text .= '%{r}';
    $text .= output_by_title();
    
    return $text;
}

# each segments

sub output_by_tag {
    my $monitor = shift;    
    
    my $tag_status = shift;
    my $tag_index  = substr($tag_status, 1, 1);
    my $tag_mark   = substr($tag_status, 0, 1);
    my $tag_name   = TAG_SHOWS->[$tag_index - 1]; # zero based

    # ----- pre tag

    my $text_pre = '';
    if ($tag_mark eq '#') {
        $text_pre = "%{B$color{'blue500'}}%{F$color{'black'}}"
                  . "%{U$color{'white'}}%{+u}".RIGHT_HARD_ARROW
                  . "%{B$color{'blue500'}}%{F$color{'white'}}"
                  . "%{U$color{'white'}}%{+u}";
    } elsif ($tag_mark eq '+') {
        $text_pre = "%{B$color{'yellow500'}}%{F$color{'grey400'}}";
    } elsif ($tag_mark eq ':') {
        $text_pre = "%{B-}%{F$color{'white'}}"
                  . "%{U$color{'red500'}}%{+u}";
    } elsif ($tag_mark eq '!') {
        $text_pre = "%{B$color{'red500'}}%{F$color{'white'}}"
                  . "%{U$color{'white'}}%{+u}";
    } else {
        $text_pre = "%{B-}%{F$color{'grey600'}}%{-u}";
    }

    # ----- tag by number

    # clickable tags
    my $text_name = "%{A:herbstclient focus_monitor \"$monitor\" && "
                  . "herbstclient use \"$tag_index\":} $tag_name %{A} ";
                  
    # non clickable tags
    # my $text_name = " $tag_name ";

    # ----- post tag
    
    my $text_post = "";
    if ($tag_mark eq '#') {
        $text_post = "%{B-}%{F$color{'blue500'}}"
                   . "%{U$color{'red500'}}%{+u}"
                   . RIGHT_HARD_ARROW;
    }
    
    my $text_clear = '%{B-}%{F-}%{-u}';
     
    return $text_pre . $text_name . $text_post . $text_clear;
}

sub output_by_title {
    my $text = "$segment_windowtitle ".SEPARATOR."  ";
    return $text;
}

sub output_by_datetime {
    return $segment_datetime;
}

# setting variables, response to event handler

sub set_tag_value {
    my $monitor = shift;
    
    my $tag_status_qx = qx(herbstclient tag_status $monitor);
       $tag_status_qx =~ s/^\s+|\s+$//g;
    @tags_status = split(/\t/, $tag_status_qx);
}

sub set_windowtitle {
    my $windowtitle = shift;  
    my $icon = PRE_ICON."".POST_ICON;
      
    $segment_windowtitle = " $icon "
                         . "%{B-}%{F$color{'grey700'}} $windowtitle";
}

sub set_datetime {
    my $date_icon = PRE_ICON."".POST_ICON;
    my $date_format = '%a %b %d';
    my $date_str = localtime->strftime($date_format);
    my $date_text = "$date_icon %{B-}%{F$color{'grey700'}} $date_str";

    my $time_icon = PRE_ICON."".POST_ICON;
    my $time_format = '%H:%M:%S';
    my $time_str = localtime->strftime($time_format);
    my $time_text = "$time_icon %{B-}%{F$color{'blue500'}} $time_str";

    $segment_datetime = "$date_text  $time_text";
}

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# pipe handler

sub handle_command_event {
    my $monitor = shift;
    my $event   = shift;
    
    # find out event origin
    my @column = split(/\t/, $event);
    my $origin = $column[0];

    if ($origin eq 'reload') {
        system('pkill lemonbar');
    } elsif ($origin eq 'quit_panel') {
        exit;
    } elsif (  # avoiding the unstable ~~ smartmatch operator
               ($origin eq 'tag_changed') 
            or ($origin eq 'tag_flags')
            or ($origin eq 'tag_added')
            or ($origin eq 'tag_removed')
            ) {
        set_tag_value($monitor);
    } elsif (  ($origin eq 'window_title_changed') 
            or ($origin eq 'focus_changed')
            ) {
        my $title = ($#column > 2) ? $column[2] : '';
        set_windowtitle($title);
    } elsif ($origin eq 'interval') {
        set_datetime();
    }   
}

sub content_init {
    my $monitor = shift;
    my $pipe_lemon_out = shift;

    # initialize statusbar before loop
    set_tag_value($monitor);
    set_windowtitle('');
    set_datetime();

    my $text = get_statusbar_text($monitor);
    print $pipe_lemon_out $text."\n";
    flush $pipe_lemon_out;
}

sub content_event_idle {
    my $pipe_cat_out = shift;
 
    my $pid_idle = fork;
    return if $pid_idle;     # in the parent process

    # start a pipe
    my $pipe_idle_in = IO::Pipe->new();
    my $command = 'herbstclient --idle';
    my $handle  = $pipe_idle_in->reader($command);

    # wait for each event
    my $event = '';
    while ($event = <$pipe_idle_in>) {
        print $pipe_cat_out $event;
        flush $pipe_cat_out;
    }
    
    $pipe_idle_in->close();
}

sub content_event_interval {
    my $pipe_cat_out = shift;

    my $pid_interval = fork;
    return if $pid_interval;     # in the parent process
    
    while(1) {         
        print $pipe_cat_out "interval\n";
        flush $pipe_cat_out;
        
        sleep 1;
    }
}

sub content_walk {
    my $monitor = shift;
    my $pipe_lemon_out = shift; 

    my ($rh_cat, $wh_cat);
    my $pid_cat = open2 ($rh_cat, $wh_cat, 'cat') 
        or die "can't pipe sh: $!";

    content_event_idle($wh_cat);
    content_event_interval($wh_cat);

    my $text  = '';
    my $event = '';

    # wait for each event, trim newline
    while (chomp($event = <$rh_cat>)) {
        handle_command_event($monitor, $event);
        
        $text = get_statusbar_text($monitor);     
        print $pipe_lemon_out $text."\n";
        flush $pipe_lemon_out;
    }

    waitpid( $pid_cat, 0 );
}

sub run_lemon { 
    my $monitor = shift;
    my $parameters = shift;

    my $command_out = "lemonbar $parameters";
    my ($rh_lemon_out, $wh_lemon_out);
    my $pid_lemon_out = open2 (
            $rh_lemon_out, $wh_lemon_out, $command_out) 
        or die "can't pipe lemon out: $!";
        
    my ($rh_sh, $wh_sh);
    my $pid_sh = open2 ($rh_sh, $wh_sh, 'sh') 
        or die "can't pipe sh: $!";

    my $pid_content = fork;
    if ($pid_content) {
        # in the parent process
        my $line_clickable = '';
        while($line_clickable = <$rh_lemon_out>) {
            print $wh_sh $line_clickable;
            flush $wh_sh;
        }        
    } else {
        # in the child process
        content_init($monitor, $wh_lemon_out);
        content_walk($monitor, $wh_lemon_out); # loop for each event
    }

    waitpid( $pid_lemon_out, 0 );
    waitpid( $pid_sh, 0 );
}

sub detach_lemon { 
    my $monitor = shift;
    my $parameters = shift;

    my $pid_lemon = fork;
    return if $pid_lemon;     # in the parent process
    
    run_lemon($monitor, $parameters);
    exit; 
}

sub detach_lemon_conky { 
    my $parameters = shift;

    my $pid_conky = fork;
    return if $pid_conky;     # in the parent process

    my $pipe_out = IO::Pipe->new();
    my $cmd_in   = "lemonbar " . $parameters;
    my $hnd_in   = $pipe_out->writer($cmd_in);

    my $pipe_in  = IO::Pipe->new();
    my $dirname  = dirname(__FILE__);
    my $path     = "$dirname/../conky";       
    my $cmd_out  = "conky -c $path/conky-lemonbar.lua";
    my $hnd_out  = $pipe_in->reader($cmd_out);

    while(<$pipe_in>) {
        print $pipe_out $_;
        flush $pipe_out;
    }

    $pipe_in->close();
    $pipe_out->close();
    exit; 
}

sub kill_zombie() {
    system('pkill -x dzen2');
    system('pkill -x lemonbar');
    system('pkill -x cat');
    system('pkill conky');
    system('pkill herbstclient');
}

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# main

my $panel_height = 24;
my $monitor = get_monitor(@ARGV);
kill_zombie();
system("herbstclient pad $monitor $panel_height 0 $panel_height 0");

# run process in the background

my $params_top = get_params_top($monitor, $panel_height);
detach_lemon($monitor, $params_top);

my $params_bottom = get_params_bottom($monitor, $panel_height);
detach_lemon_conky($params_bottom);
