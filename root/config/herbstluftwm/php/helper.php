<?php # using PHP7

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# helpers

function hc($arguments) 
{
    system("herbstclient $arguments");
}

function do_config($command, $hash) {
    # loop over hash
    foreach ($hash as $key => $value) {
        hc($command.' '.$key.' '.$value);

        # uncomment to debug in terminal
        # echo $command.' '.$key.' '.$value."\n";
    }
}

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# tags related

function set_tags_with_name() 
{
    global $tag_names, $tag_keys;

    hc("rename default '$tag_names[0]' 2>/dev/null || true");
    
    foreach($tag_names as $index=>$value) {
        hc("add '$value'");
        
        # uncomment to debug in terminal
        # echo $index."\n";

        $key = $tag_keys[$index];
        if (!empty($key)) {
            hc("keybind Mod4-$key use_index '$index'");
            hc("keybind Mod4-Shift-$key move_index '$index'");
        }
    }
}

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# miscellanous

function bind_cycle_layout() 
{
    # The following cycles through the available layouts
    # within a frame, but skips layouts, if the layout change 
    # wouldn't affect the actual window positions.
    # I.e. if there are two windows within a frame,
    # the grid layout is skipped.

    hc( "keybind Mod4-space "
        ."or , and . compare tags.focus.curframe_wcount = 2 "
        .". cycle_layout +1 vertical horizontal max vertical grid "
        .", cycle_layout +1 ");
}

# do multi monitor setup here, e.g.:
# hc("set_monitors 1280x1024+0+0 1280x1024+1280+0");
# or simply:
# hc("detect_monitors");

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# find the panel

function do_panel() 
{
    $panel   = __dir__."/panel-lemonbar.php";
    if (!is_executable($panel))
        $panel = "/etc/xdg/herbstluftwm/panel.sh";

    $raw = shell_exec('herbstclient list_monitors | cut -d: -f1');
    $monitors = explode("\n", trim($raw));

    foreach ($monitors as $monitor) {
        # start it on each monitor
        system("$panel $monitor > /dev/null &"); // no $output
    }
}

