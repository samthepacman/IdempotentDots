#!/usr/bin/lua
-- ------------------------------------------------------------------
--
--     Description: unified config for herbstluftwm autostart
--     Created by: Epsi Nurwijayadi <epsi.nurwijayadi@gmail.com)
--
--     Source
--     https://github.com/epsi-rns/dotfiles/tree/master/herbstluftwm/lua
--
--     Blog
--     http://epsi-rns.github.io/desktop/2017/05/01/herbstlustwm-modularized-overview.html
--     http://epsi-rns.github.io/desktop/2017/05/07/herbstlustwm-modularized-lua.html
--
-- ------------------------------------------------------------------

local dirname  = debug.getinfo(1).source:match("@?(.*/)")
package.path = package.path .. ';' .. dirname .. '?.lua;'

local posix = require('posix')
local gmc   = require ".gmc"

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-- config

local config = {}

-- tags array

config.tag_names = {}; config.tag_keys = {} -- new array

-- alternative 1

-- for name = 1,9 do table.insert(config.tag_names, name) end
-- for name = 1,9 do table.insert(config.tag_keys, name)  end
-- table.insert(config.tag_keys, 0)

-- alternative 2
for i = 1,9 do config.tag_names[i-1]=i end
for i = 1,9 do config.tag_keys[i-1]=i  end
config.tag_keys[9] = 0

-- alternative 3
-- http://lua-users.org/wiki/RangeIterator

-- uncomment to debug
-- for key, value in pairs(config.tag_keys) do print(key.." | "..value) end

-- keybindings

-- if you have a super key you will be much happier with Mod set to Mod4
-- Mod=Mod1    -- Use alt as the main modifier
-- Alt=Mod1
-- Mod=Mod4    -- Use the super key as the main modifier

-- Modifier variables
s = 'Shift';
c = 'Control';
m = 'Mod4';
a = 'Mod1';

-- resizing frames
resizestep = 0.05;

config.keybinds = {
  -- session
    [m .. '-' .. s .. '-q'] = 'quit',
    [m .. '-' .. s .. '-r'] = 'reload',
    [m .. '-' .. s .. '-c'] = 'close',

  -- use your $TERMINAL with xterm as fallback
    [m .. '-Return']        = 'spawn ${TERMINAL:-xfce4-terminal}',

  -- epsi
    [m .. '-d']             = 'spawn dmenu_run_hlwm',
    [m .. '-' .. s .. '-d'] = 'spawn rofi -show run -opacity 90',
    [m .. '-' .. s .. '-x'] = 'spawn oblogout',

  -- basic movement

  -- focusing clients
    [m .. '-Left']     = 'focus left',
    [m .. '-Down']     = 'focus down',
    [m .. '-Up']       = 'focus up',
    [m .. '-Right']    = 'focus right',
    [m .. '-h']        = 'focus left',
    [m .. '-j']        = 'focus down',
    [m .. '-k']        = 'focus up',
    [m .. '-l']        = 'focus right',

  -- moving clients
    [m .. '-' .. s .. '-Left']  = 'shift left',
    [m .. '-' .. s .. '-Down']  = 'shift down',
    [m .. '-' .. s .. '-Up']    = 'shift up',
    [m .. '-' .. s .. '-Right'] = 'shift right',
    [m .. '-' .. s .. '-h']     = 'shift left',
    [m .. '-' .. s .. '-j']     = 'shift down',
    [m .. '-' .. s .. '-k']     = 'shift up',
    [m .. '-' .. s .. '-l']     = 'shift right',
    
  -- splitting frames
  -- create an empty frame at the specified direction
    [m .. '-u']        = 'split   bottom  0.5',
    [m .. '-o']        = 'split   right   0.5',
  -- let the current frame explode into subframes
    [m .. '-' .. c .. '-space'] = 'split explode',

  -- resizing frames
    [m .. '-' .. c .. '-h']     = 'resize left  +'.. resizestep,
    [m .. '-' .. c .. '-j']     = 'resize down  +'.. resizestep,
    [m .. '-' .. c .. '-k']     = 'resize up    +'.. resizestep,
    [m .. '-' .. c .. '-l']     = 'resize right +'.. resizestep,
    [m .. '-' .. c .. '-Left']  = 'resize left  +'.. resizestep,
    [m .. '-' .. c .. '-Down']  = 'resize down  +'.. resizestep,
    [m .. '-' .. c .. '-Up']    = 'resize up    +'.. resizestep,
    [m .. '-' .. c .. '-Right'] = 'resize right +'.. resizestep,
    
    -- MPC
    [m .. '-' .. a .. '-h']     = 'spawn mpc toggle',
    [m .. '-' .. a .. '-t']     = 'spawn mpc prev',
    [m .. '-' .. a .. '-n']     = 'spawn mpc next'
}

-- tags

config.tagskeybinds = {
  -- cycle through tags
    [m .. '-period']   = 'use_index +1 --skip-visible',
    [m .. '-comma']    = 'use_index -1 --skip-visible',

  -- layouting
    [m .. '-r']        = 'remove',
    [m .. '-s']        = 'floating toggle',
    [m .. '-f']        = 'fullscreen toggle',
    [m .. '-p']        = 'pseudotile toggle',

  -- focus
    [m .. '-BackSpace'] = 'cycle_monitor',
    [m .. '-Tab']       = 'cycle_all +1',
    [m .. '-' .. s .. '-Tab']    = 'cycle_all -1',
    [m .. '-c']         = 'cycle',
    [m .. '-i']         = 'jumpto urgent'
}

config.mousebinds = {
  -- mouse    
    [m .. '-Button1']  = 'move',
    [m .. '-Button2']  = 'zoom',
    [m .. '-Button3']  = 'resize'
}

-- theme

config.attributes = {
    ['theme.tiling.reset']    = '1',
    ['theme.floating.reset']  = '1',

    ['theme.active.color']    = "'" .. gmc.color["red500"] .. "'",
    ['theme.normal.color']    = "'" .. gmc.color["grey200"] .. "'",
    ['theme.urgent.color']    = "'" .. gmc.color["pink500"] .. "'",

    ['theme.inner_width']     = '0',
    ['theme.inner_color']     = 'black',

    ['theme.border_width']    = '2',
    ['theme.floating.border_width'] = '4',
    ['theme.floating.outer_width']  = '1',
    ['theme.floating.outer_color']  = "'black'",

    ['theme.active.inner_color']    = "'#3E4A00'",
    ['theme.active.outer_color']    = "'#3E4A00'",
    ['theme.background_color']      = "'#141414'"
}

config.sets = {
    ['frame_border_active_color'] = "'" .. gmc.color["grey200"] .. "'",
    ['frame_bg_active_color']     = "'" .. gmc.color["yellow900"] .. "'",

    ['frame_border_normal_color'] = "'" .. gmc.color["grey50"] .. "'",
    ['frame_bg_normal_color']     = "'" .. gmc.color["red500"] .. "'",

    ['frame_border_width']        = '0',
    ['always_show_frame']         = '0',
    ['frame_bg_transparent']      = '1',
    ['frame_transparent_width']   = '2',
    ['frame_gap']                 = '20',

    ['window_gap']                = '0',
    ['frame_padding']             = '0',
    ['smart_window_surroundings'] = '0',
    ['smart_frame_surroundings']  = '1',
    ['mouse_recenter_gap']        = '0'
}

-- rules

config.rules = {
  -- normally focus new clients
    ['focus=on'] = '',

  -- zero based array
    ['class=Firefox']  = 'tag=' .. config.tag_names[1],
    ['class=Chromium'] = 'tag=' .. config.tag_names[1],
    ['class=Geany']    = 'tag=' .. config.tag_names[2],
    ['class=Thunar']   = 'tag=' .. config.tag_names[3],
    ['class=gimp']     = 'tag=' .. config.tag_names[4] ..' pseudotile=on',

    ['class=Oblogout'] = 'fullscreen=on',
    
    ["class~'(.*[Rr]xvt.*|.*[Tt]erm|Konsole)'"] = 'focus=on',

    ["windowtype~'_NET_WM_WINDOW_TYPE_(DIALOG|UTILITY|SPLASH)'"] = 'pseudotile=on',
  --["windowtype='_NET_WM_WINDOW_TYPE_DIALOG'"] = 'focus=on',
    ["windowtype='_NET_WM_WINDOW_TYPE_DIALOG'"] = 'fullscreen=on',
    ["windowtype~'_NET_WM_WINDOW_TYPE_(NOTIFICATION|DOCK|DESKTOP)'"] = 'manage=off'
}

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-- helper

local helper = {}

-- helpers

function helper.hc(arguments)
    os.execute("herbstclient " .. arguments)
end

function helper.do_config(command, hash)
    -- loop over hash
    for key, value in pairs(hash) do 
        helper.hc(command .. ' ' .. key .. ' ' .. value)

        -- uncomment to debug in terminal
        -- print(command .. ' ' .. key .. ' ' .. value)
    end
end

-- tags related

function helper.set_tags_with_name()
    local tag_names = config.tag_names
    local tag_keys = config.tag_keys    
    
    helper.hc("rename default '" .. tag_names[0] .. "' 2>/dev/null || true")
    
    for index, value in pairs(tag_names) do 
        helper.hc("add '" .. value .. "'");
        
        -- uncomment to debug in terminal
        -- print(index .. " | " .. value)

        local key = tag_keys[index]
        if (not (key == nil or key == '') ) then
            helper.hc("keybind Mod4-"..key.." use_index '"..index.."'")
            helper.hc("keybind Mod4-Shift-"..key.." move_index '"..index.."'")
        end
    end
end

-- miscellanous

function helper.bind_cycle_layout() 
    -- The following cycles through the available layouts
    -- within a frame, but skips layouts, if the layout change 
    -- wouldn't affect the actual window positions.
    -- I.e. if there are two windows within a frame,
    -- the grid layout is skipped.

    helper.hc( "keybind Mod4-space "
     .. "or , and . compare tags.focus.curframe_wcount = 2 "
     .. ". cycle_layout +1 vertical horizontal max vertical grid "
     .. ", cycle_layout +1 ")
end

-- do multi monitor setup here, e.g.:
-- hc("set_monitors 1280x1024+0+0 1280x1024+1280+0");
-- or simply:
-- hc("detect_monitors");

-- find the panel

local function file_exists(fname)
   return posix.stat(fname).type == 'regular'
end -- file_exists

-- https://codea.io/talk/discussion/2118/split-a-string-by-return-newline
local function lines(str)
  local t = {}
  local function helper(line) table.insert(t, line) return "" end
  helper(( str:gsub("(.-)\r?\n", helper) ))
  return t
end

-- http://lua-users.org/wiki/StringTrim
local function trim1(s)
  return (s:gsub("^%s*(.-)%s*$", "%1"))
end

function helper.do_panel() 
    local dirname  = debug.getinfo(1).source:match("@?(.*/)")   
    local panel   = dirname .. "panel-lemonbar.lua"
    if (not file_exists(panel)) then
        panel = "/etc/xdg/herbstluftwm/panel.sh"
    end

    command = 'herbstclient list_monitors | cut -d: -f1'
    local handle = io.popen(command)
    local result = handle:read("*a")
    handle:close()
    
    local raw = trim1(result) 
    local monitors = lines(raw)
    
    for i, monitor in pairs(monitors) do 
        if (not (monitor == nil or monitor == '') ) then
            -- start it on each monitor
            os.execute(panel .. " " .. monitor .." > /dev/null &")
        end
    end
end

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-- startup

local startup = {}

-- load on startup

function startup.run()
    -- redirect stderror to stdout, then capture the result
    command = 'new_attr bool my_not_first_autostart'
    local handle = io.popen('herbstclient ' .. command .. ' 2>&1')
    local result = handle:read('*a')
    local exitcode = handle:close()
    
    if ((result == nil or result == '')) then
     -- non windowed app
        os.execute('compton &')
        os.execute('dunst &')
        os.execute('parcellite &')
        os.execute('nitrogen --restore &')
        os.execute('mpd &')

     -- windowed app
        os.execute('xfce4-terminal &')
        os.execute('sleep 1 && firefox &')
        os.execute('sleep 2 && geany &')
        os.execute('sleep 2 && thunar &')
    end
end

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-- main

-- background before wallpaper
os.execute("xsetroot -solid '" .. gmc.color["blue500"] .. "'")

-- Read the manual in $ man herbstluftwm
helper.hc('emit_hook reload')

-- gap counter
os.execute("echo 35 > /tmp/herbstluftwm-gap")

-- do not repaint until unlock
helper.hc("lock")

-- standard
-- remove all existing keybindings
helper.hc('keyunbind --all')
helper.hc('mouseunbind --all')
helper.hc('unrule -F')

helper.set_tags_with_name()

-- do hash config
helper.do_config('keybind',   config.keybinds)
helper.do_config('keybind',   config.tagskeybinds)
helper.do_config('mousebind', config.mousebinds)
helper.do_config('attr',      config.attributes)
helper.do_config('set',       config.sets)
helper.do_config('rule',      config.rules)

-- finishing, some extra miles

-- I'm not sure what in this is
helper.bind_cycle_layout()

-- example of custom layout
local layout = "(split horizontal:0.5:0 "
         .."(clients vertical:0) (clients vertical:0))"
helper.hc("load " .. config.tag_names[0] .. " '" .. layout .. "'")

-- tag number 5
helper.hc("floating 5 on")

-- hc("set tree_style '╾│ ├└╼─┐'")
helper.hc("set tree_style '⊙│ ├╰»─╮'")

-- unlock, just to be sure
helper.hc("unlock")

-- launch statusbar panel (e.g. dzen2 or lemonbar)
helper.do_panel()

-- load on startup
startup.run()
