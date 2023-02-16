local _M = {}

local gmc    = require ".gmc"

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-- tags array

_M.tag_names = {}; _M.tag_keys = {} -- new array

-- alternative 1

-- for name = 1,9 do table.insert(_M.tag_names, name) end
-- for name = 1,9 do table.insert(_M.tag_keys, name)  end
-- table.insert(_M.tag_keys, 0)

-- alternative 2
for i = 1,9 do _M.tag_names[i-1]=i end
for i = 1,9 do _M.tag_keys[i-1]=i  end
_M.tag_keys[9] = 0

-- alternative 3
-- http://lua-users.org/wiki/RangeIterator

-- uncomment to debug
-- for key, value in pairs(_M.tag_keys) do print(key.." | "..value) end

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
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

_M.keybinds = {
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

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
-- tags

_M.tagskeybinds = {
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

_M.mousebinds = {
  -- mouse    
    [m .. '-Button1']  = 'move',
    [m .. '-Button2']  = 'zoom',
    [m .. '-Button3']  = 'resize'
}

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
-- theme

_M.attributes = {
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

_M.sets = {
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

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
-- rules

_M.rules = {
  -- normally focus new clients
    ['focus=on'] = '',

  -- zero based array
    ['class=Firefox']  = 'tag=' .. _M.tag_names[1],
    ['class=Chromium'] = 'tag=' .. _M.tag_names[1],
    ['class=Geany']    = 'tag=' .. _M.tag_names[2],
    ['class=Thunar']   = 'tag=' .. _M.tag_names[3],
    ['class=gimp']     = 'tag=' .. _M.tag_names[4] ..' pseudotile=on',

    ['class=Oblogout'] = 'fullscreen=on',
    
    ["class~'(.*[Rr]xvt.*|.*[Tt]erm|Konsole)'"] = 'focus=on',

    ["windowtype~'_NET_WM_WINDOW_TYPE_(DIALOG|UTILITY|SPLASH)'"] = 'pseudotile=on',
  --["windowtype='_NET_WM_WINDOW_TYPE_DIALOG'"] = 'focus=on',
    ["windowtype='_NET_WM_WINDOW_TYPE_DIALOG'"] = 'fullscreen=on',
    ["windowtype~'_NET_WM_WINDOW_TYPE_(NOTIFICATION|DOCK|DESKTOP)'"] = 'manage=off'
}

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-- return

return _M
