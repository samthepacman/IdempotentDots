local posix = require('posix')
local config  = require(".config")

local _M = {}

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-- helpers

function _M.hc(arguments)
    os.execute("herbstclient " .. arguments)
end

function _M.do_config(command, hash)
    -- loop over hash
    for key, value in pairs(hash) do 
        _M.hc(command .. ' ' .. key .. ' ' .. value)

        -- uncomment to debug in terminal
        -- print(command .. ' ' .. key .. ' ' .. value)
    end
end

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-- tags related

function _M.set_tags_with_name()
    local tag_names = config.tag_names
    local tag_keys = config.tag_keys    
    
    _M.hc("rename default '" .. tag_names[0] .. "' 2>/dev/null || true")
    
    for index, value in pairs(tag_names) do 
        _M.hc("add '" .. value .. "'");
        
        -- uncomment to debug in terminal
        -- print(index .. " | " .. value)

        local key = tag_keys[index]
        if (not (key == nil or key == '') ) then
            _M.hc("keybind Mod4-"..key.." use_index '"..index.."'")
            _M.hc("keybind Mod4-Shift-"..key.." move_index '"..index.."'")
        end
    end
end

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-- miscellanous

function _M.bind_cycle_layout() 
    -- The following cycles through the available layouts
    -- within a frame, but skips layouts, if the layout change 
    -- wouldn't affect the actual window positions.
    -- I.e. if there are two windows within a frame,
    -- the grid layout is skipped.

    _M.hc( "keybind Mod4-space "
     .. "or , and . compare tags.focus.curframe_wcount = 2 "
     .. ". cycle_layout +1 vertical horizontal max vertical grid "
     .. ", cycle_layout +1 ")
end

-- do multi monitor setup here, e.g.:
-- hc("set_monitors 1280x1024+0+0 1280x1024+1280+0");
-- or simply:
-- hc("detect_monitors");

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
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

function _M.do_panel() 
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
-- return

return _M
