#!/usr/bin/lua
-- ------------------------------------------------------------------
--
--     Description: unified config for herbstluftwm lemonbar
--     Created by: Epsi Nurwijayadi <epsi.nurwijayadi@gmail.com)
--
--     Source
--     https://github.com/epsi-rns/dotfiles/tree/master/standalone/lemon-hlwm/lua
--
--     Blog
--     http://epsi-rns.github.io/desktop/2017/06/11/herbstlustwm-event-idle-overview.html
--     http://epsi-rns.github.io/desktop/2017/06/07/herbstlustwm-tag-status-lua.html
--     http://epsi-rns.github.io/desktop/2017/06/17/herbstlustwm-event-idle-lua.html
--
-- ------------------------------------------------------------------

-- luaposix available in AUR
local posix = require "posix"

local dirname  = debug.getinfo(1).source:match("@?(.*/)")
package.path   = package.path .. ';' .. dirname .. '?.lua;'
  
local gmc = require('.gmc')

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-- common

local common = {}

-- common functions

function common.sleep (n)
    local t = os.clock()
    while os.clock() - t <= n do
        -- nothing
    end
end

-- https://stackoverflow.com/questions/1426954/split-string-in-lua?rq=1
function common.split(inputstr, sep)
        if sep == nil then
                sep = "%s"
        end
        local t={} ; i=1 -- non zero based
        for str in string.gmatch(inputstr, "([^"..sep.."]+)") do
                t[i] = str
                i = i + 1
        end
        return t
end

-- http://lua-users.org/wiki/StringTrim
function common.trim1(s)
  return (s:gsub("^%s*(.-)%s*$", "%1"))
end

-- https://stackoverflow.com/questions/33510736/check-if-array-contains-specific-value
function common.has_value (tab, val)
    for index, value in ipairs(tab) do
        if value == val then
            return true
        end
    end

    return false
end

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-- helper

local helper = {}

-- script arguments
function helper.get_monitor(arguments)
    -- no ternary operator, using expression
    return (#arguments > 0) and tonumber(arguments[1]) or 0
end

-- geometry calculation

function helper.get_geometry(monitor)
    local command = 'herbstclient monitor_rect ' .. monitor
    local handle = io.popen(command)
    local result = handle:read("*a")
    handle:close()

    if (result == nil or result == '') then
        print('Invalid monitor ' .. monitors)
        os.exit()
    end      
        
    local raw = common.trim1(result)  
    local geometry = common.split(raw, ' ')

    return geometry
end

function helper.get_top_panel_geometry(height, geometry)
    -- geometry has the format X Y W H
    return tonumber(geometry[1]), tonumber(geometry[2]),
           tonumber(geometry[3]), height
end

function helper.get_bottom_panel_geometry(height, geometry)
    -- geometry has the format X Y W H
    return tonumber(geometry[1]) + 0, tonumber(geometry[4]) - height, 
           tonumber(geometry[3]) - 0, height
end

-- lemon Parameters

function helper.get_params_top(monitor, panel_height)
    -- calculate geometry
    local geometry = helper.get_geometry(monitor)
    local xpos, ypos, width, height = helper.get_top_panel_geometry(
        panel_height, geometry)

    -- geometry: -g widthxheight+x+y
    local geom_res = tostring(width) .. 'x' .. tostring(height)
           .. '+' .. tostring(xpos)  .. '+' .. tostring(ypos)

    -- color, with transparency    
    local bgcolor = "'#aa000000'"
    local fgcolor = "'#ffffff'"

    -- XFT: require lemonbar_xft_git 
    local font_takaop  = "takaopgothic-9"
    local font_symbol  = "PowerlineSymbols-11"
    local font_awesome = "FontAwesome-9"
  
    local parameters = ""
        .. " -g "..geom_res.." -u 2"
        .. " -B "..bgcolor.." -F "..fgcolor
        .. " -f "..font_takaop
        .. " -f "..font_awesome
        .. " -f "..font_symbol
        
    return parameters
end

function helper.get_params_bottom(monitor, panel_height)
    -- calculate geometry
    local geometry = helper.get_geometry(monitor)
    local xpos, ypos, width, height = helper.get_bottom_panel_geometry(
        panel_height, geometry)

    -- geometry: -g widthxheight+x+y
    local geom_res = tostring(width) .. 'x' .. tostring(height)
           .. '+' .. tostring(xpos)  .. '+' .. tostring(ypos)

    -- color, with transparency    
    local bgcolor = "'#aa000000'"
    local fgcolor = "'#ffffff'"

    -- XFT: require lemonbar_xft_git 
    local font_mono    = "monospace-9"
    local font_symbol  = "PowerlineSymbols-11"
    local font_awesome = "FontAwesome-9"
  
    local parameters = ""
        .. " -g "..geom_res.." -u 2"
        .. " -B "..bgcolor.." -F "..fgcolor
        .. " -f "..font_mono
        .. " -f "..font_awesome
        .. " -f "..font_symbol
        
    return parameters
end

function helper.get_lemon_parameters(monitor, panel_height)
    return helper.get_params_top(monitor, panel_height)
end

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-- output

local output = {}

-- initialize

-- assuming $ herbstclient tag_status
-- 	#1	:2	:3	:4	.5	.6	.7	.8	.9

-- custom tag names
output.tag_shows = {'一 ichi', '二 ni', '三 san', '四 shi', 
  '五 go', '六 roku', '七 shichi', '八 hachi', '九 kyū', '十 jū'}

-- initialize variable segment
output.segment_windowtitle = '' -- empty string
output.tags_status         = {} -- empty table
output.segment_datetime    = '' -- empty string

-- decoration

output.separator = '%{B-}%{F' .. gmc.color['yellow500'] .. '}|%{B-}%{F-}'

-- Powerline Symbol
output.right_hard_arrow = ""
output.right_soft_arrow = ""
output.left_hard_arrow  = ""
output.left_soft_arrow  = ""

-- theme
output.pre_icon    = '%{F' .. gmc.color['yellow500'] .. '}'
output.post_icon   = '%{F-}'

-- main

function output.get_statusbar_text(monitor)
    local text = ''
    
    -- draw tags, non zero based
    text = text .. '%{l}'
    for index = 1, #(output.tags_status) do
        text = text .. output.output_by_tag(monitor, output.tags_status[index])
    end

    -- draw date and time
    text = text .. '%{c}'
    text = text .. output.output_by_datetime()

    -- draw window title 
    text = text .. '%{r}'
    text = text .. output.output_by_title()
  
    return text
end

-- each segments

function output.output_by_tag(monitor, tag_status)
    local tag_index  = string.sub(tag_status, 2, 2)
    local tag_mark   = string.sub(tag_status, 1, 1)
    local index      = tonumber(tag_index)-- not a zero based array
    local tag_name   = output.tag_shows[index]

    -- ----- pre tag

    local text_pre = ''
    if tag_mark == '#' then
        text_pre = '%{B' .. gmc.color['blue500'] .. '}'
                .. '%{F' .. gmc.color['black'] .. '}'
                .. '%{U' .. gmc.color['white'] .. '}%{+u}' 
                .. output.right_hard_arrow
                .. '%{B' .. gmc.color['blue500'] .. '}'
                .. '%{F' .. gmc.color['white'] .. '}'
                .. '%{U' .. gmc.color['white'] .. '}%{+u}'
    elseif tag_mark == '+' then
        text_pre = '%{B' .. gmc.color['yellow500'] .. '}'
                .. '%{F' .. gmc.color['grey400'] .. '}'
    elseif tag_mark == ':' then
        text_pre = '%{B-}%{F' .. gmc.color['white'] .. '}'
                .. '%{U' .. gmc.color['red500'] .. '}%{+u}'
    elseif tag_mark == '!' then
        text_pre = '%{B' .. gmc.color['red500'] .. '}'
                .. '%{F' .. gmc.color['white'] .. '}'
                .. '%{U' .. gmc.color['white'] .. '}%{+u}'
    else
        text_pre = '%{B-}%{F' .. gmc.color['grey600'] .. '}%{-u}'
    end

    -- ----- tag by number

    -- clickable tags
    local text_name = '%{A:herbstclient focus_monitor '
                   .. '"' .. monitor .. '" && '
                   .. 'herbstclient use "' .. tag_index .. '":}'
                   .. ' ' .. tag_name ..' %{A} '
    
    -- non clickable tags
    -- local text_name = ' ' .. tag_name .. ' '

    -- ----- post tag

    local text_post = ""
    if (tag_mark == '#') then
        text_post = '%{B-}' 
                 .. '%{F' .. gmc.color['blue500'] .. '}' 
                 .. '%{U' .. gmc.color['red500'] .. '}%{+u}' 
                 .. output.right_hard_arrow
    end

    text_clear = '%{B-}%{F-}%{-u}'

     
    return text_pre .. text_name .. text_post .. text_clear
end

function output.output_by_title()
    local text = output.segment_windowtitle .. ' ' .. output.separator .. '  '

    return text
end

function output.output_by_datetime()
    return output.segment_datetime
end

-- setting variables, response to event handler

function output.set_tag_value(monitor)
    local command = 'herbstclient tag_status ' .. monitor
    local handle = io.popen(command)
    local result = handle:read("*a")
    handle:close() 
        
    local raw = common.trim1(result)  
    output.tags_status = common.split(raw, "\t")
end

function output.set_windowtitle(windowtitle)
    local icon = output.pre_icon .. '' .. output.post_icon
    if (windowtitle == nil) then windowtitle = '' end

    output.segment_windowtitle = ' ' .. icon ..
        ' %{B-}%{F' .. gmc.color['grey700'] .. '} ' .. windowtitle
end

function output.set_datetime()
    local date_icon = output.pre_icon .. '' .. output.post_icon
    local date_str  = os.date('%a %b %d')     
    local date_text = date_icon .. ' %{B-}'
        .. '%{F' .. gmc.color['grey700'] .. '} ' .. date_str

    local time_icon = output.pre_icon .. '' .. output.post_icon
    local time_str  = os.date('%H:%M:%S')
    local time_text = time_icon .. ' %{B-}'
        .. '%{F' .. gmc.color['blue500'] .. '} ' .. time_str

    output.segment_datetime = date_text .. '  ' .. time_text
end

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-- pipe handler

local pipehandler = {}

-- helper

-- because os.clock function will hogs your cpu
function pipehandler.os_sleep(n)
  os.execute('sleep ' .. tonumber(n))
end

function pipehandler.handle_command_event(monitor, event)
    -- find out event origin
    local column = common.split(event, "\t")
    local origin = column[1] -- non zero based

    local tag_cmds = {'tag_changed', 
        'tag_flags', 'tag_added', 'tag_removed'}
    local title_cmds = {'window_title_changed', 'focus_changed'}

    if origin == 'reload' then
        os.execute('pkill lemonbar')
    elseif origin == 'quit_panel' then
        os.exit()
    elseif common.has_value(tag_cmds, origin) then
        output.set_tag_value(monitor)
    elseif common.has_value(title_cmds, origin) then
        local title = (#column > 2) and (column[3]) or ''
        output.set_windowtitle(title)
    elseif origin == 'interval' then
        output.set_datetime()
    end
end

function pipehandler.content_init(monitor, pipe_lemon_out)
    -- initialize statusbar before loop
    output.set_tag_value(monitor)
    output.set_windowtitle('')
    output.set_datetime()

    local text = output.get_statusbar_text(monitor)
    pipe_lemon_out:write(text .. "\n")
    pipe_lemon_out:flush()
end

function pipehandler.content_event_idle(pipe_cat_out)
    local pid_idle = posix.fork()

    if pid_idle == 0 then -- this is the child process
        -- start a pipe
        command_in = 'herbstclient --idle'
        local pipe_in  = assert(io.popen(command_in,  'r'))
  
        -- wait for each event 
        for event in pipe_in:lines() do
            posix.write(pipe_cat_out, event)
            io.flush()
        end -- for loop
   
        pipe_in:close()
    else             -- this is the parent process
        -- nothing
    end
end

function pipehandler.content_event_interval(pipe_cat_out) 
    local pid_interval = posix.fork()

    if pid_interval == 0 then -- this is the child process
        while true do
            posix.write(pipe_cat_out, "interval\n")
            io.flush() 

            pipehandler.os_sleep(1)
        end
    else             -- this is the parent process
        -- nothing
    end
end

function pipehandler.content_walk(monitor, pipe_lemon_out)  
    rd, wr = posix.pipe()

    pipehandler.content_event_idle(wr)
    pipehandler.content_event_interval(wr)

    local bufsize = 4096
    local event = ''

    while true do
        -- wait for next event, trim newline
        event = common.trim1(posix.read(rd, bufsize))
        if event == nil or #event == 0 then break end
    
        pipehandler.handle_command_event(monitor, event)    
    
        text = output.get_statusbar_text(monitor)
        pipe_lemon_out:write(text .. "\n")
        pipe_lemon_out:flush()
    end -- not using for loop

    posix.close(rd)
    posix.close(wr)
end

function pipehandler.run_lemon(monitor, parameters) 
    -- no bidirectional in Lua, using shell pipe instead
    local command_out  = 'lemonbar ' .. parameters .. ' | sh'
    local pipe_lemon_out = assert(io.popen(command_out, 'w'))
    
    pipehandler.content_init(monitor, pipe_lemon_out)
    pipehandler.content_walk(monitor, pipe_lemon_out) -- loop for each event
        
    pipe_lemon_out:close()
end

function pipehandler.detach_lemon(monitor, parameters)
    local pid_lemon = posix.fork()

    if pid_lemon == 0 then -- this is the child process
        pipehandler.run_lemon(monitor, parameters)
    else             -- this is the parent process
        -- nothing
    end
end

function pipehandler.detach_lemon_conky(parameters)
    local pid_conky = posix.fork()

    if pid_conky == 0 then -- this is the child process
        local cmd_out  = 'lemonbar ' .. parameters
        local pipe_out = assert(io.popen(cmd_out, 'w'))

        local dirname  = debug.getinfo(1).source:match("@?(.*/)")
        local path     = dirname .. "../conky"
        local cmd_in   = 'conky -c ' .. path .. '/conky-lemonbar.lua'
        local pipe_in  = assert(io.popen(cmd_in,  'r'))

        for line in pipe_in:lines() do
            pipe_out:write(line.."\n")
            pipe_out:flush()
        end -- for loop
   
        pipe_in:close()    
        pipe_out:close()
    else                   -- this is the parent process
        -- nothing
    end
end

function pipehandler.kill_zombie()
    os.execute('pkill -x dzen2')
    os.execute('pkill -x lemonbar')
    os.execute('pkill -x cat')
    os.execute('pkill conky')
    os.execute('pkill herbstclient')
end

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-- main

local panel_height = 24
local monitor = helper.get_monitor(arg)

pipehandler.kill_zombie()
os.execute('herbstclient pad ' .. monitor .. ' ' 
    .. panel_height .. ' 0 ' .. panel_height .. ' 0')

-- run process in the background

local params_top = helper.get_params_top(monitor, panel_height)
pipehandler.detach_lemon(monitor, params_top)

local params_bottom = helper.get_params_bottom(monitor, panel_height)
pipehandler.detach_lemon_conky(params_bottom)
