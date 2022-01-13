#!/usr/bin/lua
-- ------------------------------------------------------------------
--
--     Description: unified config for herbstluftwm dzen2 statusbar
--     Created by: Epsi Nurwijayadi <epsi.nurwijayadi@gmail.com)
--
--     Source
--     https://github.com/epsi-rns/dotfiles/tree/master/standalone/dzen2-hlwm/lua
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

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
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

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-- dzen Parameters

function helper.get_params_top(monitor, panel_height)
    local geometry = helper.get_geometry(monitor)
    local xpos, ypos, width, height = helper.get_top_panel_geometry(
        panel_height, geometry)
    
    local bgcolor = '#000000'
    local fgcolor = '#ffffff'
    local font    = '-*-takaopgothic-medium-*-*-*-12-*-*-*-*-*-*-*'
  
    local parameters = ""
        .. " -x " .. tostring(xpos)  .. " -y " .. tostring(ypos)
        .. " -w " .. tostring(width) .. " -h " .. tostring(height)
        .. " -ta l -bg '" .. bgcolor .. "' -fg '" .. fgcolor .. "'"
        .. " -title-name dzentop"
        .. " -fn '" .. font .. "'"

    return parameters
end

function helper.get_params_bottom(monitor, panel_height)
    local geometry = helper.get_geometry(monitor)
    local xpos, ypos, width, height = helper.get_bottom_panel_geometry(
        panel_height, geometry)
    
    local bgcolor = '#000000'
    local fgcolor = '#ffffff'
    local font    = '-*-fixed-medium-*-*-*-11-*-*-*-*-*-*-*'
  
    local parameters = ""
        .. " -x " .. tostring(xpos)  .. " -y " .. tostring(ypos)
        .. " -w " .. tostring(width) .. " -h " .. tostring(height)
        .. " -ta l -bg '" .. bgcolor .. "' -fg '" .. fgcolor .. "'"
        .. " -title-name dzenbottom"
        .. " -fn '" .. font .. "'"

    return parameters
end

function helper.get_dzen2_parameters(monitor, panel_height)
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

output.separator = '^bg()^fg(' .. gmc.color['black'] .. ')|^bg()^fg()'

-- http://fontawesome.io/
output.font_awesome = '^fn(FontAwesome-9)'

-- Powerline Symbol
output.right_hard_arrow = '^fn(powerlinesymbols-14)^fn()'
output.right_soft_arrow = '^fn(powerlinesymbols-14)^fn()'
output.left_hard_arrow  = '^fn(powerlinesymbols-14)^fn()'
output.left_soft_arrow  = '^fn(powerlinesymbols-14)^fn()'

-- theme
output.pre_icon    = '^fg(' .. gmc.color['yellow500'] .. ')' 
              .. output.font_awesome
output.post_icon   = '^fn()^fg()'

-- main

function output.get_statusbar_text(monitor)
    local text = ''
    
    -- draw tags, non zero based
    for index = 1, #(output.tags_status) do
        text = text .. 
               output.output_by_tag(monitor, output.tags_status[index])
    end

    -- draw date and time
    text = text .. output.output_by_datetime()

    -- draw window title    
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
        text_pre = '^bg(' .. gmc.color['blue500'] .. ')'
                .. '^fg(' .. gmc.color['black'] .. ')'
                .. output.right_hard_arrow
                .. '^bg(' .. gmc.color['blue500'] .. ')'
                .. '^fg(' .. gmc.color['white'] .. ')'
    elseif tag_mark == '+' then
        text_pre = '^bg(' .. gmc.color['yellow500'] .. ')'
                .. '^fg(' .. gmc.color['grey400'] .. ')'
    elseif tag_mark == ':' then
        text_pre = '^bg()'
                 .. '^fg(' .. gmc.color['white'] .. ')'
    elseif tag_mark == '!' then
        text_pre = '^bg(' .. gmc.color['red500'] .. ')'
                .. '^fg(' .. gmc.color['white'] .. ')'
    else
        text_pre = '^bg()'
                .. '^fg(' .. gmc.color['grey600'] .. ')'
    end

    -- ----- tag by number
    
    -- assuming using dzen2_svn
    -- clickable tags if using SVN dzen
    local text_name = '^ca(1,herbstclient focus_monitor '
                   .. '"' .. monitor .. '" && '
                   .. 'herbstclient use "' .. tag_index .. '")'
                   .. ' ' .. tag_name ..' ^ca()'

    -- ----- post tag

    local text_post = ""
    if (tag_mark == '#') then
        text_post = '^bg(' .. gmc.color['black'] .. ')'
                       .. '^fg(' .. gmc.color['blue500'] .. ')'
                       .. output.right_hard_arrow
    end

     
    return text_pre .. text_name .. text_post
end

function output.output_by_title()
    local text = ' ^r(5x0) ' .. output.separator .. ' ^r(5x0) '
               .. output.segment_windowtitle

    return text
end

function output.output_by_datetime()
    local text = ' ^r(5x0) ' .. output.separator .. ' ^r(5x0) '
               .. output.segment_datetime

    return text
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
        ' ^bg()^fg(' .. gmc.color['grey700'] .. ') ' .. windowtitle
end

function output.set_datetime()
    local date_icon = output.pre_icon .. '' .. output.post_icon
    local date_str  = os.date('%a %b %d')     
    local date_text = date_icon .. ' ^bg()'
        .. '^fg(' .. gmc.color['grey700'] .. ') ' .. date_str

    local time_icon = output.pre_icon .. '' .. output.post_icon
    local time_str  = os.date('%H:%M:%S')
    local time_text = time_icon .. ' ^bg()'
        .. '^fg(' .. gmc.color['blue500'] .. ') ' .. time_str

    output.segment_datetime = date_text .. '  ' .. time_text
end

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-- pipe handler

local pipehandler = {}

-- because os.clock function will hogs your cpu
function pipehandler.os_sleep(n)
  os.execute('sleep ' .. tonumber(n))
end

function pipehandler.handle_command_event(monitor, event)
    -- find out event origin
    column = common.split(event, "\t")
    origin = column[1] -- non zero based

    tag_cmds = {'tag_changed', 'tag_flags', 'tag_added', 'tag_removed'}
    title_cmds = {'window_title_changed', 'focus_changed'}

    if origin == 'reload' then
        os.execute('pkill dzen2')
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

function pipehandler.content_init(monitor, pipe_dzen2_out)
    -- initialize statusbar before loop
    output.set_tag_value(monitor)
    output.set_windowtitle('')
    output.set_datetime()

    local text = output.get_statusbar_text(monitor)
    pipe_dzen2_out:write(text .. "\n")
    pipe_dzen2_out:flush()
end

function pipehandler.content_event_idle(pipe_cat_out)
    local pid = posix.fork()

    if pid == 0 then -- this is the child process
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
    local pid = posix.fork()

    if pid == 0 then -- this is the child process
        while true do
            posix.write(pipe_cat_out, "interval\n")
            io.flush() 

            pipehandler.os_sleep(1)
        end
    else             -- this is the parent process
        -- nothing
    end
end

function pipehandler.content_walk(monitor, pipe_dzen2_out)  
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
        pipe_dzen2_out:write(text .. "\n")
        pipe_dzen2_out:flush()
    end -- not using for loop

    posix.close(rd)
    posix.close(wr)
end

function pipehandler.run_dzen2(monitor, parameters) 
    local command_out    = 'dzen2 ' .. parameters
    local pipe_dzen2_out = assert(io.popen(command_out, 'w'))
    
    pipehandler.content_init(monitor, pipe_dzen2_out)
    pipehandler.content_walk(monitor, pipe_dzen2_out) -- loop for each event
        
    pipe_dzen2_out:close()
end

function pipehandler.detach_dzen2(monitor, parameters)
    local pid_dzen2 = posix.fork()

    if pid_dzen2 == 0 then -- this is the child process
        pipehandler.run_dzen2(monitor, parameters)
    else             -- this is the parent process
        -- nothing
    end
end

function pipehandler.detach_dzen2_conky(parameters)
    local pid_conky = posix.fork()

    if pid_conky == 0 then -- this is the child process
        local cmd_out  = 'dzen2 ' .. parameters
        local pipe_out = assert(io.popen(cmd_out, 'w'))

        local dirname  = debug.getinfo(1).source:match("@?(.*/)")
        local path     = dirname .. "../conky"
        local cmd_in   = 'conky -c ' .. path .. '/conky-dzen2.lua'
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

function pipehandler.detach_transset()
    local pid_transset = posix.fork()

    if pid_transset == 0 then -- this is the child process
        common.sleep(1)
        os.execute('transset .8 -n dzentop    >/dev/null') 
        os.execute('transset .8 -n dzenbottom >/dev/null') 
    else                      -- this is the parent process
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

panel_height = 24
monitor = helper.get_monitor(arg)

pipehandler.kill_zombie()
os.execute('herbstclient pad ' .. monitor .. ' ' 
    .. panel_height .. ' 0 ' .. panel_height .. ' 0')

-- run process in the background

--local params_top = helper.get_params_top(monitor, panel_height)
--pipehandler.detach_dzen2(monitor, params_top)

local params_bottom = helper.get_params_bottom(monitor, panel_height)
pipehandler.detach_dzen2_conky(params_bottom)

-- optional transparency
--pipehandler.detach_transset()
