local helper  = require('.helper')

local _M = {}

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-- load on startup

function _M.run()
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
-- return

return _M
