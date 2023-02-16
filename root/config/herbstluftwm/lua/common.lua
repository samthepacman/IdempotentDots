local _M = {}

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-- common functions

function _M.sleep (n)
    local t = os.clock()
    while os.clock() - t <= n do
        -- nothing
    end
end

-- https://stackoverflow.com/questions/1426954/split-string-in-lua?rq=1
function _M.split(inputstr, sep)
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
function _M.trim1(s)
  return (s:gsub("^%s*(.-)%s*$", "%1"))
end

-- https://stackoverflow.com/questions/33510736/check-if-array-contains-specific-value
function _M.has_value (tab, val)
    for index, value in ipairs(tab) do
        if value == val then
            return true
        end
    end

    return false
end

-- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-- return

return _M
