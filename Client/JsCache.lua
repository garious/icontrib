-- JsCache is used to concatenate Yoink modules

local function readfile(p)
    local f = assert(io.open(p, 'r'))
    local s = f:read('*all');
    f:close()
    return s
end

-- Get output file
local out
if arg[1] == '-o' then
    local outfile = arg[2]
    out = assert( io.open(outfile, 'w') )

    table.remove(arg, 1)  -- pop
    table.remove(arg, 1)  -- pop
else
    out = io.stdout
end

-- The rest of the arguments are files
local files = arg

out:write('var PRELOADED_MODULES = {\n')
for i,path in ipairs(files) do
    local text = readfile(path)
    out:write('"/' .. path .. '": ')
    out:write('(function (Yoink) {"use strict";\n')
    out:write(text)
    out:write('\n})')

    if i ~= #files then
        out:write(',\n')
    end
end
out:write('};\n')

out:close()
   
