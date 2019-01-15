local class = require "middleclass"

Optional = class "Optional"

local Repeat = Repeat or require "Repeat"

function Optional:initialize(name)
    return Repeat(name)
end

return Optional