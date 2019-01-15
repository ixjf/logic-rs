local class = require "middleclass"

Alternatives = class "Alternatives"

local SequenceGroup = SequenceGroup or require "SequenceGroup"

function Alternatives:initialize(...)
    self.elements = {...}

    for i, v in ipairs(self.elements) do
        if (type(v.isInstanceOf) ~= "function" or not v:isInstanceOf(SequenceGroup)) then
            error("invalid element specified at index " .. i, 2)
        end
    end
end

function Alternatives:alternatives()
    return self.elements
end

return Alternatives