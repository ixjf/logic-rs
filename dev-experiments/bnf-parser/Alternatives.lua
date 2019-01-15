local class = require "middleclass"

Alternatives = class "Alternatives"

local SequenceGroup = SequenceGroup or require "SequenceGroup"

function Alternatives:initialize(...)
    self.elements = {...}

    for i, v in ipairs(self.elements) do
        if not SequenceGroup.isInstanceOf(v, SequenceGroup) then
            error("invalid element specified at index " .. i, 2)
        end
    end
end

function Alternatives:alternatives()
    return self.elements
end

function Alternatives:add(...)
    local elements = {...}

    for i, v in ipairs(elements) do
        if not SequenceGroup.isInstanceOf(v, SequenceGroup) then
            error("invalid element specified at index " .. i, 2)
        end

        table.insert(self.elements, v)
    end
end

return Alternatives