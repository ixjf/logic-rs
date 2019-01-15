local class = require "middleclass"

SequenceGroup = class "SequenceGroup"

local Alternatives = Alternatives or require "Alternatives"
local Repeat = Repeat or require "Repeat"
local Optional = Optional or require "Optional"
local Range = Range or require "Range"
local Char = Char or require "Char"

function SequenceGroup:initialize(...)
    self.elements = {...}

    assert(#self.elements > 0)

    for i, v in ipairs(self.elements) do
        if type(v) ~= "number" and 
           type(v) ~= "string" and
           not Alternatives.isInstanceOf(v, Alternatives) and
           not Repeat.isInstanceOf(v, Repeat) and
           not Optional.isInstanceOf(v, Optional) and
           not Range.isInstanceOf(v, Range) and
           not Char.isInstanceOf(v, Char)
        then
            error("invalid element specified at index " .. i, 2)
        end
    end
end

function SequenceGroup:all_elements()
    return self.elements
end

return SequenceGroup