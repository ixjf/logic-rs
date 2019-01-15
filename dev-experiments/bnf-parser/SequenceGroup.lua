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
           (type(v.isInstanceOf) ~= "function" or not v:isInstanceOf(Alternatives)) and
           (type(v.isInstanceOf) ~= "function" or not v:isInstanceOf(Repeat)) and
           (type(v.isInstanceOf) ~= "function" or not v:isInstanceOf(Optional)) and
           (type(v.isInstanceOf) ~= "function" or not v:isInstanceOf(Range)) and
           (type(v.isInstanceOf) ~= "function" or not v:isInstanceOf(Char))
        then
            error("invalid element specified at index " .. i, 2)
        end
    end
end

function SequenceGroup:all_elements()
    return self.elements
end

return SequenceGroup