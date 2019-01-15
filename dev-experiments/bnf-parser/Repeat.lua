local class = require "middleclass"

Repeat = class "Repeat"

local Alternatives = Alternatives or require "Alternatives"
local Range = Range or require "Range"
local Char = Char or require "Char"

function Repeat:initialize(element, min, max)
    if type(element) ~= "number" and 
       type(element) ~= "string" and
       not Alternatives.isInstanceOf(element, Alternatives) and
       not Range.isInstanceOf(element, Range) and
       not Char.isInstanceOf(element, Char)
    then
        error("invalid element specified at index " .. i, 2)
    end

    if type(min) ~= "number" and type(min) ~= "nil" then
        error("invalid value type specified for 'min'", 2)
    end

    if type(max) ~= "number" and type(max) ~= "nil" then
        error("invalid value type specified for 'max'", 2)
    end

    if type(min) ~= nil and type(max) ~= nil and min > max then
        error("minimum value cannot be higher than maximum value", 2)
    end

    if type(min) ~= nil and type(max) ~= nil and min < 0 then
        error("minimum value cannot be negative", 2)
    end

    self.element = element
    self.min = min or 0
    self.max = max or nil -- (max or infinity)
end

function Repeat:element()
    return self.element
end

function Repeat:min()
    return self.min
end

function Repeat:max()
    return self.max
end

return Repeat