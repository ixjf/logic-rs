local class = require "middleclass"

Char = class "Char"

function Char:initialize(char)
    if type(char) ~= "string" then
        error("invalid value type for arg 'char'", 2)
    end

    if utf8.len(char) ~= 1 then
        error("invalid value specified for arg 'char'", 2)
    end

    self.char = char
end

function Char:try_match(char)
    print("Char:try_match (self.char: ", self.char, " - char: ", char, ")")
    return char == self.char
end

return Char