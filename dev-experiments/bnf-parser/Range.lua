local class = require "middleclass"

Range = class "Range"

function Range:initialize(first, last)
    if type(first) ~= "number" and type(last) ~= "number" then
        if type(first) ~= "string" and type(last) ~= "string" then
            error("invalid values specified for range", 2)
        end
    end

    self.first = first
    self.last = last
end

function Range:try_match(char)
    assert(type(char) == "string", "(internal error) try_match called with non-string value")
    assert(utf8.len(char) == 1, "(internal error) try_match not called with single character")

    if type(self.first) == "number" and type(self.last) == "number" then
        local n = tonumber(char)
        
        if not n then
            return false
        end

        if self.first >= n or self.last <= n then
            return false
        end
        
        return true
    elseif type(self.first) == "string" and type(self.last) == "string" then
        local codepoint_first = utf8.codepoint(self.first)
        local codepoint_last = utf8.codepoint(self.last)
        local codepoint_char = utf8.codepoint(char)
        return codepoint_first <= codepoint_char and codepoint_last >= codepoint_char
    end
end

return Range