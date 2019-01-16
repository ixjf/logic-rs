local class = require "middleclass"

InputStream = class "InputStream"

function InputStream:initialize(input)
    assert(type(input) == "string", "(internal error) invalid value type for arg 'input'")

    self.line = 1
    self.col = 1
    self.input = input
    self.position = 1
end

function InputStream:eof()
    return self.position > utf8.len(self.input)
end

function InputStream:clone()
    local clone = InputStream(self.input)
    clone.line = self.line
    clone.col = self.col
    clone.position = self.position
    return clone
end

function InputStream:curr()
    if self:eof() then
        return nil
    end

    return utf8.char(utf8.codepoint(self.input, utf8.offset(self.input, self.position)))
end

function InputStream:advance()
    self.position = math.min(self.position + 1, utf8.len(self.input) + 1)

    if not self:eof() then
        if self:curr() == '\n' then
            self.line = self.line + 1
            self.col = 1
        else
            self.col = self.col + 1
        end

        return true
    else
        return false
    end
end

function InputStream:current_line()
    return self.line
end

function InputStream:current_col()
    return self.col
end

return InputStream