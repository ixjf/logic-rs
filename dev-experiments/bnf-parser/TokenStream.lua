local class = require "middleclass"

TokenStream = class "TokenStream"

local Token = Token or require "Token"

function TokenStream:initialize(token_tree)
    assert(type(token_tree) == "table", "(internal error) invalid value type for arg 'token_tree'")
    assert(#token_tree > 0, "(internal error) token_tree is empty")

    for i, v in ipairs(token_tree) do
        assert(type(v) == "table" and Token.isInstanceOf(v.token, Token), "(internal error) token_tree contains invalid elements")
        assert(type(v.line) == "number", "(internal error) token_tree contains invalid elements")
        assert(type(v.col) == "number", "(internal error) token_tree contains invalid elements")
    end

    self.token_tree = token_tree
    self.position = 1
end

function TokenStream:curr()
    if self:eof() then
        return nil
    end

    return self.token_tree[self.position].token
end

function TokenStream:current_line()
    if self:eof() then
        return nil
    end

    return self.token_tree[self.position].line
end

function TokenStream:current_col()
    if self:eof() then
        return nil
    end

    return self.token_tree[self.position].col
end

function TokenStream:eof()
    return self.position > #self.token_tree
end

function TokenStream:advance()
    self.position = math.min(self.position + 1, #self.token_tree + 1)
    return not self:eof()
end

function TokenStream:clone()
    local clone = TokenStream(self.token_tree)
    clone.position = self.position
    return clone
end

return TokenStream