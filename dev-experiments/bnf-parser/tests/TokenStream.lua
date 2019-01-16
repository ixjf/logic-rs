local TokenStream = TokenStream or require "../TokenStream"
local Token = Token or require "../Token"

function curr()
    local tok_stream = TokenStream({{token = Token("rule1"), line = 0, col = 0}, {token = Token("rule2"), line = 0, col = 0}})
    assert(Token.isInstanceOf(tok_stream:curr(), Token), "curr() returned invalid token")
    assert(tok_stream:curr():of_rule() == "rule1", "curr() returned wrong token")
    tok_stream:advance()
    assert(Token.isInstanceOf(tok_stream:curr(), Token), "curr() returned invalid token")
    assert(tok_stream:curr():of_rule() == "rule2", "curr() returned wrong token")
    tok_stream:advance()
    assert(tok_stream:curr() == nil, "curr() returned invalid token")
end

function eof()
    local tok_stream = TokenStream({{token = Token("rule1"), line = 0, col = 0}, {token = Token("rule2"), line = 0, col = 0}})
    tok_stream:advance()
    tok_stream:advance()
    assert(tok_stream:eof(), "eof() returned incorrect state")
end

function advance()
    local tok_stream = TokenStream({{token = Token("rule1"), line = 0, col = 0}, {token = Token("rule2"), line = 0, col = 0}})
    assert(tok_stream:advance(), "advance() failed when it shouldn't")
    assert(tok_stream:curr():of_rule() == "rule2", "advance() failed to... advance")
    assert(not tok_stream:advance(), "advance() succeeded but we had reached eof")
end

function clone()
    local tok_stream = TokenStream({{token = Token("rule1"), line = 0, col = 0}, {token = Token("rule2"), line = 0, col = 0}})
    local tok_stream2 = tok_stream1:clone()

    tok_stream2:advance()
    
    assert(tok_stream2:curr():of_rule() ~= tok_stream1:curr():of_rule(), "clone() returned reference to original")
end

function current_line()
    local tok_stream = TokenStream({{token = Token("rule1"), line = 1, col = 2}, {token = Token("rule2"), line = 4, col = 3}})
    assert(tok_stream:current_line() == 1, "current_line() returned wrong line")

    tok_stream:advance()
    assert(tok_stream:current_line() == 4, "current_line() returned wrong line")
end

function current_col()
    local tok_stream = TokenStream({{token = Token("rule1"), line = 1, col = 2}, {token = Token("rule2"), line = 4, col = 3}})
    assert(tok_stream:current_col() == 2, "current_col() returned wrong line")

    tok_stream:advance()
    assert(tok_stream:current_col() == 3, "current_col() returned wrong line")
end

curr()
eof()
advance()
clone()
current_line()
current_col()