local InputStream = InputStream or require "../InputStream"

function clone()
    local inp_stream1 = InputStream("ABCD")
    inp_stream1:advance()

    local inp_stream2 = inp_stream1:clone()
    inp_stream2:advance()

    assert(inp_stream1:current_col() ~= inp_stream2:current_col(), "clone() returned reference to original")
end

function curr()
    local inp_stream = InputStream("ABCD")
    assert(inp_stream:curr() == "A", "curr() did not return the correct character")
    inp_stream:advance()
    assert(inp_stream:curr() == "B", "curr() did not return the correct character")
    inp_stream:advance()
    assert(inp_stream:curr() == "C", "curr() did not return the correct character")
    inp_stream:advance()
    assert(inp_stream:curr() == "D", "curr() did not return the correct character")
    inp_stream:advance()
    assert(inp_stream:curr() == nil, "curr() did not return the correct character")
end

function advance()
    local inp_stream = InputStream("ABCD")
    assert(inp_stream:advance(), "advance() failed")
    assert(inp_stream:curr() == "B", "advance() did not advance")

    assert(inp_stream:advance(), "advance() failed")
    assert(inp_stream:curr() == "C", "advance() did not advance")

    assert(inp_stream:advance(), "advance() failed")
    assert(inp_stream:curr() == "D", "advance() did not advance")

    assert(not inp_stream:advance(), "advance() succeeded but we had reached eof")
    assert(inp_stream:curr() == nil, "advance() did not advance")
end

function eof()
    local inp_stream = InputStream("AB")
    assert(not inp_stream:eof(), "eof() returned incorrect state - we're not at eof")
    inp_stream:advance()
    assert(not inp_stream:eof(), "eof() returned incorrect state - we're not at eof")
    inp_stream:advance()
    assert(inp_stream:eof(), "eof() returned incorrect state - we're at eof")
end

function current_line()
    local inp_stream = InputStream("A\nBCD  \nFFF")
    assert(inp_stream:current_line() == 1, "current_line() returned wrong line")
    inp_stream:advance()
    assert(inp_stream:current_line() == 2, "current_line() returned wrong line")
    inp_stream:advance()
    inp_stream:advance()
    inp_stream:advance()
    inp_stream:advance()
    inp_stream:advance()
    inp_stream:advance()
    assert(inp_stream:current_line() == 3, "current_line() returned wrong line")
    inp_stream:advance()
    inp_stream:advance()
    inp_stream:advance()
    assert(inp_stream:current_line() == 3, "current_line() returned wrong line")
end

function current_col()
    local inp_stream = InputStream("ABC\nEF")
    assert(inp_stream:current_col() == 1, "current_col() returned wrong column")
    inp_stream:advance()
    assert(inp_stream:current_col() == 2, "current_col() returned wrong column")
    inp_stream:advance()
    assert(inp_stream:current_col() == 3, "current_col() returned wrong column")
    inp_stream:advance()
    assert(inp_stream:current_col() == 1, "current_col() returned wrong column")
    inp_stream:advance()
    assert(inp_stream:current_col() == 2, "current_col() returned wrong column")
    inp_stream:advance()
    assert(inp_stream:current_col() == 3, "current_col() returned wrong column")
end

clone()
curr()
advance()
eof()
current_line()
current_col()