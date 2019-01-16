local class = require "middleclass"

Error = class "Error"

Error.static.Types = {
    UnrecognizedToken = "UnrecognizedToken",
    UnrecognizedExpr = "UnrecognizedExpr"
}

function Error:initialize(err, line, col)
    assert(Error.static.Types[err], "(internal error) invalid value passed for arg 'err'")
    assert(type(line) == "number", "(internal error) invalid value passed for arg 'line'")
    assert(type(col) == "number", "(internal error) invalid value passed for arg 'col'")

    self.err = err
    self.line = line
    self.col = col
end

function Error:err_type()
    return self.err
end

function Error:err_line()
    return self.line
end

function Error:err_col()
    return self.col
end

return Error