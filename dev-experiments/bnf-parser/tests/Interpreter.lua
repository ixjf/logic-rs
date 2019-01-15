local Interpreter = Interpreter or require "Interpreter"
local Grammar = Grammar or require "Grammar"
local SequenceGroup = SequenceGroup or require "SequenceGroup"
local Char = Char or require "Char"
local Alternatives = Alternatives or require "Alternatives"

local g = Grammar()
    :add_rule("logical-conjunction", SequenceGroup(Char("("), "statement", Char("&"), "statement", Char(")")))
    :add_rule("statement", SequenceGroup(Alternatives(SequenceGroup("simple-statement"), SequenceGroup("complex-statement"))))
    :add_rule("simple-statement", SequenceGroup(Range("A", "Z"), Range(utf8.char(0x2081), utf8.char(0x2089))))
    :add_rule("complex-statement", SequenceGroup(Alternatives(SequenceGroup("logical-conjunction"), SequenceGroup("logical-disjunction"), SequenceGroup("logical-negation"), SequenceGroup("logical-conditional"))))
    :add_rule("logical-disjunction", SequenceGroup(Char("("), "statement", Char(utf8.char(0x2228)), "statement", Char(")")))
    :add_rule("logical-negation", SequenceGroup(Char("~"), "statement"))
    :add_rule("logical-conditional", SequenceGroup(Char("("), "statement", Char(utf8.char(0x2283), "statement", Char(")"))))


function parses_valid_statement()
    local i = Interpreter(g)
    assert(i:parse("(A" .. utf8.char(0x2081) .. "&B" .. utf8.char(0x2082) .. ")", "statement"), "failed to parse valid logical conjunction")
    assert(i:parse("F" .. utf8.char(0x2084), "statement"), "failed to parse valid simple statement")
    assert(i:parse("~Z" .. utf8.char(0x2089), "statement"), "failed to parse valid logical negation")
    assert(i:parse("(C" .. utf8.char(0x2086) .. utf8.char(0x2283) .. "D" .. utf8.char(0x2087) .. ")", "statement"), "failed to parse valid logical disjunction")
    assert(i:parse("(K" .. utf8.char(0x2088) .. utf8.char(0x2228) .. "M" .. utf8.char(0x2083) .. ")", "statement"), "failed to parse valid logical conditional")
end

parses_valid_statement()