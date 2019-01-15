-- So a rule has:
-- > CHECK Name
-- > CHECK Elements

-- Elements can be either:
-- > CHECK Terminal values (characters) - tokens
-- > CHECK Non-terminal values (other rules) - other rules

-- Terminal values denote characters, but these can be made to:
-- > CHECK Specify characters (either Unicode or ASCII)
-- > CHECK Numbers
-- > CHECK You can use ranges, or specify multiple numbers that could match the terminal
-- > CHECK (Why should it be case insensitive?) Specify a string, which is just a set of characters, but which is case-insensitive

-- CHECK Non-terminal values simply refer to another rule (by its name in Lua)

-- CHECK Elements can be concatenated
-- CHECK You can specify alternative elements (/)
-- CHECK You can specify incremental alternatives (=/) - do I need this? the only reason I might use this is for readability/separating rulesets
-- CHECK You can specify sequences in parentheses
-- CHECK Elements can be repeated, and you can specify a minimum or maximum amount of times (0 min, no max default)
-- CHECK Elements can be made optional


-- 1. CHECK Interpreter takes in UTF8/ASCII input
-- 2. CHECK Interpreter takes in a grammar
-- 3. Any of these panic! if invalid data is input, unless the invalid data is the user input obv

local g = Grammar.new()
    .add_rule("my_token_1", SequenceGroup.new(Range.new(20, 30)))
    .add_rule("my_token_3", SequenceGroup.new(Range.new(utf8.char(0x2080), utf8.char(0x2089))))
    .add_rule("my_token_2", SequenceGroup.new(Char.new(utf8.char(0x2080))))
    .add_rule("foo", SequenceGroup.new("my_token_1"))
    .add_rule("bar", SequenceGroup.new("my_token_3"))
    .add_rule("mumble", SequenceGroup.new("foo", "bar", "foo"))
    .add_rule("moo", SequenceGroup.new(Alternatives.new(SequenceGroup.new("foo"), SequenceGroup.new("bar"))))
    .add_rule("ff", SequenceGroup.new(Repeat.new("moo", 1, 3))) -- or Optional.new() which creates a Repeat that has minimum 0 times and maximum infinity times
    .add_alternative_to_rule("bar", SequenceGroup.new(token_name_2))

local i = Interpreter.new(input, g, "moo")
local res = i.parse_tree()

-- 1. Interpreter reads all tokens and rules
-- 2. Checks if no names are repeated & all names reference rules/tokens
-- 3. Reads the entire input and creates a token stream from it, or fails if input is lexically invalid
-- 4. Parser takes in token stream & start rule and then:
    -- 5. For each alternative of the rule, try to match tokens
    -- 6. If rule takes a rule in the position we're at, repeat from 5. onwards for this new rule. If it suceeds, go back to where we were and finish; if no match was possible, error out
    -- 7. If rule takes a repeat in the position we're at, check if repeat is matched
    -- 8. If rule takes a range in the position we're at, check if range fulfils token
    -- 8. If rule is a sequence group, check if all rules inside it match
    -- 10. If rule is a string, it could be a char as well
    -- 8. Succeed on the first full match

    -- Basically, you may read from a rule:
    -- 1. A rule
    -- 2. A char
    -- 3. A range
    -- 4. A repeat
    -- 5. TODO: An optional

-- Alternatives of a rule are determined by combining each possibility with every other alternative's possibilities
-- E.g. SequenceGroup.new(Alternatives.new(SequenceGroup.new("foo"), SequenceGroup.new("bar")))
--      Rule is: foo / bar
--      Alternatives: 1) foo 2) bar
-- But: SequenceGroup.new("foo", Alternatives.new(SequenceGroup.new("bar"), SequenceGroup.new("moo")))
--      Rule is: foo (bar / moo)
--      Alternatives: 1) foo bar 2) foo moo