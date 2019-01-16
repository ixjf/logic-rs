-- This entire thing only works on non-left recursive, fully context-free grammars
-- and it'll be able to look ahead as many times as needed to get the most accurate
-- rule match

local class = require "middleclass"

Interpreter = class "Interpreter"

local Grammar = Grammar or require "Grammar"
local SequenceGroup = SequenceGroup or require "SequenceGroup"
local Char = Char or require "Char"
local Token = Token or require "Token"
local Rule = Rule or require "Rule"
local InputStream = InputStream or require "InputStream"
local TokenStream = TokenStream or require "TokenStream"

function Interpreter:initialize(grammar)
    if not Grammar.isInstanceOf(grammar, Grammar) then
        error("invalid value type for arg 'input'", 2)
    end

    self.grammar = grammar
end

function Interpreter:run(input, start_rule_name)
    if type(input) ~= "string" then
        error("invalid value type for arg 'input'", 2)
    end

    local start_rule = self.grammar:all_rules()[start_rule_name]

    if start_rule == nil then
        error("start rule specified does not exist", 2)
    end

    local res = self:lexer(input)

    if res == false then
        return false -- lexer failed, no token tree, needs more error info
    end

    local tok_stream = TokenStream(res)

    local res, new_tok_stream = self:parser_match_rule(tok_stream, start_rule:all_elements())

    if res == true then
        tok_stream = new_tok_stream
    end

    return res
    -- TODO: return parse tree
end

function Interpreter:parser_match_rule(tok_stream, seq_group)
    -- TODO: We need to keep track of line/column for error messages
    -- TODO: ERROR MESSAGES!
    assert(TokenStream.isInstanceOf(tok_stream, TokenStream))
    assert(SequenceGroup.isInstanceOf(seq_group, SequenceGroup))

    -- TODO: This isn't going to work if two rules begin with the same sequence
    -- E.g. rule1 = rule2 rule3
    -- rule4 = rule2
    -- An input of 'rule2' is going to match rule4 and stop, even though there's still input
    -- What we need to do here is make sure the ENTIRE input matches, so we probably
    -- need to turn this around (instead of iterating over rules, iterate over the characters)

    -- Nope, not gonna work: we're told here to check if input matches this rule, we can't
    -- do anything about any other rule.
    -- What we CAN do though is, again, what I already wanted to do before: we can try all
    -- alternatives and check which matches _best_
    
    -- But that _still_ doesn't solve the problem of having garbage after the rule
    -- It may well be that the rule matches but everything that's after it is nonsense
    -- What has been said above does not correct this
    -- I mean, I could just do if utf8.len(input) > input_position at the end, and if it is
    -- then fail, but errors would be absolutely awful

    -- Don't forget we can always add attributes to grammar. So if the grammar can aid us...

    local matched = false

    local new_tok_stream = tok_stream:clone()

    for _, element in ipairs(seq_group:all_elements()) do
        if type(element) == "string" then
            local element_rule = self.grammar:all_rules()[element]
            assert(type(element_rule) ~= "nil", "non-existing rule '" .. element .. "' was referenced in another rule")

            if element_rule:all_attributes()["Token"] then
                if element ~= new_tok_stream:curr():of_rule() then
                    matched = false -- this needs to be here, because it may be that the previous loop iteration set this to true
                    break
                else
                    matched = true
                    new_tok_stream:advance()
                end
            else
                local res, _new_tok_stream = self:parser_match_rule(new_tok_stream, element_rule:all_elements())

                if res == false then
                    matched = false
                    break
                end

                matched = true
                new_tok_stream = _new_tok_stream
            end
        elseif Alternatives.isInstanceOf(element, Alternatives) then
            for _, alt in ipairs(element:alternatives()) do
                assert(SequenceGroup.isInstanceOf(alt, SequenceGroup))

                local res, _new_tok_stream = self:parser_match_rule(new_tok_stream, alt)

                if res == true then
                    matched = true
                    new_tok_stream = _new_tok_stream
                    break
                else
                    matched = false -- it could have previously been true
                end
            end

            if matched == false then
                break
            end
        else
            assert(false)
        end
    end

    if matched == true then -- if we finished and matched is true, then the rule matches
        return true, new_tok_stream
    else
        return false
    end
end

--  TODO: Optionals & Repeat! both in lexer and parser (can be part of any rule)
function Interpreter:lexer(input)
    local token_tree = {}

    -- Task list:
    -- Once all bugs are fixed, I go back to what I was doing: error handling, and then fixing the problem mentioned in parser_match_rule
    -- TODO: Doesn't the lexer need a best match too? They're still rules after all

    -- WHAT ARE WE DOING RIGHT NOW?
    -- Separation of responsabilities
    -- We need an InputStream and a TokenStream, so we can keep track of line/column
    -- so that we can do some simple error reporting on lexical errors
    -- then we'll see what we can do about further error reporting
    -- i.e. see above, or in parser_match_rule, or notepad file

    -- For every UTF8 character
    local inp_stream = InputStream(input)

    while not inp_stream:eof() do
        local matched = false

        -- For every rule
        for rule_name, rule in pairs(self.grammar:all_rules()) do
            -- ...that is a token
            if rule:all_attributes()["Token"] then
                -- Check if the sequence starting at lexer_state.position matches the rule
                local success, token, new_inp_stream = self:lexer_match_rule(inp_stream, rule_name, rule:all_elements())

                if success == true then
                    table.insert(token_tree, token)
                    matched = true
                    inp_stream = new_inp_stream
                    break
                end
            end
        end

        -- No token matched this character/sequence of characters
        if matched == false then
            return false -- TODO: error messages
        end
    end

    return token_tree
end

function Interpreter:lexer_match_rule(inp_stream, rule_name, seq_group)
    local inp_stream = inp_stream:clone()

    local matched = false

    -- Check every element in its sequence group
    for _, elem in ipairs(seq_group:all_elements()) do
        local c = inp_stream:curr()

        -- If it's a char, check if it matches the input character. If it doesn't, then this isn't the token
        if Char.isInstanceOf(elem, Char) then
            local res = elem:try_match(c)

            if res == false then
                break
            end

            matched = true
            inp_stream:advance()
        -- If it's a range, check if it matches the input character. If it doesn't, then this isn't the token
        elseif Range.isInstanceOf(elem, Range) then
            local res = elem:try_match(c)

            if res == false then
                break
            end

            matched = true
            inp_stream:advance()
        -- If it's an alternative, check if any alternative matches the input character. If it doesn't, then this isn't the token
        elseif Alternatives.isInstanceOf(elem, Alternatives) then
            for _, alt in ipairs(elem:alternatives()) do
                local success, token, new_inp_stream = self:lexer_match_rule(inp_stream, rule_name, alt)

                if success == true then
                    matched = true
                    inp_stream = new_inp_stream
                    break
                end
            end
        else
            assert(false) -- oops, invalid rule!!
        end
    end

    if matched == true then
        return true, Token(rule_name), inp_stream
    else
        return false
    end
end

return Interpreter