-- This entire thing only works on non-left recursive, fully context-free grammars
-- and it'll be able to look ahead as many times as needed to get the most accurate
-- rule match

local class = require "middleclass"

Interpreter = class "Interpreter"

local Grammar = Grammar or require "Grammar"
--local Range = require "Range"
--local Alternatives = require "Alternatives"
--local Optional = require "Optional"
--local Repeat = require "Repeat"
local SequenceGroup = SequenceGroup or require "SequenceGroup"
local Char = Char or require "Char"
local Token = Token or require "Token"
local Rule = Rule or require "Rule"

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

    local parse_state = {
        token_stream = self:lexer(input),
        position = 1
    }

    if parse_state.token_stream == false then
        return false -- needs moar error messages
    end

    return self:parser_match_rule(start_rule:all_elements(), parse_state)
    -- TODO: return parse tree
end

function Interpreter:parser_match_rule(seq_group, parse_state)
    -- TODO: We need to keep track of line/column for error messages
    -- TODO: ERROR MESSAGES!
    assert(SequenceGroup.isInstanceOf(seq_group, SequenceGroup))
    assert(type(parse_state) == "table")
    assert(type(parse_state.token_stream) == "table")
    assert(type(parse_state.position) == "number")

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

    local new_parse_state = {
        token_stream = parse_state.token_stream,
        position = parse_state.position
    }

    for _, element in ipairs(seq_group:all_elements()) do
        if type(element) == "string" then
            local element_rule = self.grammar:all_rules()[element]
            assert(type(element_rule) ~= "nil", "non-existing rule '" .. element .. "' was referenced in another rule")

            if element_rule:all_attributes()["Token"] then
                if element ~= new_parse_state.token_stream[new_parse_state.position]:of_rule() then
                    matched = false -- this needs to be here, because it may be that the previous loop iteration set this to true
                    break
                else
                    matched = true
                    new_parse_state.position = new_parse_state.position + 1
                end
            else
                local res = self:parser_match_rule(element_rule:all_elements(), new_parse_state)

                if res == false then
                    matched = false
                    break
                end

                matched = true
            end
        elseif Alternatives.isInstanceOf(element, Alternatives) then
            for _, alt in ipairs(element:alternatives()) do
                assert(SequenceGroup.isInstanceOf(alt, SequenceGroup))

                local res = self:parser_match_rule(alt, new_parse_state)

                if res == true then
                    matched = true
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
        parse_state.position = new_parse_state.position
        return true
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

    -- For every UTF8 character
    local lexer_state = {
        input = input,
        position = 1
    }

    while lexer_state.position <= utf8.len(input) do
        local matched = false

        -- For every rule
        for rule_name, rule in pairs(self.grammar:all_rules()) do
            -- ...that is a token
            if rule:all_attributes()["Token"] then
                -- Check if the sequence starting at lexer_state.position matches the rule
                local success, token = self:lexer_match_rule(lexer_state, rule_name, rule:all_elements())

                if success == true then
                    table.insert(token_tree, token)
                    matched = true
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

function Interpreter:lexer_match_rule(lexer_state, rule_name, seq_group)
    local new_lexer_state = {
        input = lexer_state.input,
        position = lexer_state.position
    }

    local matched = false

    -- Check every element in its sequence group
    for _, elem in ipairs(seq_group:all_elements()) do
        local c = utf8.char(utf8.codepoint(new_lexer_state.input, utf8.offset(new_lexer_state.input, new_lexer_state.position)))

        -- If it's a char, check if it matches the input character. If it doesn't, then this isn't the token
        if Char.isInstanceOf(elem, Char) then
            local res = elem:try_match(c)

            if res == false then
                break
            end

            matched = true
            new_lexer_state.position = new_lexer_state.position + 1
        -- If it's a range, check if it matches the input character. If it doesn't, then this isn't the token
        elseif Range.isInstanceOf(elem, Range) then
            local res = elem:try_match(c)

            if res == false then
                break
            end

            matched = true
            new_lexer_state.position = new_lexer_state.position + 1
        -- If it's an alternative, check if any alternative matches the input character. If it doesn't, then this isn't the token
        elseif Alternatives.isInstanceOf(elem, Alternatives) then
            for _, alt in ipairs(elem:alternatives()) do
                local success, token = self:lexer_match_rule(rule_name, alt, new_lexer_state)

                if success == true then
                    matched = true
                    break
                end
            end
        else
            assert(false) -- oops, invalid rule!!
        end
    end

    if matched == true then
        lexer_state.position = new_lexer_state.position
        return true, Token(rule_name)
    else
        return false
    end
end

return Interpreter