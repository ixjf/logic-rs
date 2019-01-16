-- This entire thing only works on non-left recursive, fully context-free grammars
-- and it is able to look ahead as many times as needed to get the most accurate
-- rule match, i.e. it will accept the rule that matches the highest no. of tokens
-- It does this in both the lexer phase, from input to token, and in the parser phase,
-- from token tree to parse rule

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

    local res, new_tok_stream, no_tokens_matched = self:parser_match_rule(tok_stream, start_rule:all_elements())

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
    
    -- TODO: I have implemented the best match algorithm, however:
    -- that _still_ doesn't solve the problem of having garbage after the rule
    -- It may well be that the rule matches but everything that's after it is nonsense
    -- What has been said above does not correct this
    -- I mean, I could just do if utf8.len(input) > input_position at the end, and if it is
    -- then fail, but errors would be absolutely awful

    -- Don't forget we can always add attributes to grammar. So if the grammar can aid us...

    local matched = false

    local new_tok_stream = tok_stream:clone()

    local no_tokens_matched = 0

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
                    no_tokens_matched = no_tokens_matched + 1
                end
            else
                local res, _new_tok_stream, _no_tokens_matched = self:parser_match_rule(new_tok_stream, element_rule:all_elements())

                if res == false then
                    matched = false
                    break
                end

                matched = true
                new_tok_stream = _new_tok_stream
                no_tokens_matched = no_tokens_matched + _no_tokens_matched
            end
        elseif Alternatives.isInstanceOf(element, Alternatives) then
            -- Implements a best match algorithm
            -- Tries every alternative, accepts the one that matches the most tokens
            -- (but that matches fully)
            local matches = {}

            for _, alt in ipairs(element:alternatives()) do
                assert(SequenceGroup.isInstanceOf(alt, SequenceGroup))

                local res, _new_tok_stream, _no_tokens_matched = self:parser_match_rule(new_tok_stream, alt)

                if res == true then
                    -- TODO: What if multiple rules match the same no. of tokens?

                    table.insert(matches, {no_tokens_matched = _no_tokens_matched, tok_stream = _new_tok_stream})
                end
            end

            if #matches == 0 then
                matched = false
                break
            end

            table.sort(matches, function(a, b) return a.no_tokens_matched > b.no_tokens_matched end)

            matched = true

            new_tok_stream = matches[1].tok_stream
        else
            assert(false)
        end
    end

    if matched == true then -- if we finished and matched is true, then the rule matches
        return true, new_tok_stream, no_tokens_matched
    else
        return false, nil, no_tokens_matched
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
                -- Check if the sequence starting at the current input stream position matches the rule
                local success, token, new_inp_stream, no_chars_matched = self:lexer_match_rule(inp_stream, rule_name, rule:all_elements())

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

    local no_chars_matched = 0

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
            no_chars_matched = no_chars_matched + 1
        -- If it's a range, check if it matches the input character. If it doesn't, then this isn't the token
        elseif Range.isInstanceOf(elem, Range) then
            local res = elem:try_match(c)

            if res == false then
                break
            end

            matched = true
            inp_stream:advance()
            no_chars_matched = no_chars_matched + 1
        -- If it's an alternative, check if any alternative matches the input character. If it doesn't, then this isn't the token
        elseif Alternatives.isInstanceOf(elem, Alternatives) then
            local matches = {}

            for _, alt in ipairs(elem:alternatives()) do
                local success, token, new_inp_stream, _no_chars_matched = self:lexer_match_rule(inp_stream, rule_name, alt)

                if success == true then
                    table.insert(matches, {inp_stream = new_inp_stream, no_chars_matched = _no_chars_matched})
                end
            end

            if #matches > 0 then
                matched = true
                
                table.sort(matches, function(a, b) return a.no_chars_matched > b.no_chars_matched end)

                inp_stream = matches[1].inp_stream
            else
                matched = false
            end
        else
            assert(false) -- oops, invalid rule!!
        end
    end

    if matched == true then
        return true, Token(rule_name), inp_stream, no_chars_matched
    else
        return false
    end
end

return Interpreter