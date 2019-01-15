local class = require "middleclass"

Interpreter = class "Interpreter"

local Grammar = Grammar or require "Grammar"
--local Range = require "Range"
--local Alternatives = require "Alternatives"
--local Optional = require "Optional"
--local Repeat = require "Repeat"
local SequenceGroup = SequenceGroup or require "SequenceGroup"
local Char = Char or require "Char"

function Interpreter:initialize(grammar)
    if not Grammar.isInstanceOf(grammar, Grammar) then
        error("invalid value type for arg 'input'", 2)
    end

    self.grammar = grammar
end

function Interpreter:parse(input, start_rule_name)
    if type(input) ~= "string" then
        error("invalid value type for arg 'input'", 2)
    end

    local start_rule = self.grammar:all_rules()[start_rule_name]

    if start_rule == nil then
        error("start rule specified does not exist", 2)
    end

    local parse_state = {
        input = input,
        input_position = 1
    }

    return self:parse_sequence_group(start_rule, parse_state)
    -- return parse tree
end

function Interpreter:parse_sequence_group(sequence_group, parse_state)
    -- TODO: ERROR MESSAGES!
    assert(SequenceGroup.isInstanceOf(sequence_group, SequenceGroup))
    assert(type(parse_state) == "table")
    assert(type(parse_state.input) == "string")
    assert(type(parse_state.input_position) == "number")

    -- TODO: This isn't going to work if two rules begin with the same sequence
    -- E.g. rule1 = rule2 rule3
    -- rule4 = rule2
    -- An input of 'rule2' is going to match rule4 and stop, even though there's still input
    -- What we need to do here is make sure the ENTIRE input matches, so we probably
    -- need to turn this around (instead of iterating over rules, iterate over the characters)

    -- Nope, not gonna work: we're told here to check if input matches this rule, we can't
    -- do anything about any other rule.
    -- What we CAN do though is, again, what I already wanted to do before: we can try all
    -- rules and check which matches _best_

    local matched_rule = false

    local new_parse_state = {
        input = parse_state.input,
        input_position = parse_state.input_position
    }

    for i, element in ipairs(sequence_group:all_elements()) do
        if type(element) == "string" then
            local seq_group = self.grammar:all_rules()[element]
            assert(type(seq_group) ~= "nil")

            local res = self:parse_sequence_group(seq_group, new_parse_state)

            if res == false then
                matched_rule = false
                break
            end

            matched_rule = true
        elseif Alternatives.isInstanceOf(element, Alternatives) then
            for _, alt in ipairs(element:alternatives()) do
                assert(SequenceGroup.isInstanceOf(alt, SequenceGroup))

                local res = self:parse_sequence_group(alt, new_parse_state)

                if res == true then
                    matched_rule = true
                    break
                end
            end

            if matched_rule == false then
                break
            end
        else
            local curr_input_char = utf8.char(utf8.codepoint(input, utf8.offset(new_parse_state.input, new_parse_state.input_position)))

            -- We've reached the end of the input stream prematurely
            if type(curr_input_char) == "nil" then
                return false -- TODO!
            end

            if Char.isInstanceOf(element, Char) then
                local res = element:try_match(curr_input_char)

                if res == false then
                    matched_rule = false
                    break
                end

                new_parse_state.input_position = new_parse_state.input_position + 1
                matched_rule = true
            elseif Range.isInstanceOf(element, Range) then
                local res = element:try_match(curr_input_char)

                if res == false then
                    matched_rule = false
                    break
                end

                new_parse_state.input_position = new_parse_state.input_position + 1
                matched_rule = true
            --[[elseif element:isInstanceOf(Repeat) then
                local count = 0
                local min = element:min()
                local max = element:max()
                local repeat_element = element:element()]]
            -- TODO: Optionals & repeat
            else
                assert(false)
            end
        end
    end

    if matched_rule == true then -- if we finished and matched_rule is true, then the alternative matches and we don't need to look at any others
        parse_state.input_position = new_parse_state.input_position
        return true
    end
    -- else continue on

    return false
end

return Interpreter