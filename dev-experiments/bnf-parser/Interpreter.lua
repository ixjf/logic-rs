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
    if type(grammar.isInstanceOf) ~= "function" or not grammar:isInstanceOf(Grammar) then
        error("invalid value type for arg 'input'", 2)
    end

    self.grammar = grammar
end

function Interpreter:parse(input, start_rule_name)
    if type(input) ~= "string" then
        error("invalid value type for arg 'input'", 2)
    end

    if self.grammar:all_rules()[start_rule_name] == nil then
        error("start rule specified does not exist", 2)
    end

    local parse_state = {
        input_position = 1
    }

    return self:parse_rule(input, start_rule_name, parse_state)

    -- TODO: This alternatives code cannot be here
    -- in parse_rule we try to parse rules that may have alternatives in them...
    --[[for _, alternative in ipairs(self.grammar:alternatives(start_rule_name)) do
        success = self:parse_rule(input, alternative, parse_state)
    
        if success == true then
            break
        end
    end

    return success]]
    -- return parse tree
end

function Interpreter:parse_rule(input, rule_name, parse_state)
    -- TODO: ERROR MESSAGES!
    assert(type(input) == "string")
    assert(type(self.grammar:all_rules()[rule_name].isInstanceOf) == "function" and self.grammar:all_rules()[rule_name]:isInstanceOf(SequenceGroup))
    assert(type(parse_state) == "table")

    -- TODO: This isn't going to work if two rules begin with the same sequence
    -- E.g. rule1 = rule2 rule3
    -- rule4 = rule2
    -- An input of 'rule2' is going to match rule4 and stop, even though there's still input
    -- What we need to do here is make sure the ENTIRE input matches, so we probably
    -- need to turn this around (instead of iterating over rules, iterate over the characters)
    for _, alternative in ipairs(self.grammar:alternatives(rule_name)) do
        local matched_rule = false

        local new_parse_state = {
            input_position = parse_state.input_position
        }

        for i, element in ipairs(alternative:all_elements()) do
            if type(element) == "string" then
                local res = self:parse_rule(input, element, new_parse_state)

                if res == false then
                    matched_rule = false
                    break
                end

                matched_rule = true
            elseif type(element.isInstanceOf) == "function" then
                local curr_input_char = utf8.char(utf8.codepoint(input, utf8.offset(input, new_parse_state.input_position)))

                -- We've reached the end of the input stream prematurely
                if type(curr_input_char) == "nil" then
                    return false -- TODO!
                end

                if element:isInstanceOf(Char) then
                    local res = element:try_match(curr_input_char)

                    if res == false then
                        matched_rule = false
                        break
                    end

                    new_parse_state.input_position = new_parse_state.input_position + 1
                    matched_rule = true
                elseif element:isInstanceOf(Range) then
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
    end

    return false
end

return Interpreter