local class = require "middleclass"

Grammar = class "Grammar"

local SequenceGroup = SequenceGroup or require "SequenceGroup"
local Alternatives = Alternatives or require "Alternatives"
local Range = Range or require "Range"

-- TODO: Grammar needs to check if names input to SequenceGroup, Repeat are valid
-- rules or tokens
-- But that should probably be deferred until later, since we can't know when we'll be done
-- adding new grammar
-- Should the Interpreter deal with that?

function Grammar:initialize()
    self.rules = {}
end

function Grammar:add_rule(name, elements)
    if self.rules[name] ~= nil then
        error("a rule with name '" .. name .. "' already exists", 2)
    end

    if not SequenceGroup.isInstanceOf(elements, SequenceGroup) then
        error("invalid value specified for rule", 2)
    end

    self.rules[name] = elements

    return self
end

function Grammar:add_alternative_to_rule(name, elements)
    local rule = self.rules[name]

    if rule == nil then
        error("no rule exists with name '" .. name .. "'", 2)
    end

    if not SequenceGroup.isInstanceOf(elements, SequenceGroup) then
        error("invalid value specified for alternative", 2)
    end

    if #rule:all_elements() == 1 and Alternatives.isInstanceOf(rule:all_elements()[1], Alternatives) then
        rule:all_elements()[1]:add(elements)
    else
        self.rules[name] = SequenceGroup(Alternatives(rule, elements))
    end

    return self
end

function Grammar:all_rules()
    return self.rules
end

return Grammar