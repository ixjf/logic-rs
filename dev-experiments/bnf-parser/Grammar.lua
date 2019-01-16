local class = require "middleclass"

Grammar = class "Grammar"

local SequenceGroup = SequenceGroup or require "SequenceGroup"
local Alternatives = Alternatives or require "Alternatives"
local Range = Range or require "Range"
local Rule = Rule or require "Rule"

-- TODO: Grammar needs to check if names input to SequenceGroup, Repeat are valid
-- rules or tokens
-- But that should probably be deferred until later, since we can't know when we'll be done
-- adding new grammar
-- Should the Interpreter deal with that?
-- Also, needs to make sure there is at least one token in the grammar
-- Also need to make sure sequence groups of rules without token attributes don't have
-- range, char, etc. which are token-only elements
-- And tokens don't have repeats!
-- Rules should be case insensitive!

function Grammar:initialize()
    self.rules = {}
end

function Grammar:add_rule(name, elements, attributes)
    if self.rules[name] ~= nil then
        error("a rule with name '" .. name .. "' already exists", 2)
    end

    if not SequenceGroup.isInstanceOf(elements, SequenceGroup) then
        error("invalid value type specified for arg 'elements'", 2)
    end

    if attributes ~= nil and not Attributes.isInstanceOf(attributes, Attributes) then
        error("invalid value type specified for arg 'attributes'", 2)
    end

    self.rules[name] = Rule(elements, attributes)

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

    local rule_seq_group = rule:all_elements()

    if #rule_seq_group:all_elements() == 1 and Alternatives.isInstanceOf(rule_seq_group:all_elements()[1], Alternatives) then
        rule_seq_group:all_elements()[1]:add(elements)
    else
        self.rules[name] = Rule(SequenceGroup(Alternatives(rule_seq_group, elements)), 
                                #rule:all_attributes() > 0 and Attributes(table.unpack(rule:all_attributes())) or nil)
    end

    return self
end

function Grammar:all_rules()
    return self.rules
end

return Grammar