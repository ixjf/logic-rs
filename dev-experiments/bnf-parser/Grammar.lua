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

    if (type(elements.isInstanceOf) ~= "function" or not elements:isInstanceOf(SequenceGroup)) then
        error("invalid value specified for rule", 2)
    end

    self.rules[name] = elements

    return self
end

function Grammar:add_alternative_to_rule(name, elements)
    if self.rules[name] == nil then
        error("no rule exists with name '" .. name .. "'", 2)
    end

    if (type(elements.isInstanceOf) ~= "function" or not elements:isInstanceOf(SequenceGroup)) then
        error("invalid value specified for alternative", 2)
    end

    -- TODO: If we call this function more than once, we'll get a nice bit of nested
    -- SequenceGroup->Alternatives :)
    self.rules[name] = SequenceGroup(Alternatives(self.rules[name], elements))

    return self
end

-- Will take a rule and grab all possible alternatives for it
-- If I have something of the sort:
-- .add_rule("moo", SequenceGroup.new(Alternatives.new(SequenceGroup.new("foo", "zoo"), SequenceGroup.new("bar"))))
-- I'll get:
-- Alternative 1: foo zoo
-- Alternative 2: bar
-- Basically, it'll get rid of the mandatory SequenceGroups inside Alternatives
-- And the initial SequenceGroup
function Grammar:alternatives(rule_name)
    local rule = self.rules[rule_name]

    if rule == nil then
        error("no rule exists with name '" .. rule_name .. "'", 2)
    end

    assert(type(rule.isInstanceOf) == "function" and rule:isInstanceOf(SequenceGroup), "(internal error) rule is invalid")

    local alternatives = {}

    for i, element in ipairs(rule:all_elements()) do
        --assert(type(element.isInstanceOf) == "function", "(internal error) rule has invalid elements")
        if type(element.isInstanceOf) == "function" and element:isInstanceOf(Alternatives) then
            for _, alternative in ipairs(element:alternatives()) do
                assert(type(alternative.isInstanceOf) == "function" and alternative:isInstanceOf(SequenceGroup))

                alternatives[#alternatives + 1] = SequenceGroup(table.unpack(rule:all_elements()))
                
                -- Table.insert but replace the first
                alternatives[#alternatives]:all_elements()[i] = alternative:all_elements()[1];
                for i2, v in ipairs(alternative:all_elements()) do
                    if i2 > 1 then
                        table.insert(alternatives[#alternatives]:all_elements(), i + i2 - 1, v)
                    end
                end
            end
        end
    end

    if #alternatives == 0 then
        alternatives = {rule}
    end

    return alternatives
end

function Grammar:all_rules()
    return self.rules
end

return Grammar