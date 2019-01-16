local class = require "middleclass"

Rule = class "Rule"

local SequenceGroup = SequenceGroup or require "SequenceGroup"
local Attributes = Attributes or require "Attributes"

function Rule:initialize(elements, attributes)
    assert(SequenceGroup.isInstanceOf(elements, SequenceGroup), "(internal error) rule elements not a SequenceGroup")
    assert(attributes == nil or Attributes.isInstanceOf(attributes, Attributes), "(internal error) rule attributes not an Attributes")

    self.elements = elements
    self.attributes = {}

    if attributes ~= nil then
        for k, v in pairs(attributes:all_attributes()) do
            self.attributes[k] = v
        end
    end
end

function Rule:all_elements()
    return self.elements
end

function Rule:all_attributes()
    return self.attributes
end

return Rule