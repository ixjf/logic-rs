local class = require "middleclass"

Token = class "Token"

function Token:initialize(rule_name)
    assert(type(rule_name) == "string", "invalid value type for arg 'rule_name'")
    self.rule_name = rule_name
end

function Token:of_rule()
    return self.rule_name
end

return Token