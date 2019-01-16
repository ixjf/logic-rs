local class = require "middleclass"

Attributes = class "Attributes"

Attributes.static.Types = {
    Token = "Token"
}

function Attributes:initialize(...)
    self.attributes = {}

    local attributes = {...}
    
    if #attributes == 0 then
        error("no attributes specified", 2)
    end

    for _, vattr in ipairs(attributes) do
        assert(type(vattr) == "string", "invalid value type for arg 'vattr' specified")

        if not Attributes.static.Types[vattr] then
            error("invalid attribute '" .. vattr .. "' specified", 2)
        end
        
        self.attributes[vattr] = vattr
    end
end

function Attributes:all_attributes()
    return self.attributes
end

return Attributes