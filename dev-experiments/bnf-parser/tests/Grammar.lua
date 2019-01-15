local Grammar = require "../Grammar"
local SequenceGroup = SequenceGroup or require "../SequenceGroup"
local Alternatives = Alternatives or require "../Alternatives"

function all_rules_returns_a_table_of_rules()
    local g = Grammar()

    assert(type(g:all_rules()) == "table", "all_rules doesn't return a table")
    assert(#g:all_rules() == 0, "rule table not empty")
end

function rules_are_added()
    local g = Grammar():add_rule("foo", SequenceGroup("moo"))

    assert(g:all_rules()["foo"], "rule not added")
end

function add_alternative_to_rule_works()
    local g = Grammar()
    g:add_rule("foo", SequenceGroup("moo"))
    g:add_alternative_to_rule("foo", SequenceGroup("bar"))

    local added_rule = g:all_rules()["foo"]
    assert(type(added_rule:all_elements()) == "table" and #added_rule:all_elements() == 1, "rule has no elements")
    assert(type(added_rule:all_elements()[1].isInstanceOf) == "function" and added_rule:all_elements()[1]:isInstanceOf(Alternatives), "rule's only element is not an Alternatives")
    
    local added_rule_alternatives = added_rule:all_elements()[1]:alternatives()
    assert(type(added_rule_alternatives) == "table", "Alternatives alternatives() doesn't return a table")
    assert(#added_rule_alternatives == 2, "rule doesn't have two alternatives")
    
    local added_rule_alternative_1 = added_rule_alternatives[1]
    local added_rule_alternative_2 = added_rule_alternatives[2]

    assert(type(added_rule_alternative_1.isInstanceOf) == "function" and added_rule_alternative_1:isInstanceOf(SequenceGroup), "alternative is not a SequenceGroup")
    assert(type(added_rule_alternative_2.isInstanceOf) == "function" and added_rule_alternative_2:isInstanceOf(SequenceGroup), "alternative is not a SequenceGroup")   

    assert(added_rule_alternative_1:all_elements()[1] == "moo", "alternative 1 isn't 'moo'")
    assert(added_rule_alternative_2:all_elements()[1] == "bar", "alternative 2 isn't 'bar'")
end

all_rules_returns_a_table_of_rules()
rules_are_added()
add_alternative_to_rule_works()