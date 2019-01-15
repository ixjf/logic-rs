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
    assert(Alternatives.isInstanceOf(added_rule:all_elements()[1], Alternatives), "rule's only element is not an Alternatives")
    
    local added_rule_alternatives = added_rule:all_elements()[1]:alternatives()
    assert(type(added_rule_alternatives) == "table", "Alternatives alternatives() doesn't return a table")
    assert(#added_rule_alternatives == 2, "rule doesn't have two alternatives")
    
    local added_rule_alternative_1 = added_rule_alternatives[1]
    local added_rule_alternative_2 = added_rule_alternatives[2]

    assert(SequenceGroup.isInstanceOf(added_rule_alternative_1, SequenceGroup), "alternative 1 is not a SequenceGroup")
    assert(SequenceGroup.isInstanceOf(added_rule_alternative_2, SequenceGroup), "alternative 2 is not a SequenceGroup")

    assert(added_rule_alternative_1:all_elements()[1] == "moo", "alternative 1 isn't 'moo'")
    assert(added_rule_alternative_2:all_elements()[1] == "bar", "alternative 2 isn't 'bar'")
end

function add_alternative_to_rule_adds_to_existing_alternatives()
    local g = Grammar()
        :add_rule("foo", SequenceGroup("moo"))
        :add_alternative_to_rule("foo", SequenceGroup("bar"))
        :add_alternative_to_rule("foo", SequenceGroup("zoo"))
        :add_alternative_to_rule("foo", SequenceGroup("boo"))

    local added_rule = g:all_rules()["foo"]

    assert(#added_rule:all_elements() == 1, "rule should have only one child - an Alternatives - but it doesn't")
    assert(Alternatives.isInstanceOf(added_rule:all_elements()[1], Alternatives), "only child of rule isn't an Alternatives")

    local alternatives = added_rule:all_elements()[1]
    assert(#alternatives:alternatives() == 4, "rule was specified with 4 alternatives but doesn't have that amount")
    assert(alternatives:alternatives()[1]:all_elements()[1] == "moo", "first alternative is not 'moo'")
    assert(alternatives:alternatives()[2]:all_elements()[1] == "bar", "first alternative is not 'bar'")
    assert(alternatives:alternatives()[3]:all_elements()[1] == "zoo", "first alternative is not 'zoo'")
    assert(alternatives:alternatives()[4]:all_elements()[1] == "boo", "first alternative is not 'boo'")
end

all_rules_returns_a_table_of_rules()
rules_are_added()
add_alternative_to_rule_works()
add_alternative_to_rule_adds_to_existing_alternatives()