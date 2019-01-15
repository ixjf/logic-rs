local Char = require "../Char"

function matches_character()
    local char1 = Char('B')
    assert(char1:try_match('B'))
    assert(not char1:try_match('D'))

    local char2 = Char(utf8.char(0x2083))
    assert(char2:try_match(utf8.char(0x2083)))
    assert(not char2:try_match('D'))
end

matches_character()