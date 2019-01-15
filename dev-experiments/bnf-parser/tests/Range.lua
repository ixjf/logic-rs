local Range = require "../Range"

function matches_character_ranges()
    local subscript = Range(utf8.char(0x2080), utf8.char(0x2089))
    assert(subscript:try_match(utf8.char(0x2083)), "utf8 character not matched")
    assert(not subscript:try_match(utf8.char(0)), "out of range utf8 character matched")

    local alphabetic = Range('A', 'Z')
    assert(alphabetic:try_match('C'), "alphabetic character not mached")
    assert(not alphabetic:try_match(' '), "out of range character matched")
end

function matches_numeric_ranges()
    local digit = Range(0, 9)
    assert(digit:try_match('8'), "digit not matched")
    assert(not digit:try_match('a'), "out of range character matched digit range")
end

matches_character_ranges()
matches_numeric_ranges()