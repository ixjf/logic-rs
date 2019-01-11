use std::cmp;

pub struct SourceReader {
    char_iter: Vec<char>,
    curr_idx: Option<usize>,
    line: u32,
    col: u32
}

impl SourceReader {
    pub fn new(s: &str) -> Self {
        SourceReader { char_iter: s.chars().collect(), curr_idx: None, line: 1, col: 0 }
    }

    pub fn current_line(&self) -> u32 {
        self.line
    }

    pub fn current_column(&self) -> u32 {
        self.col
    }

    // Reads the next character, returns Some(c)
    // If we've reached the end of the source, returns None
    // Counter stays at value 'len' after reaching the end,
    // even if next() is called multiple times
    // Also keeps track of lines and columns as follows:
    // If haven't started reading or it's a new line, then line is x and column is 0
    // So:
    // abc
    // def
    //0123
    // 0, 1, 2, and 3 are the columns
    pub fn next(&mut self) -> Option<char> {
        match self.curr_idx {
            Some(i) => if i < self.char_iter.len() {
                self.curr_idx = Some(i + 1)
            },
            None => self.curr_idx = Some(0)
        }

        match self.char_iter.get(self.curr_idx.unwrap()).map(|x| *x) {
            Some(c) => {
                if c == '\n' {
                    self.line += 1;
                    self.col = 0;
                }
                else {
                    self.col += 1;
                }
                Some(c)
            }
            None => None
        }
    }

    // Skips n characters
    pub fn skip(&mut self, n: usize) {
        for _ in 0..n {
            self.next();
        }
    }

    // Reads the character that is after the current one. iI there's none, returns None
    pub fn peek_forward(&self) -> Option<char> {
        self.multipeek_forward(1).and_then(|x| x.chars().nth(0))
    }

    // Returns a string with all the characters requested, in order
    // If the source is smaller than the no. requested, returns all the characters possible
    // If there are no characters to read, returns None
    pub fn multipeek_forward(&self, len: usize) -> Option<String> {
        assert!(len > 0, "len must be higher than 0");

        let idx_start;

        match self.curr_idx {
            Some(i) => idx_start = i + 1,
            None => idx_start = 0
        }

        if idx_start > self.char_iter.len() {
            return None;
        }

        let idx_end = cmp::min(idx_start + (len - 1) + 1, self.char_iter.len()); // +1 because end is exclusive

        Some(self.char_iter[idx_start..idx_end].into_iter().collect::<String>())
    }

    // Returns the character that comes before the current one
    // If there's none, returns None
    // If we've reached the end of the source, returns the last character
    pub fn peek_back(&self) -> Option<char> {
        match self.curr_idx {
            Some(i) => {
                if i == 0 { return None };

                self.char_iter.get(i - 1).map(|x| *x)
            },
            None => return None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    static INPUT: &str = "$ % !)#= ASJISJ";
    static INPUT2:&str = "$\n!)#=\nASJISJ";

    #[test]
    fn can_read_characters() {
        let mut src_reader = SourceReader::new(INPUT);

        assert_eq!(src_reader.next(), Some('$'));
        assert_eq!(src_reader.next(), Some(' '));
        assert_eq!(src_reader.next(), Some('%'));
        assert_eq!(src_reader.next(), Some(' '));
        assert_eq!(src_reader.next(), Some('!'));
        assert_eq!(src_reader.next(), Some(')'));
        assert_eq!(src_reader.next(), Some('#'));
        assert_eq!(src_reader.next(), Some('='));
        assert_eq!(src_reader.next(), Some(' '));
        assert_eq!(src_reader.next(), Some('A'));
        assert_eq!(src_reader.next(), Some('S'));
        assert_eq!(src_reader.next(), Some('J'));
        assert_eq!(src_reader.next(), Some('I'));
        assert_eq!(src_reader.next(), Some('S'));
        assert_eq!(src_reader.next(), Some('J'));
        assert_eq!(src_reader.next(), None);
    }

    #[test]
    fn can_peek_forward() {
        let mut src_reader = SourceReader::new(INPUT);

        assert_eq!(src_reader.peek_forward(), Some('$'));
        src_reader.next();
        assert_eq!(src_reader.peek_forward(), Some(' '));
        src_reader.next();
        assert_eq!(src_reader.peek_forward(), Some('%'));
        src_reader.next();
        assert_eq!(src_reader.peek_forward(), Some(' '));
        src_reader.next();
        assert_eq!(src_reader.peek_forward(), Some('!'));
        src_reader.next();
        assert_eq!(src_reader.peek_forward(), Some(')'));
        src_reader.next();
        assert_eq!(src_reader.peek_forward(), Some('#'));
        src_reader.next();
        assert_eq!(src_reader.peek_forward(), Some('='));
        src_reader.next();
        assert_eq!(src_reader.peek_forward(), Some(' '));
        src_reader.next();
        assert_eq!(src_reader.peek_forward(), Some('A'));
        src_reader.next();
        assert_eq!(src_reader.peek_forward(), Some('S'));
        src_reader.next();
        assert_eq!(src_reader.peek_forward(), Some('J'));
        src_reader.next();
        assert_eq!(src_reader.peek_forward(), Some('I'));
        src_reader.next();
        assert_eq!(src_reader.peek_forward(), Some('S'));
        src_reader.next();
        assert_eq!(src_reader.peek_forward(), Some('J'));
        src_reader.next();
        assert_eq!(src_reader.peek_forward(), None);
    }

    #[test]
    fn can_multipeek_forward() {
        let src_reader = SourceReader::new(INPUT);

        assert_eq!(src_reader.multipeek_forward(15), Some("$ % !)#= ASJISJ".to_owned()));
    }

    #[test]
    fn can_peek_back() {
        let mut src_reader = SourceReader::new(INPUT);

        src_reader.next();
        assert_eq!(src_reader.peek_back(), None);
        src_reader.next();
        assert_eq!(src_reader.peek_back(), Some('$'));
        src_reader.next();
        assert_eq!(src_reader.peek_back(), Some(' '));
        src_reader.next();
        assert_eq!(src_reader.peek_back(), Some('%'));
        src_reader.next();
        assert_eq!(src_reader.peek_back(), Some(' '));
        src_reader.next();
        assert_eq!(src_reader.peek_back(), Some('!'));
        src_reader.next();
        assert_eq!(src_reader.peek_back(), Some(')'));
        src_reader.next();
        assert_eq!(src_reader.peek_back(), Some('#'));
        src_reader.next();
        assert_eq!(src_reader.peek_back(), Some('='));
        src_reader.next();
        assert_eq!(src_reader.peek_back(), Some(' '));
        src_reader.next();
        assert_eq!(src_reader.peek_back(), Some('A'));
        src_reader.next();
        assert_eq!(src_reader.peek_back(), Some('S'));
        src_reader.next();
        assert_eq!(src_reader.peek_back(), Some('J'));
        src_reader.next();
        assert_eq!(src_reader.peek_back(), Some('I'));
        src_reader.next();
        assert_eq!(src_reader.peek_back(), Some('S'));
        src_reader.next();
        assert_eq!(src_reader.peek_back(), Some('J'));
        src_reader.next();
        assert_eq!(src_reader.peek_back(), Some('J'));
    }

    #[test]
    fn can_skip_characters() {
        let mut src_reader = SourceReader::new(INPUT);

        src_reader.skip(2);
        assert_eq!(src_reader.peek_forward(), Some('%'));
        src_reader.skip(2);
        assert_eq!(src_reader.peek_forward(), Some('!'));
        src_reader.skip(2);
        assert_eq!(src_reader.peek_forward(), Some('#'));
        src_reader.skip(2);
        assert_eq!(src_reader.peek_forward(), Some(' '));
        src_reader.skip(2);
        assert_eq!(src_reader.peek_forward(), Some('S'));
        src_reader.skip(2);
        assert_eq!(src_reader.peek_forward(), Some('I'));
        src_reader.skip(2);
        assert_eq!(src_reader.peek_forward(), Some('J'));
        src_reader.skip(2);
        assert_eq!(src_reader.peek_forward(), None);
    }

    #[test]
    fn can_keep_track_of_line_count() {
        let mut src_reader = SourceReader::new(INPUT2);

        assert_eq!(src_reader.current_line(), 1);

        src_reader.skip(2);
        assert_eq!(src_reader.current_line(), 2);

        src_reader.skip(5);
        assert_eq!(src_reader.current_line(), 3);
    }

    #[test]
    fn can_keep_track_of_column() {
        let mut src_reader = SourceReader::new(INPUT2);

        assert_eq!(src_reader.current_column(), 0);

        src_reader.skip(1);
        assert_eq!(src_reader.current_column(), 1);
        src_reader.skip(1);
        assert_eq!(src_reader.current_column(), 0);
        src_reader.skip(1);
        assert_eq!(src_reader.current_column(), 1);
        src_reader.skip(1);
        assert_eq!(src_reader.current_column(), 2);
        src_reader.skip(1);
        assert_eq!(src_reader.current_column(), 3);
        src_reader.skip(1);
        assert_eq!(src_reader.current_column(), 4);
        src_reader.skip(1);
        assert_eq!(src_reader.current_column(), 0);
        src_reader.skip(1);
        assert_eq!(src_reader.current_column(), 1);
        src_reader.skip(1);
        assert_eq!(src_reader.current_column(), 2);
        src_reader.skip(1);
        assert_eq!(src_reader.current_column(), 3);
        src_reader.skip(1);
        assert_eq!(src_reader.current_column(), 4);
        src_reader.skip(1);
        assert_eq!(src_reader.current_column(), 5);
        src_reader.skip(1);
        assert_eq!(src_reader.current_column(), 6);
        src_reader.skip(1);
        assert_eq!(src_reader.current_column(), 6);
    }
}