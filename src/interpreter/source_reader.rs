use mystd::iter::streaming::*;

pub struct SourceReader<'a> {
    iter: StreamingIterator<'a, char>,
    line: u32,
    col: u32
}

impl<'a> SourceReader<'a> {
    pub fn new(source: &'a Vec<char>) -> Self {
        SourceReader { 
            iter: StreamingIterator::new(source), 
            line: 1, 
            col: 0 
        }
    }

    pub fn current_line(&self) -> u32 {
        self.line
    }

    pub fn current_column(&self) -> u32 {
        self.col
    }

    // Wraps 'next' so as to keep track of line count and column no.
    // Lines 1..
    // Columns 0.. where 0 means before the first character
    pub fn next(&mut self) -> Option<&char> {
        match self.iter.next() {
            Some(c) => {
                if *c == '\n' {
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

    // Overrides default implementation of skip because we need to keep track
    // of line count & column no.
    pub fn skip(&mut self, n: usize) {
        for _ in 0..n {
            self.next();
        }
    }

    pub fn peek(&self) -> Option<&char> {
        self.iter.clone().next()
    }

    pub fn peek_some(&self, n: usize) -> Option<String> {
        let mut v: Vec<&char> = Vec::new();

        let mut iter = self.iter.clone();

        for _ in 0..n {
            match iter.next() {
                Some(c) => v.push(c),
                None => {}
            }
        }

        if v.is_empty() {
            None
        }
        else {
            Some(v.into_iter().collect::<String>())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    static INPUT: [char; 13] = ['$', '\n', '!', ')', '#', '=', '\n', 'A', 'S', 'J', 'I', 'S', 'J'];

    #[test]
    fn can_keep_track_of_line_count() {
        let vec = INPUT.iter().cloned().collect();
        let mut src_reader = SourceReader::new(&vec);

        assert_eq!(src_reader.current_line(), 1);

        src_reader.skip(2);
        assert_eq!(src_reader.current_line(), 2);

        src_reader.skip(5);
        assert_eq!(src_reader.current_line(), 3);
    }

    #[test]
    fn can_keep_track_of_column() {
        let vec = INPUT.iter().cloned().collect();
        let mut src_reader = SourceReader::new(&vec);

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