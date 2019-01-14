//use std::cmp;

// Streaming iterator
// Doesn't implement the Iterator trait because the skip method would have to
// match the Iterator's skip function def
pub struct StreamingIterator<'a, T: 'a> {
    data: &'a Vec<T>,
    counter: Option<usize>
}

impl<'a, T> StreamingIterator<'a, T> {
    pub fn new(data: &'a Vec<T>) -> Self {
        StreamingIterator { data, counter: None }
    }

    // Same behaviour as Iterator::next
    // If there's some item to read, returns Some(&Item)
    // If there are no more items to read, returns None
    pub fn next(&mut self) -> Option<&'a T> {
        match self.counter {
            Some(c) => if c < self.data.len() {
                self.counter = Some(c + 1);
            },
            None => self.counter = Some(0)
        }

        self.data.get(self.counter.unwrap())
    }

    // If there is some item before the current one, returns Some(&Item)
    // If we haven't started reading or we're at the first item, returns None
    pub fn prev(&mut self) -> Option<&'a T> {
        match self.counter {
            Some(c) => if c > 0 {
                self.counter = Some(c - 1);
            }
            else {
                return None;
            },
            None => return None
        }

        self.data.get(self.counter.unwrap())
    }

    // Differs in behaviour from Iterator::skip
    // It doesn't return an iterator adaptor, but instead skips n items
    // in this iterator
    pub fn skip(&mut self, n: usize) {
        for _ in 0..n {
            self.next();
        }
    }

    // Returns Some(&Item) if there's a next item, None if there isn't
    /*pub fn peek_forward_first(&self) -> Option<&'a T> {
        self.peek_forward(1).and_then(|x| x.first().cloned())
    }*/

    // Returns a Vec<&Item> if there are any items to read, or None if there aren't
    // If n is smaller than the number of items left to read, then it returns
    // all the items left
    /*pub fn peek_forward(&self, n: usize) -> Option<Vec<&'a T>> {
        assert!(n > 0, "n must be higher than 0");

        let idx_start;

        match self.counter {
            Some(c) => idx_start = c + 1,
            None => idx_start = 0
        }

        if idx_start > self.data.len() {
            return None;
        }

        let idx_end = cmp::min(idx_start + (n - 1), self.data.len() - 1); // +1 because end is exclusive

        Some(self.data[idx_start..(idx_end + 1)].into_iter().collect::<Vec<&'a T>>())
    }*/

    // Returns Some(&Item) if there was an item before the current one, None if there wasn't
    // If the iterator is at the end, returns the last item
    /*pub fn peek_back_first(&self) -> Option<&'a T> {
        match self.counter {
            Some(c) => {
                if c == 0 { return None; }

                self.data.get(c - 1)
            },
            None => None
        }
    }*/
}

impl<'a, T> Clone for StreamingIterator<'a, T> {
    fn clone(&self) -> Self {
        StreamingIterator { data: self.data, counter: self.counter }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    static INPUT: [char; 5] = ['A', 'B', 'C', 'D', 'E'];

    #[test]
    fn next() {
        let vec = &INPUT.to_vec();
        let mut iter = StreamingIterator::new(vec);

        assert_eq!(iter.next(), Some(&('A')));
        assert_eq!(iter.next(), Some(&('B')));
        assert_eq!(iter.next(), Some(&('C')));
        assert_eq!(iter.next(), Some(&('D')));
        assert_eq!(iter.next(), Some(&('E')));
        assert_eq!(iter.next(), None);
    }

    /*#[test]
    fn peek_forward_first() {
        let vec = &INPUT.to_vec();
        let mut iter = StreamingIterator::new(vec);

        assert_eq!(iter.peek_forward_first(), Some(&('A')));
        iter.next();
        assert_eq!(iter.peek_forward_first(), Some(&('B')));
        iter.next();
        assert_eq!(iter.peek_forward_first(), Some(&('C')));
        iter.next();
        assert_eq!(iter.peek_forward_first(), Some(&('D')));
        iter.next();
        assert_eq!(iter.peek_forward_first(), Some(&('E')));
        iter.next();
        assert_eq!(iter.peek_forward_first(), None);
    }

    #[test]
    fn peek_forward() {
        let vec = &INPUT.to_vec();
        let iter = StreamingIterator::new(vec);

        assert_eq!(
            iter.peek_forward(15).map(|x| x.into_iter().collect::<String>()), 
            Some("ABCDE".to_owned()));
    }

    #[test]
    fn peek_back_first() {
        let vec = &INPUT.to_vec();
        let mut iter = StreamingIterator::new(vec);

        iter.next();
        assert_eq!(iter.peek_back_first(), None);
        iter.next();
        assert_eq!(iter.peek_back_first(), Some(&('A')));
        iter.next();
        assert_eq!(iter.peek_back_first(), Some(&('B')));
        iter.next();
        assert_eq!(iter.peek_back_first(), Some(&('C')));
        iter.next();
        assert_eq!(iter.peek_back_first(), Some(&('D')));
        iter.next();
        assert_eq!(iter.peek_back_first(), Some(&('E')));
        iter.next();
        assert_eq!(iter.peek_back_first(), Some(&('E')));
    }*/

    /*#[test]
    fn skip() {
        let vec = &INPUT.to_vec();
        let mut iter = StreamingIterator::new(vec);

        assert_eq!(iter.peek_forward_first(), Some(&('A')));
        iter.skip(1);
        assert_eq!(iter.peek_forward_first(), Some(&('B')));
        iter.skip(2);
        assert_eq!(iter.peek_forward_first(), Some(&('D')));
        iter.skip(2);
        assert_eq!(iter.peek_forward_first(), None);
    }*/
}