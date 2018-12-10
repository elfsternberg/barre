// Most of the 'cons' lists I've seen on crates.io are more like singly
// linked lists.  But a cons list, even a typed one, looks more like
// the below: Either a cell with two trees, or the contents of a cell,
// or nothing.  This allows me to make a proper lispy tree thing.

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub enum Cell<T> {
    Nil,
    Lit(T),
    Pair(Box<Cell<T>>, Box<Cell<T>>),
}

impl<T> Cell<T> {
    pub fn cons(l: Cell<T>, r: Cell<T>) -> Cell<T> {
        Cell::Pair(Box::new(l), Box::new(r))
    }

    pub fn pairp(&self) -> bool {
        match self {
            Cell::Nil => false,
            Cell::Lit(_) => false,
            Cell::Pair(_, _) => true
        }
    }
    
    pub fn car(&self) -> Option<&Cell<T>> {
        match self {
            Cell::Nil => None,
            Cell::Lit(_) => None,
            Cell::Pair(ref a, _) => Some(a)
        }
    }

    pub fn cdr(&self) -> Option<&Cell<T>> {
        match self {
            Cell::Nil => None,
            Cell::Lit(_) => None,
            Cell::Pair(_, ref b) => Some(b)
        }
    }

    pub fn cadr(&self) -> Option<&Cell<T>> {
        if let Some(p) = self.cdr() {
            p.car()
        } else {
            None
        }
    }

    pub fn cddr(&self) -> Option<&Cell<T>> {
        if let Some(p) = self.cdr() {
            p.cdr()
        } else {
            None
        }
    }
}

macro_rules! cons {
    ($l:expr, $r:expr) => {{
        Cell::cons($l, $r)
    }};

    ($l:expr, $r:expr, $($x:expr),+) => {{
        Cell::cons($l, cons!($r, $($x),*))
    }}
}


#[cfg(test)]
mod tests {
    use super::Cell::*;
    use super::Cell;

    #[test]
    fn simple_list() {
        let t = cons!(Lit('a'), Lit('b'), Lit('c'));
        assert!(t.car() == Some(&Lit('a')));
        assert!(t.cadr() == Some(&Lit('b')));
        assert!(t.cddr() == Some(&Lit('c')));
    }

    #[test]
    fn a_tree() {
        let s = cons!(Lit('a'), Lit('b'), Lit('c'));
        let t = cons!(s, Lit('d'), Lit('e'));
        assert!(t.cadr() == Some(&Lit('d')));
        assert!(t.cddr() == Some(&Lit('e')));
        assert!(t.car().unwrap().car().unwrap() == &Lit('a'));
    }
    
}
