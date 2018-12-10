use std::ops::Index;
use std::ops::IndexMut;

pub type NodeId = usize;

#[derive(Debug, Clone)]
pub struct Node<T> {
    pub left: NodeId,
    pub right: NodeId,
    pub data: T
}

impl<T> Node<T> {
    pub fn new(data: T) -> Node<T> {
        Node {
            left: NodeId::default(),
            right: NodeId::default(),
            data: data,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Arena<T> {
    pub arena: Vec<Node<T>>
}

impl<T> Arena<T> {
    pub fn new() -> Arena<T> {
        Arena { arena: Vec::new() }
    }

    pub fn len(&self) -> usize {
        self.arena.len()
    }

    pub fn is_empty(&self) -> bool {
        self.arena.len() == 0
    }
    
    pub fn last_index(&self) -> NodeId {
        self.arena.len() - 1
    }

    pub fn push(&mut self, node: Node<T>) -> NodeId {
        self.arena.push(node);
        self.last_index()
    }

    pub fn add(&mut self, data: T) -> NodeId {
        self.push(Node::new(data))
    }
}

impl<T> Index<NodeId> for Arena<T> {
    type Output = Node<T>;

    fn index<'a>(&'a self, index: NodeId) -> &'a Node<T> {
        debug_assert!(index < self.arena.len());
        &self.arena[index]
    }
}    

impl<T> IndexMut<NodeId> for Arena<T> {
    fn index_mut<'a>(&'a mut self, index: NodeId) -> &'a mut Node<T> {
        debug_assert!(index < self.arena.len());
        &mut self.arena[index]
    }
}

impl<T> Default for Arena<T> {
    fn default() -> Self {
        Self::new()
    }
}

    
#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn new_arena() {
        let mut arena = Arena::<char>::new();
        let k1 = arena.add('a');
        let k2 = arena.add('c');
        let k3 = arena.add('b');
        arena[k3].left = k1;
        arena[k3].right = k2;
        assert!(arena.len() == 3);
        assert!(arena[2].data == 'b');
    }
}
