use arena::{Arena, NodeId};
use types::{Parser, Siaa};

#[cfg(feature="render_trees")]
pub fn render<T: Siaa>(arena: &Arena<Parser<T>>, start: NodeId, filename: &str) {
    use std::io::Write;
    use std::fs::File;
    use dot;
    use std::borrow::Cow;
    
    type Nd = usize;
    type Ed<'a> = &'a (usize, usize);

    struct Graph {
        nodes: Vec<String>,
        edges: Vec<(usize, usize)>,
    }

    struct GrammarRenderer<'a, T: 'a + Siaa> {
        start: NodeId,
        grammar: &'a Arena<Parser<T>>,
        graph: Graph,
    }

    macro_rules! pn0 {
        ($self: expr, $node:expr, $text:expr) => {
            {
                $self.graph.nodes.push($text);
                $self.graph.nodes.len() - 1
            }
        }
    }

    macro_rules! pn1 {
        ($self: expr, $node:expr, $text:expr) => {
            {
                $self.graph.nodes.push($text);
                let pos = $self.graph.nodes.len() - 1;
                let child1 = $self.handler($node.left);
                $self.graph.edges.push((pos, child1));
                pos
            }
        }
    }

    macro_rules! pn2 {
        ($self: expr, $node:expr, $text:expr) => {
            {
                $self.graph.nodes.push($text);
                let pos = $self.graph.nodes.len() - 1;
                let child1 = $self.handler($node.left);
                let child2 = $self.handler($node.right);
                $self.graph.edges.push((pos, child1));
                $self.graph.edges.push((pos, child2));
                pos
            }
        }
    }

    
    impl<'a, T: Siaa> GrammarRenderer<'a, T> {
        pub fn new(grammar: &'a Arena<Parser<T>>, start: NodeId) -> GrammarRenderer<T> {
            GrammarRenderer {
                start: start,
                grammar: grammar,
                graph: Graph {
                    nodes: vec![],
                    edges: vec![],
                },
            }
        }

        fn handler(&mut self, nodeid: NodeId) -> NodeId {
            let node = self.grammar[nodeid].clone();

            match &node.data {
                Parser::Emp =>        pn0!(self, node, format!("{:4} Emp", nodeid)),
                Parser::Tok(ref c) => pn0!(self, node, format!("{:4} ':{:?}", nodeid, c)),
                Parser::Eps(ref c) => pn0!(self, node, format!("{:4} e:{:?}", nodeid, c)),
                Parser::Alt =>        pn2!(self, node, format!("{:4} Alt", nodeid)),
                Parser::Cat =>        pn2!(self, node, format!("{:4} Cat", nodeid)),
                Parser::Laz(ref c) => pn1!(self, node, format!("{:4} L:{:?}", nodeid, c)),
                Parser::Del =>        pn1!(self, node, format!("{:4} Del", nodeid)),
                Parser::Unk =>        pn0!(self, node, format!("{:4} Unk", nodeid)),
            }
        }

        pub fn render_to<W: Write>(&mut self, output: &mut W) {
            let start = self.start;
            self.handler(start);
            dot::render(&self.graph, output).unwrap()
        }
    }

    impl<'a> dot::Labeller<'a, Nd, Ed<'a>> for Graph {
        fn graph_id(&'a self) -> dot::Id<'a> {
            dot::Id::new("handler").unwrap()
        }

        fn node_id(&'a self, n: &Nd) -> dot::Id<'a> {
            dot::Id::new(format!("N{}", n)).unwrap()
        }

        fn node_label<'b>(&'b self, n: &Nd) -> dot::LabelText<'b> {
            dot::LabelText::LabelStr(Cow::from(self.nodes[*n].to_owned()))
        }
    }

    impl<'a> dot::GraphWalk<'a, Nd, Ed<'a>> for Graph {
        fn nodes(&self) -> dot::Nodes<'a, Nd> {
            (0..self.nodes.len()).collect()
        }
        fn edges(&'a self) -> dot::Edges<'a, Ed<'a>> {
            self.edges.iter().collect()
        }
        fn source(&self, e: &Ed) -> Nd {
            let &&(s, _) = e;
            s
        }
        fn target(&self, e: &Ed) -> Nd {
            let &&(_, t) = e;
            t
        }
    }

    let mut f = File::create(filename).unwrap();
    let mut g = GrammarRenderer::new(arena, start);
    g.render_to(&mut f)
}

// Using inline here to tell rustc that, if it encounters this function
// render_trees is not enabled, inline it so that not even the
// function call is costed.

#[cfg(not(feature="render_trees"))]
#[inline]
pub fn render<T: Siaa>(_arena: &Arena<Parser<T>>, _start: NodeId, _filename: &str) {
    // No-op
}
