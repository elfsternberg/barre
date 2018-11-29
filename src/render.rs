use std::fs::File;
use arena::{Arena, NodeId};
use std::borrow::Cow;
use std::io::Write;
use types::{Parser, Siaa};
use dot;

type Nd = usize;
type Ed<'a> = &'a (usize, usize);

struct Graph {
    nodes: Vec<String>,
    edges: Vec<(usize, usize)>
}

struct GrammarRenderer<'a, T: 'a + Siaa> {
    start: NodeId,
    grammar: &'a Arena<Parser<T>>,
    graph: Graph
}

impl<'a, T: Siaa> GrammarRenderer<'a, T> {
    pub fn new(grammar: &'a Arena<Parser<T>>, start: NodeId) -> GrammarRenderer<T> {
        GrammarRenderer {
            start: start,
            grammar: grammar,
            graph: Graph {
                nodes: vec!(),
                edges: vec!(),
            },
        }
    }

    fn handler(&mut self, nodeid: NodeId) -> NodeId {
        let node = self.grammar[nodeid].clone();

        match &node.data {
            Parser::Emp => {
                self.graph.nodes.push("Emp".to_owned());
                self.graph.nodes.len() - 1
            },
            
            Parser::Tok(ref c) => {
                self.graph.nodes.push(format!("'{:?}'", c));
                self.graph.nodes.len() - 1
            },
            
            Parser::Eps(ref c) => {
                self.graph.nodes.push(format!("e:{:?}", c));
                self.graph.nodes.len() - 1
            },
            
            Parser::Alt => {
                self.graph.nodes.push(format!("Alt"));
                let pos = self.graph.nodes.len() - 1;
                let child1 = self.handler(node.left);
                let child2 = self.handler(node.right);
                self.graph.edges.push((pos, child1));
                self.graph.edges.push((pos, child2));
                pos
            }

            Parser::Cat => {
                self.graph.nodes.push(format!("Cat"));
                let pos = self.graph.nodes.len() - 1;
                let child1 = self.handler(node.left);
                let child2 = self.handler(node.right);
                self.graph.edges.push((pos, child1));
                self.graph.edges.push((pos, child2));
                pos
            }
            
            Parser::Laz(ref c) => {
                self.graph.nodes.push(format!("l:{:?}", c));
                let pos = self.graph.nodes.len() - 1;
                let child1 = self.handler(node.left);
                self.graph.edges.push((pos, child1));
                pos
            },

            Parser::Del => {
                self.graph.nodes.push(format!("Del"));
                let pos = self.graph.nodes.len() - 1;
                let child1 = self.handler(node.left);
                self.graph.edges.push((pos, child1));
                pos
            },

            Parser::Unk => {
                self.graph.nodes.push("Unk".to_owned());
                self.graph.nodes.len() - 1
            }
        }
    }

    pub fn render_to<W: Write>(&mut self, output: &mut W) {
        let start = self.start;
        self.handler(start);
        dot::render(&self.graph, output).unwrap()
    }
}

impl<'a> dot::Labeller<'a, Nd, Ed<'a>> for Graph {
    fn graph_id(&'a self) -> dot::Id<'a> { dot::Id::new("handler").unwrap() }

    fn node_id(&'a self, n: &Nd) -> dot::Id<'a> {
        dot::Id::new(format!("N{}", n)).unwrap()
    }

    fn node_label<'b>(&'b self, n: &Nd) -> dot::LabelText<'b> {
        dot::LabelText::LabelStr(Cow::from(self.nodes[*n].to_owned()))
    }
}

impl<'a> dot::GraphWalk<'a, Nd, Ed<'a>> for Graph {
    fn nodes(&self) -> dot::Nodes<'a,Nd> { (0..self.nodes.len()).collect() }
    fn edges(&'a self) -> dot::Edges<'a,Ed<'a>> { self.edges.iter().collect() }
    fn source(&self, e: &Ed) -> Nd { let & &(s,_) = e; s }
    fn target(&self, e: &Ed) -> Nd { let & &(_,t) = e; t }
}

pub fn render<T: Siaa>(arena: &Arena<Parser<T>>, start: NodeId, filename: &String) {
    let mut f = File::create(filename).unwrap();
    let mut g = GrammarRenderer::new(arena, start);
    g.render_to(&mut f)
}
