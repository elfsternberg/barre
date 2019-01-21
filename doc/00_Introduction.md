# REGEX Science

## Introduction

I was inspired by Araz Abishov's [Rope
Science](http://abishov.com/xi-editor/docs/rope_science_00.html) series,
in which he outlines the underlying technology of ropes (super-powered
strings in the programming language sense of the word) used in the text
editor project he works on,
[Xi](https://github.com/xi-editor/xi-editor).  I was also inspired by
Feynman's Technique for Learning Anything, which is pretty much how I
got into this:

1. Pick a topic and start studying it. Write everything down. 
1. Try to teach the topic to someone else.
1. The gaps in your knowledge will be obvious. Go back and study.
1. Simplify, simplify, simplify.

Still, a warning. Regular expressions, for all their practical use, are
fundamentally and mathematically *weird*. Although this series is about
a regular expression engine written in
[Rust](https://www.rust-lang.org/), there are going to be some Haskell
words thrown around later. Words like "catamorphism," and "semiring,"
and "least fixed points of operations on sets."

Part of this document's job will be to explain those terms as clearly as
possible.

There is a fairly long bibliography.

## Chapter 1: The very, very, very beginning.

### What is a regular expression?

What is a regular expression?  Let's build up from the bottom: we start
with:

Alphabet
: An alphabet is a set of symbols

Word
: A word is a sequence of symbols from an alphabet

Language
: A language is a set of word sequences

If our alphabet is ASCII and words are English, then a very simple
language would be something like 

    Common_Pets: {dog, cat, fish, hamster, parakeet}.

For the most part, we're going to concentrate on ASCII and Unicode, and
not until much, much later in this series are we going to discuss are we
going to go with anything else.  So for our purposes, I'm going to use
"character" instead of "symbol."  Because our "words" aren't words as
Latin languages understand them, I'll use "string" instead:  

- A language is a set of strings made up of characters.

### Deterministic finite automata

The other thing we have to understand is: What is a deterministic finite
automata, or DFA?  A DFA is a graph, a series of nodes and arrows, that
describes a state machine.  A small DFA looks like this:

          +---+    +---+    +---+    +---+
          |   | C  |   | A  |   | T  | ! |
    +---->+   +--->+   +--->+   +--->+   |
          +---+    +---+    +---+    +---+
          
This is a DFA.  Each node represents a state in the processing of a
string, and each arrow represents a transition rule from one state to
the next.  This one recognizes "CAT".  The exclamation point in the
box means that when the DFA transitions to that box, the DFA is in the
"Accept" state, and it has recognized the string.  Any other string, or
an incomplete input, or no input, and the DFA is in the "Reject" state.

                            +---+
                        U   |   |
                      +---> |   |
                      |     +-+-+
                      |       |
          +---+    +--++    +-v-+    +-v-+
          |   | C  |   | A  |   | T  |   |
    +---->+   +--->+   +--->+   +--->+   |
          +---+    +---+    +---+    +---+

This DFA has an "or" in it.  In a traditional regular expression, we'd
write this as `C(A|U)T`, and it recognizes "CAT" or "CUT".

A DFA defines a *regular language*.  There's a if-and-only-if
relationship between DFAs and regular languages: A language is "regular"
if one can construct a DFA that recognizes it.  It's called "regular"
because you can proceed from one to the other without any additional
memory aside from the DFA itself.

And that's the whole definition: 

- a regular expression is a written formula that can be converted into a DFA
- the DFA can be used to recognize if a string passed to it belongs to a
regular language described by the expression.

That circular definition is Regular Expressions 101.

### Weeds: Non-deterministic finite automata

A DFA is the tool of choice for recognizing regular expressions, but if
a regular expression has a lot of alternatives, especially nested
alternatives, may result in an explosion of nodes.  As an alternative,
some regular expressions use non-deterministic finite automata (NFA). An
NFA is one in which each node may have multiple arrows leading to other
nodes *for the same character*.  For each successive character, multiple
states have to be checked for transition, and as long as one of them
succeeds, the NFA is still seeking the Accept state.

An NFA uses signficantly less memory than a DFA; at most, it uses twice
the memory as the number of states, once for the nodes and transition,
and once for the largest possible number of nodes that may be tracked on
any iteration, at the cost of some CPU.

There are a number of variants on representations of NFAs; the most
common algorithm, [Thompson's
Construction](https://en.wikipedia.org/wiki/Thompson%27s_construction),
creates NFAs with lots of "empty" states, that immediately transition
even when no character is available for processing.  [Glushkov's
Construction](https://en.wikipedia.org/wiki/Glushkov%27s_construction_algorithm)
generates NFAs without empty states.

Every regular expression can be turned into a DFA, every DFA can be
turned into an NFA, and every NFA can likewise be rendered as a regular
expression.  They are just different representations of the same thing.

### Recognition isn't everything

One thing you may have noticed is that all we've discussed is how
regular expressions *recognize* strings that belong to the language they
represent.  And in fact, that's all they could do for a very long time.
A regular expression is an *expression* in the [programming language
sense](https://fsharpforfunandprofit.com/posts/expressions-vs-statements/):
after construction (and note, that's very important), a regular
expression is a function that takes a string and returns Accept or
Reject. True or False: that string is in the language the regular
expression recognizes.

Modern regular expression engines want more than that.  We want data to
be returned.  Parentheses in modern regex dialects return their
contents, letting us find and analyze the content of strings.  Modern
regular expression engines *parse* our data for us.

The most common regular expression engines are
[PCRE](https://www.pcre.org/) and [RE2](https://github.com/google/re2).
Both use a three-step strategy for regular expressions.  They first
analyze the regular expression with a cost and complexity function, and
if the expression is simple and only basic recognition is wanted, they use
a DFA.  If the expression would result in an exponential explosion of
nodes, they switch to an NFA.  And if backreferences are required, they
break the resulting NFA into smaller segments and use a full-fledged
virtual machine, a mini-programming language interpreter, to do the
work, store the result, and return the verdict.  All of them start from
Thompson's Construction.

The data these engines return is ad-hoc and opportunistic.  It does what
users want it to do, which is fine, but the capture groups and named
capture groups are just "tacked onto the side."  Often with much
engineering thought, but still, not central to the algorithm.

### The Alternative

Thompson and Glushkov's Constructions work because they're easy to
understand and code (in theory, although in practice PCRE and RE2 are
engineering marvels).  There is a third mechanism, described in 1965:
Brzozowski's Algorithm.

Janus Brzozowski proposed a simple ([but not
easy](https://www.infoq.com/presentations/Simple-Made-Easy)) algorithm
for constructing a given regular expression, with its sequences of
letters, alternatives, and so forth, into a very simple sequence, and
then he said that the *only* DFA you really needed was the one necessary
to handle the very next character; you only needed to construct the next
stage of the DFA once that character had been processed.

The Brzozowski algorithm starts with a regular expression, and then *for
each character* generates an entirely new regular expression designed to
handle *the rest of the candidate string*.  When the string is empty, if
the algorithm accepts the empty string, we are in an Accept state;
otherwise Reject.

That's the theory.

For our next chapter, we'll get deeper into the theory.  A *lot*
deeper.  Thompson is an engineer (and a good one; he gave us Unix,
Unicode, and the Go programming language), and to the extent that he
cares about "the math" it's only to shore up his intuition.  As he once
said, "When in doubt, use brute force."

Thompson's Construction is elegant, but still brute force.  That's why
it's called a *construction.* Brzozowski's Algorithm is called an
*algorithm* because Brzozowski was a mathematician.

Get ready for some math.

## Chapter 2: The Math of Regular Expressions

In 1951, Stephen Kleene invented regular expressions.  He said there
were six common operations that we wanted to do with search strings, and
they are:

    R ::= ∅ | ε | c | R1 ◦ R2 | R1 ∪ R2 | R*

Regular expressions are built out of other regular expressions.  Each
one of those is a function that takes a string and returns Accept or
Reject.

The first three are the primitives. `∅` returns Reject no matter what
you give it.  It's always Reject.  `ε` return Accept if you pass it an
empty string, and Reject for anything else. `c` is a character; it
return Accept if the string you pass it has exactly that character, and
Reject otherwise.

The last three are the composites.  `R1 ◦ R2` is just sequence: the
first regular expression followed by the second.  The expression "ab" is
three regular expressions: two characters, and a sequence to put them in
order. `R1 ∪ R2` is alternation; if either returns Accept, then the
whole expression returns Accept. `R*` is the star operator you know: if
zero or more instances of the contained expression Accepts, then the
whole expression Accepts.

Thompson's Construction basically starts at the head of the regex and
begins, primitive by primitive and composite by composite, recursively
building out an NFA until there is only a list of characters to
recognize, a table of transition states from one to the next, and a
stack as big as that table of "next states."

Brzozowski's algorithm is understandably different. Brzozowski wondered
if, for any regular expression after one character, you could derive a
new regular expression for the rest of the language to be recognized.
He discovered that there was, and this is what he came up with: 

    Dc(∅) = ∅
    Dc(ε) = ∅
    Dc(s) = if c == s then ε else ∅
    Dc(R1 ∪ R2) = Dc(R1) ∪ Dc(R2).
    Dc(R*) = ε ∪ Dc(R) ◦ R
    Dc(L1 ◦ L2) = if ε ∈ R1 then Dc(R1) ◦ R2 else Dc(R1) ◦ R2 ∪ Dc(R2)

Here, `Dc` means "The derivative with respect to a character."  As you
can see, null becomes null, empty becomes null, a character becomes
empty (this is used as a signal to "keep going").  The derivative of an
alternation is the alternation of the derivatives, the derivative of the
star was the derivative of the language, sequenced with the star of the
language.

And sequence is where things get very tricky.  Sequences are of two
other expressions, so if the left expression *can* handle empty, then
both the left and right have to be assessed as possible alternations.
But if the left expression is long, how can you know it *can* handle
empty?  Brzozowksi discovered a function to determine this, and used `δ`
to symbolize it.

    δ(∅) = ∅
    δ(ε) = ε
    δ(c) = ∅
    δ(R1 ∪ R2) = δ(R1) ∪ δ(R2)
    δ(R1 ◦ R2) = δ(R1) ◦ δ(R2)
    δ(R*) = ε.

*RANT:* This function is called "the nullability function."  Each of
Brzozowski's `Dc` functions returns another regular expression, and we
say an expression is "nullable" when it can return the *empty string
expression*, the `ε` (epsilon) expression, *not* when it can return the
*empty expression*, the one symbolized by `∅` (null). This drove me
crazy for about a month. Just remember that when we talk about the
nullability function, we're talking about the one that determines
whether or not the derivative of a regular expression can (not will,
*can*) return the empty string handling expression.

You may wonder why the nullability rules have `∅` or `ε` as their
result, instead of `True` and `False`.  That reason is simple: the
sequence rule is easier to write:

    Dc(R1 ◦ R2) = (Dc(R1) ◦ R2) ∪ (δ(R1) ◦ Dc(R2)).
    
Here, `∅` becomes an *annihilator*.  `∅` sequenced with anything becomes
just `∅`, and `∅ ∪ R` becomes just `R`, while `ε` is just the identity,
so `ε ◦ R == R`.  Think of `∅` and `ε` as zero and one, `◦` as
multiplication, and `∪` as addition. (In fact, deep, deep down in
abstract algebra theory, sequencing it is *exactly* like multiplication
and alternation is *exactly* like addition.  So much so that we can
apply certain strategies to make this engine go much, much faster than
the theory would suggest.)

That's the start of the math.  We'll get deeper into it in a moment.

But first, let's talk about the challenges.

## Challenges.

> "You seem a decent fellow. I hate to kill you."  "You seem a decent
> fellow. I hate to die."

Recognition vs Parsing
: The algorithm so far still only handles recognition!

It's slow (part 1)
: The sequencing operation can produce a ton of useless nodes, and
that's computation time we could put to better use.

It's slow (part 2)
: Because the front of the operation is constantly changing the regular
expression, the nullability of expressions under consideration can also
change.  For every character, we must traverse the entire remaining tree
to determine nullability, and that's computationally expensive.

It's slow (part 3)
: Sequences are always in pairs.  A poorly-built sequence, represented
in memory as, for example `seq(seq(seq(seq('a', 'b'), 'c'), 'd'), 'e')`
will create a tree with a left-heavy branch that the engine must
traverse to reach the *current* node of interest.

It's memory-intensive
: The algorithm leaves behind a lot of *abandoned nodes* as it proceeds
forward.  In a garbage-collected language that wouldn't be a problem,
but the target language here is Rust.

It's ambiguous 
: This is a common problem in regular expression engines.  for the
expression `(a|ab)` and the string "absent", what do you get back? The
answer is: *it depends*.  Perl, POSIX, BSD, and PEG all have different
rules for disambiguation of alternatives.

It's unremarkable
: There are already several regular expression engines, including two
for Rust.

Here's a hint of what's to come: with the BARGE common core, you get
regular expression parsing.  You also get *Context Free Grammar* parsing
supporting left-recursion for free, so you can write programming
language parsers with BARGE.  You get *an arbitrary disambiguation
strategy* supporting "all of the above."  You get *a complete,
specified, type-safe data extraction strategy*.  Combine those three and
you get *PEG parsing*... well, not "free", but with not that much extra
work (until you add semantic actions, then things get hairy).  The data
extraction strategy allows you to build a *streaming parser* and a
*partial parser*, giving you a Language Server Protocol for your
programming language, with syntax error checking, for free.

The interior protocol is easily extensible, which allows us to support
Extended Regular Expressions.  Regular Expressions can be wholly
expressed in set-theoretic language, the most evident of which is the
use of the union operator `∪` to describe the sets of return values from
alternation.  But an Extended Regular Expression (ERE) can also support
intersection (both expressions must Accept on a simultaneous sequence of
characters), negation (an expression that returns Accept is rejected,
and vice versa), and more.  And extension to ERE (yes, there are such)
adds the interleaf operator, which is like sequence in that every child
expression must be present, but they can be present *in any order*.

Yes, there is a use case for the interleaf operator.

And just to push what BARGE can do even further, consider this: regular
expression functions are *membership testing* and answer the question
"is this string a member of the language described by this expression?"
Let's say you're writing a programming language with algebraic data
types.  Checking to see if a type declaration in the current scope is
valid is also membership testing.  If instead of ASCII or Unicode, your
alphabet consists of the set {Int, Float, Char, Bool, Unit, List,
Function, Constructor}, BARGE can be the core for a type checker.

And yes, I think we can make it go fast.

## The First Implementation: Naive Implementation

The first successful implementation of Brzozowski's algorithm in Rust,
which can be found deep in the git log for the project, is just a
recognizer for simple regular expressions.  It was a testbed for the
entire project, and it ended up going through several wild gyrations
before settling on the version that was committed to Github.  It had the
two basic features: Nodes that processed and derived their own
derivatives, and a table of rules for nullability.  Nullability was
derived repeatedly and with recursive scanning that did no caching
whatsoever.

### The Memory Arena

The first implementation also made a fateful decision: the table of
nodes, and their relationships, would be kept in a [memory
arena](https://exyr.org/2018/rust-arenas-vs-dropck/).  That decision
stands as of today.  The arena is hand-made and crude, and I have plans
to revisit it once I better understand the problems involved.  The use
of indexes into the arena as pointers to other nodes has been a boon to
performance, but the memory costs are substantial, and remain so.

## The Second Implementation: Smart Epsilons

The second implementation had a *lot* of improvements, so I'm going to
talk about them in theoretical terms.  As I mentioned in the
*Challenges* section, the algorithm in V.α1 only recognized, rather than
parsed, the contents of the regular expression.  There is a way to fix
this.

### Smart Epsilon

Fritz Henglein [gave a
lecture](http://www.cs.ox.ac.uk/ralf.hinze/WG2.8/27/slides/fritz.pdf)
about the ad-hoc, opportunistic instrumentation in most regular
expression engines to derive *some* information.  "Ideally," he said,
"regular expression matching is about parsing the string and, if
accepted, doing catamorphic processing on the resulting parse tree."

"Catamorphism" is one of those *Haskell words* I warned you about. The
word has the same root as "catastrophe," and can be thought of us a
destructive operation.  It's not difficult concept: it's the `reduce`
function, but applied to containers in a generic way.  The function "sum
of everything in a list of integers" is a catamorphism: it extracts
useful information from a list, but what it returns isn't a list, and
the contents of the list can't be derived from its sum.  The function
"sum of everything in a tree of integers" and "sum of everything in a
set of integers" are *the same catamorphism*: `sum` of a collection,
without caring about what kind of container it is. As long as there's a
rule for visiting every element once, the `sum` catamorphism can be
applied.

So that's what Henglein is talking about.

Matt Might's [Parsing With
Derivatives](http://matt.might.net/papers/might2011derivatives.pdf)
takes this idea seriously.  In the third iteration of his
[Racket](https://racket-lang.org/) implementation, he introduces the
*smart epsilon*.

If we consider, as Brzozowski does, a regular expression as a
left-to-right succession of nested regular expressions, the "front" of
the algorithm moves rightward with each character analyzed, replacing
nodes at the front with successive derivatives, until both the string
and the expression are fully consumed, and everything to the left of the
front is discarded.  Henglein and Might say, instead, that we should
*replace* the processed nodes with nodes containing the actual content
of the string that was accepted.

The smart epsilon does that; it can be constructed with `None`, or with
`Some(char)`.  In the latter case, it represents a character that has
been processed and accepted.  If the entire operation succeeds, we scan
the entire tree one last time, joining all the found characters, and
returning the string.

While this is technically a catamorphism (the structure of the returned
parse tree is destroyed), it's highly unsatisfying and ad-hoc.  To meet
Henglein's challenge, we need something more advanced.

## The Third Implementation: The Big Recurse

The third implementation introduces *recursive regular expressions* into
the system.  And this is where things started to get very, very
strange.

Let's review: Brzozowski's Algorithm defines a directed finite automata
(a directed graph) of nodes that define a character, a sequence of two
nodes one after another, an alternative between two nodes, and the
repetition of nodes, along with two "special" cases, the null case and
the empty string case.  In the case of sequences, if the left node can
accept the empty string, then both the left and right nodes must be
processed simultaneously as alternatives; this exactly corresponds to
"empty set node" conditions in Thompson's Construction of an NFA.

As a reminder of my rant in chapter 2, testing for the "empty" case is
called *nullability*, and testing for the "null" case is called
*termination*.  Yeah, I'm still not happy about that.

### The Problem with Recursion (see: The Problem with Recursion)

The problem with a recursive regular expression is that it's
*recursive*, and in our DFA that means at least one complete node
*points back* to itself under some condition.  For the most basic
example, we could take out the repetition operator (the star operator,
`*`) and replace it with recursion (the little 's' symbol there
indicates that this is the *smart epsilon* implemented in the second
iteration):

>    `R∗ = εₛ ∪ (R ◦ R*)`

And while that looks straightforward, let's look at a variation:

>    `R = εₛ ∪ (R ◦ c)`

The derivative of this is:

>    `Dc(R) = εₛ ∪ Dc(R) ◦ c`

We can go without the alternative concatenation operator because we know
the rule `δ(c) = ∅`, so the alternative is annihilated.  

Look at what it says: To know the derivative of R, we need to know the
derivative of R!  As Matt Might says, "Mathematically, this is
sensible. Computationally, it is not."

There are three strategies deployed to solve this.

### Laziness

In Might & Adams' initial implementations, *laziness* is used to prevent
the initial infinite loop with recursion.  For every character
processed, we only do *the minimum amount of work necessary* to create
the next "head" of nodes.  Finding the derivatives of those nodes is
suspended until they are absolutely needed.

David Darais' created a sort of two-step operation instead; in his
implementation, he inserts a temporary node with a new type, "Unknown,"
as the target of the recursion.  He then processes the child nodes,
determines what kind of node the temporary node should be, and replaces
it.  By doing so, the child nodes *don't* have a pointer to their real
parent and don't go into an infinite loop; instead, they simply halt
there after having done just enough work to handle one more character.
This implements the suspension of recursive derivation until needed in
non-lazy languages.

### Memoization

But this isn't enough for sequencing.  Sequencing requires we know the
nullability of the entire sub-graph of the node being analyzed.  To
achieve this, Might introduces *memoization*; a table of type `[(R, c) ->
R]`, that is, given a language and a character, if we've already
determined what the derivative of a regular expression with respect to a
character is, go use that derivative instead.  When encountering a
memoized value, we can completely skip the rest of the processing; we
know it's a loop, and won't resolve.

### Fixed Points of Nullability
 
And here's where I'm into fuzzy, difficult territory.  Determining the
nullability of a recursive regular expression can't be done with
memoization and laziness; we don't need a structure that represents the
current state of the process, we need a boolean value that says, of an
entire expression, whether or not it is nullable.  If the expression
contains a recursion, this is still a problem.

Might reaches into recursion theory to discuss *the least fixed point of
a function* as the solution to the problem. Now, I tried to understand
the fixed point theorem, and I *sorta* understand what's going on? 

Let's start with two basic ideas.  The first is from programming: what
is a loop?  Imagine a function (a pure function, one that takes only
values as input and delivers only values as output), and it has a loop:
what is the minimal correct value of that function?  Well, what if the
loop turns into an infinite loop?

Hold that thought.  We're wandering into set theory next.  (In fact, all
of Regular Expressions come out of set theory; I sometimes think
computer scientists just borrow from mathematicians whenever they find
themselves stuck, and rule by analogy.)  What is a set?

"A set is a collection of unique objects."
[Wikipedia](https://en.wikipedia.org/wiki/Set_theory)

What kind of objects?  It doesn't matter.  In fact, trying to reason
about the kinds of objects is part of the problem.  A set is a pure
abstraction, a notion of a collection of unique objects.  (We should get
used to not reasoning about the "objects" in the set, because category
theory will also involve reasoning about objects and "arrows".)  In this
documentation, we will be discussing some types of objects, such as
numbers, but also sets of expressions, sets of trees, and sets of sets.

An ordering relation is a relationship between two objects that
establishes one is "lesser" or "before" the order "in some way."  That's
all it means.

A *partially ordered set*, or *poset*, is a set with an ordering
relation defined on it.  Every object in the set may be compared with
any other, but not every object in the set may respond to the ordering!
The classic example of a poset is the natural numbers (the integers
0, 1, 2, ...) with the ordering "is less than or equal to."  This
ordering is written (ℕ, ≤).

It's important to make a distinction here.  Yes, every natural number is
unique, and every two unique natural numbers have a pairing such that
one is less than the other. But that doesn't have to be true; let's say
S is the set of all words in the dictionary, and the relationship is
"the length of each word is less than or equal to."  Your poset is still
valid: every word's length is less than *or equal to* every other
word's.

So let's back up: a pure function is a mapping between two sets: the
domain and the codomain.  (Note: "range" is the set of all possible
values; "codomain" is the set of all values the function will generate
given the definition of the domain.  We'll mostly use codomain.)  A
function with a loop may not terminate, so for some input values no
output value is possible.

Every regular expression *is a* function; that is, it has a distinct
input (a string), and a distinct output (a boolean for recognition; a
parse tree for parsing).  The nullability of a regular expression is
also a function; it's input is a regular expression, and its output is a
boolean. The derivative of a regular expression is itself a regular
expression, which means that the derivative function may have a *fixed
point.*

A function has a fixed point if, when you insert that value into the
function, you get the same value out.  The derivative of a regular
expression is another regular expression, so it is possible to find the
fixed of a regular expression.  The regular expression `a*` has many
fixed points, but one of them is *least*, one of them does the least
amount of work, produces the smallest parse tree, and iterates the
fewest times: `ε`.  We assume that our expressions always terminate,
that on each iteration they consume a string of finite length, so we
assume that every regular expression, even a recursive one, has a fxed
point.

The smallest regular expression that correctly handles a recursive
expression is the *least* expression, the *least upper bound* on the set
of possible expressions, and is the *least fixed point* of that
expression.

Might finds this least fixed point this by starting with the node, and
recursing down the graph, deriving repeatedly in a loop, asserting
nullability of each node from a terminal upward, avoiding nodes that
have already been determined to be nullable to prevent the internal
looping of the graph.

Adams' approach is different, and it's the one I've used here.  In this
formulation, a map of the nodes and their nullability is built into the
parser; the nullability states are `Accept`, `Reject`, `Unknown`, and
`Processing`.  The map provides a cache of *known nullabilities* for
existing nodes.  When processing nullability, a map of nodes to
immediate parent nodes is made; whenever a node's nullability is
established, the parent nodes are notified and their nullability is
re-calcuated, bounding *up* the tree of nodes until the verdict is
delivered to the requesting function.

It's clever, and as Adams notes, eliminates 98.5% of the calls Adams
makes, processing the nullability tree, when parsing the Python grammar.

Congratulations, you've made it through some heady math.  If you want to
know more, this is what I read to try and understand it better: [The
Fixed Point
Theorem](http://www.cs.cornell.edu/courses/cs6110/2013sp/lectures/lec20-sp13.pdf).

## The Fourth Implementation: Smart Constructors

The fourth implementation is actually taken less from example source
code and more directly from Adams' paper, [On the Complexity and
Performance of Parsing with Derivatives](TK), in which he outlines
certain substitutions.

Kleene, the "discoverer" of regular expressions, defined them as
[operations on a set](https://en.wikipedia.org/wiki/Kleene_algebra).  In
fact, they're operations on a partially ordered set, which is why Might's
& Adams's adaptations work so well.  But given that a regular expression
is a set (and we've used some set notation to describe them already),
it's possible to say some interesting things about them.

Let's define a regular expression the way Kleene did: ({}, {ε}, ⊕, ⊗,
𝔸˟), that is, an expression can be empty, the empty string, alternation,
or sequencing of words derived from an alphabet).  In this construction,
called a *semiring*, the first two elements have a special role.
Another semiring is arithmetic of natural numbers (remember them?), and
are defined as (0, 1, +, ⨯, ℕ).  Categorically, we can "map" these
operations up to the above operations, and set theory will show that
these hold true:

- `0 * n == 0` is categorially `{} ⊗ R = {}`; that is, the empty
expression rejects everything, so if it's concatenated with any
expression, the concatenating expression always rejects.

- `1 * n == n` is categorically `{ε} ⊗ R == R`; the empty string
concatenated with expression is just that expression.

- `0 + n == n` is categorically `{} ⊕ R`; the reject string alternated
with an expression is just that expression.

Since alternation and especially concatenation are the source of the
greatest number of new nodes generated in our system, it behooves us to
try and eliminate the number of new nodes generated.

In several instances in the code, I replaced the naive constructors with
node factories, which in turn will, given the child nodes to be
associated with it, return something other than the node requested.
If a sequence node's derivative children would produce a null parser,
the other child node is returned unwrapped, and so on.

Between Adams' reduction of the cost of nullability determination, and
the automated elimination of "dead" paths, Brzozowski regular
expressions are now a stone's throw away from competing with Rust Regex.

## The Fifth Implementation: Internal Catamorphisms

So this is where things start to get even weirder.  With me so far?
Right, what have we done?

1. Created a memory model that's leaky but fast.
2. Implemented the fastest construction set possible.
3. Implemented the fastest nullification paths possible.
4. Implemented a universal record-keeping scheme to track parsing.

The word "universal" there is pretty important.  It goes back to
Henglein's point: *our* parse engine returns everything we care about,
but... not really.

One of the things that modern regular expression engines *do* is return
structured information.  It returns this information by *annotating* the
regular expression tree at certain points with information unrelated to
the act of parsing.  The most obvious of these is the *collection
group*, an array of values extracted from sub-expressions of the regular
expression.

Might proposes that we steal a concept from *parser combinators* with
which to implement Henglein's ideal: we create a new node called a
*Reduction*.

If you're familiar at all with functional programming, you will have
encountered `map`, `filter`, and `reduce`.  Reduce is the catamorphism:
with it, we convert the parse tree into *something else*.  We can
annotate it with labels and return a `struct` of parsed information,
such as a capture group.  Or we can do some serious processing on it and
return a struct that's some node in the intermediate representation of a
programming language.

For the fifth iteration, I implemented *internal catamorphism* only.  If
the input type is a string, then its symbols are chars, and the return
type is no longer a tree of chars, but instead just another string.  The
catamorphisms introduced in this iteration are:

- if a smart epsilon of type string is concatenated with an expression,
the value of the smart epsilon is replaced with itself concatenated with
the result of the expression.

- if a tree is a left-heavy sequence (see "It's slow, part 3" above), we
can rebalance it to be right-heavy and linear, but then we have to use a
reduction to restore the resulting tree to a left-heavy sequence of
results.

- if we have multiple catamorphisms, we can move them up above the
derivation, so that each only has to handle the result once.

In short, we introduce a new node that holds a *function* designed to
take a parse tree and return a parse tree.  If we define the tree as
holding strings rather than characters, then what we get back is a
highly efficiently parsed string of the result.  This is barely better
than the previous iteration (only the left-heavy sequence is improved,
and that ought to be a rare occurence).  But it lays the ground work for
the next iteration.

## The Six















# BARGE and BARRE

Welcome to Barge (Brzozowski's Algorimth for Regular Grammars Engine)
and Barre (Barge Applied to Rust Regular Expressions).  In this series
of documents we describe what regular grammars and regular expressions
are, where they came from, and how Barge and Barre handle them
differently from the traditional mechanisms found in most programming
languages.

The term "regular expression" has come to mean a function that
recognizes a specified string of text in a larger body of text.  The
function is specified via a string of its own: `/i want (some )?
(c[oa]ke|cookies)/i` would recognize the strings "I want Coke" and "I
want some cookies" but not "I want the chocolate".

A regular expression therefore is a miniature programming language that
compiles into a function that analyzes strings and returns useful
information about the string.  That information could be a simple "Yes,
the string is present," as it is in the familiar Unix `grep` utility, or
it could be a full parse of the string, returning small,
extracted components of the string for further analysis.

But regular expressions are much more than that, and as we'll see in
this documentation, they lead to some very interesting places in the
development of programming languages.  More importantly, some modern
discoveries about the implementation of regular expression leads to the
unification of regular expressions and context-free grammars— and the
replacement of many of the ad-hoc formalities of lexing-vs-parsing with
a unified library of tools for turning text into data structures.

There is no getting around one important detail: Barre gives the user an
experience similar to that of the Rust Regex library, but the underlying
toolkit, Barge, is a significantly different and more powerful tool than
the usual regular expression libraries.  It's a little bit slower, but
that sacrifice is speed is, in our opinion, more than made up for in the
utility, power, and capability of the Barge engine.







## "Regular"

In 1958, [Stephen
Kleene](https://en.wikipedia.org/wiki/Regular_expression) took [Noam
Chomsky](https://en.wikipedia.org/wiki/Chomsky_hierarchy)'s notion of
linguistic hierarchies and applied them to computer science.  He started
with the following ideas.

+ *letters* are the atoms of all linguistics.  In this context, a letter
is any symbol, not just the usual ones.  Every letter, character, space,
tab, and emoji in the Unicode specification is a "letter" in the formal
sense.
+ An *alphabet* is a set (in the formal mathematical sense) of of
letters.  The ASCII set is an alphabet.  The Unicode specification is
a different set.  The symbols that make up Morse code are yet a third,
distinct set.
+ A *word* is a finite sequence of letters from a given alphabet, but
don't make the mistake of thinking that "word" always corresponds to
spoken language's idea of what "word" means.  In the database
programming language SQL, for example, "GROUP BY" is a single word: a
finite sequence of letters that has a single semantic meaning.
+ A *string* is a sequence of words strung together in a specific order.
+ A *language* is a set of strings composed out of a given alphabet.

That last one is the "formal" definition, but we human beings often
apply a qualifier to it: it's the set of strings that have an existing
semantic relationship, that is, it's the set of strings that *make sense
to us* in some way.  The tools that Kleene first described have come to
be known as *parsers*: they take strings of data in and convert them
into something meaningful: yes/no (Yes, I recognize that string; No, I
don't), Yes/and (Yes, I recognize that string, and here are the parts
you wanted extracted), or Yes/plus (Yes, I recognize all the text in
that document and here is the spreadsheet (or video, or music, or
executable) it represents).

Kleene then went on to define three "root" operators for languages, and
three combinations of those languages.  Think of these as functions:

> L ::= ∅ | ε | c | L1 ◦ L2 | L1 ∪ L2 | L1<sup>*</sup>
    
+ `∅` 
: NULL: Always returns False
+ `ε` 
: EMPTY: Returns True if the string handed to it is empty.
+ `c` 
: TOKEN: Returns True if the *letter* passed to it matches the letter with
which it was _constructed_.
+ `L1 ◦ L2` 
: SEQUENCE: Constructed out of two languages, this returns True if first language
takes the string and returns true *followed by* the second language
taking *what remains* of the string and returning true.
+ `L1 ∪ L2` 
: ALTERNATIVES: Constructed out of two languages that both take the string
at the same time, this returns True if *either* language returns true.
+ `L1*`
: REPETITION: Constructed out of another language, this returns True
if the string passed it contains zero or more instances of that
language.

There are some "extended" versions of this language, which you may
recognize:

+ `L1 ∩ L2`
: INTERSECTION: Constructed out of two languages that both take the string at
the same time, returns True if both languages return true.
+ `¬L1` 
: NEGATION: Constructed out of another language, returns True if L1 returns
False, and False if L1 returns True.
+ `L1+`
: AT LEAST ONE: Constructed out of another language, this returns True
if the string passed to it contains one or more instances of that
language.  
+ `L1 | L2`
: INTERLEAF: Constructed out of two languages, returns true if L1 is
followed by L2, *or* L2 is followed by L1.

The **+** operator can be built out Kleene's base operators:
`L+ ::= L ◦ L*`.  Likewise, regular expression
libraries that support both intersection and negation can express some
extremely difficult languages that can't be expressed easily.

The "interleaf" operator may ask: why would anyone want that?  Consider
an operation over programming language tokens, rather than individual
letters of an alphabet, as advanced parsers like YACC may do, for
parsing HTML.  The attributes of an HTML tag can be *in any order*.  

The interleaf operator allows you to write an [HTML parser in
regex](https://stackoverflow.com/questions/1732348/regex-match-open-tags-except-xhtml-self-contained-tags/1732454#1732454)
that actually works.

One thing Kleene's library can't handle is *nested* expressions.  Nested
parentheticals are beyond Kleene's algorithm to handle them because it
has no means of keeping track of how often the nesting has happened.  It
only recognizes letters as they're coming in and keeps no record of what
it's seen before.

## "Expression"

One loose breakdown in programming languages is between those that are
"statement oriented" and those that are "expression oriented."  In the C
programming language, for example, an *if statement* looks like this:
```
if (some_value) { y = 10; } else { y = 11; }
```

And an *if expression* looks like this:
```
y = (some_value) ? 10 : 11;
```

In the first, each arm of the statement can do something entirely
different.  In the second, the only thing the *expression* can do is
return a value, and because the destination is the same in both cases
those values must be of the same *type*.

Regular expressions are *expressions* because, once defined, each takes
a value and returns a value: it takes a string and returns the results
of recognizing (or failing to recognize) parts of that string.  They're
"pure expressions" in that they have no side effects, and can't do
anything other than the recognition and conversion they're programmed to
perform.

## Next Steps

We've reached 1960, the year when computer scientists and engineers
began grappling with turning source code into running programs in a
formal way, rather than the ad-hoc, hand-typed assembly language they'd
been using through the previous decade.  In the next chapter, we'll
discuss a little more about the history of regular expressions, and how
Kleene's mathematics were turned into the running programs we use today.



## Bibliography

HENGLEIN, Fritz. [*Kleen Meets Church: Regular Expressions as
Types*](http://www.cs.ox.ac.uk/ralf.hinze/WG2.8/27/slides/fritz.pdf)

