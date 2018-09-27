# BARRE - A Regular Expression Engine

This library implements a regular-expression-like engine using
Brzozoski's "parsing with derivatives" algorithm rather than the
traditional DFA/NFA arrangement.  Right now, this is simply a
*recognizer*, in that it acknowledges that a string matches a regular
expression, return true or false.  It doesn't do anything special beyond
that.

"BARRE" stands for Brzozoski/Antimirov Regular Expressions.

Right now I haven't done anything with [Antimirov's algorithm](https://pdfs.semanticscholar.org/8b0e/ef83c7f93884a7edcb7d46519879a8dde564.pdf).  

## Acknowledgements

Matt Might has implemented parsing with derivatives in his blog post
[Yacc Is Dead](http://matt.might.net/articles/parsing-with-derivatives/)
and subsequent research, in a variety of languages including Racket,
Scala, and Haskell.  You'll note that all of those are functional,
garbage-collected languages.  The best implementation, in the sense of
being the most "readable", is the
[Racket Implementation](https://github.com/plum-umd/parsing-with-derivatives/tree/master/racket-code/racket)
at the Programming Languages at University of Maryland program, and it's
what I've been using as an example, although my first draft is line-for
line a reimplementation of my
[Python example](https://elfsternberg.com/2018/01/24/parsing-derivatives-naive-python-edition/).

As Rust is famously hostile to traditional tree structures, I decided to
use Rust Leipzig's
[Idiomatic Trees in Rust](https://rust-leipzig.github.io/architecture/2016/12/20/idiomatic-trees-in-rust/)
example, although my use case is much smaller and simpler than his and
so I've gone and done it my way.

## Status

Right now the smallest, most primitive possible engine is working.
`Token`, `Alt`, `Cat`, and `Repeat` are implemented.  No optimizations have
been applied.

## Description

BARRE implements a regular expression library that implements the same
primitives as the Rustlang Regular Expression library.  The underlying
library consists of the following primitives:

```
Empty        Matches nothing
Epsilon      Matches the empty string
Token(c)     Matches a single character
Any(c)       Matches any character
Alt(n, n)    Alternate
Cat(n, n)    Concatenation
Repeat(n)    Repeat
Cap(n)       Defines a capture group
Back(i)      Defines a backreference
```

With these, the entire universe of regular expressions can be described.
For example, the expression `(a|b)` is encoded as `Alt(Char('a'),
Char('b'))`, and `(a|b)*` is `Repeat(Alt(Char('a'), Char('b')))`.

### Macros

A macro library is provided.  The above examples could be written as:

`alt['a', 'b']`

and

`rep[alt['a', 'b']]`

### Traditional Regexp language

The traditional regular expression language is handled by a parser
written in itself (naturally).  It supports the following constructs:

### Matching one character
```
.             any character except new line (includes new line with s flag)
\d            digit (\p{Nd})
\D            not digit
\w            word character
\W            non-word character
\s            whitespace
\S            not-whitespace
```

### Character classes
```
[xyz]         A character class matching either x, y or z (union).
[^xyz]        A character class matching any character except x, y and z.
[a-z]         A character class matching any character in range a-z.
[[:alpha:]]   ASCII character class ([A-Za-z])
[[:^alpha:]]  Negated ASCII character class ([^A-Za-z])
[x[^xyz]]     Nested/grouping character class (matching any character except y and z)
[a-y&&xyz]    Intersection (matching x or y)
[0-9&&[^4]]   Subtraction using intersection and negation (matching 0-9 except 4)
[0-9--4]      Direct subtraction (matching 0-9 except 4)
[a-g~~b-h]    Symmetric difference (matching `a` and `h` only)
[\[\]]        Escaping in character classes (matching [ or ])
```

### Composite Groups
```
xy     Concatenation
(x|y)  Alternation
```

### Repetitions
```
x*        zero or more of x
x+        one or more of x
x?        zero or one of x
x{n,m}    at least n x and at most m x
x{n,}     at least n x
x{n}      exactly n x
x{n}?     exactly n x
```

### Captured Groups
```
(exp)     Capture group returned in a Vec
```

### Backreferences
```
\1        the matched text (*not* the expression) of the first captured group
```

Note that this does *not* backtrack; that is, if more than one
expression in the capture group could possibly match the string
encountered, only the first valid match will be considered for a later
capture group backreference.

## Theory

I'm going to use the word "char" throughout to discuss the symbols in a
string that one can regex.  It isn't necessary true, and there are more
advanced uses, but for now, we're going to stick with what we know:
regular expressions parse strings of characters.

### Traditional Regular Expressions

In a traditional regular expression engine there is a graph, a bunch of
*nodes* connected by *edges*.  There's a starting node.  For the first
character, we check to see if the starting node recognizes it.  If it
doesn't, the match fails.  If it does, we move to another node, and
check the second character, and so forth.  It's possible that the node
we move to is the *same* node, as is the case when you have, say, `A*`,
the repetition operator.  Eventually, you reach a *terminal* node, which
means that you've reached the end of the regular expression, and the
string matches.

In the basic, traditional regular expression engine, there are only a
few kinds of nodes: Alternative (the `|` operator), Repetition (the `*`
operator), Concatenation, and Char. In this system, "ABC" is
`Concatenation(Char('A'), Char('B'), Char('C'))`.  The problem with
traditional regular expression engines is that if the Alternative node
fails somewhere down the line, the system has to backtrack to the
Alternative node and take a different edge out until it gets a match or
all alternatives fail.

### Parsing With Derivatives

In parsing with derivatives, every node has a *derivative*, which is
a regular expression that will match *the rest of the string* after a
character has been analyzed.  So if you have a string `ABC` and a
recognizer `ABC`, after `A` has been analyzed you have a new analyzer,
`BC`, which is ready to analyze the rest of the string.

When you put in repetition and alternatives, though, things get messy.
The good news is that, when you pass a character to an alternative,
because the derivative of an alternative contains all possible future
alternatives given the character just processed, there's no need for
backtracking, just a pruning of possible continuations until one
alternate is exhausted, or none succeed and the match fails.

The bad news is that even a few simple expressions could blossomm into
dozens or even hundreds of nodes.  The performance is currently
terrible, too.  There are specializations that can speed it up, and I
hope to get to them.


The basics of the algorithm, as shown in my Python implementation, are
described in mathematical notation:

D(∅) = ∅
D(ε) = ∅
D(c) = ε if c = c'
D(c') = ∅ if c ≠ c'
D(p s) = N(p) D(s) | D(p) s
D(l | r) = D(1) | Dc(r)
D(e*) = D(e) e*

N(∅) = ∅
N(ε) = ε
N(c) = ∅
N(p s) = N(p) N(s)
N(l | r) = N(l) | N(r)
N(e*) = ε

First, the most basic recognizers are the null ∅, which recognizes no
strings; epsilon ε, which recognizes only the empty string, and c, which
recognizes a single character.  The rules of derivatives, D, say that
trying to derive null or epsilon results in null, deriving c returns
epsilon if it matches the candidate, otherwise null.  The derivative of
an alternation is the alternation of all the derivatives of all the
expressions being alternated, the derivative of repetition is the
derivative of the thing being repeated concatenated with the original,
and concatenation is... weird.

Concatenation has a prefix-- the next character, and a suffix--
everything that follows.  If the prefix cannot return null, the
derivative is the concatenation of the derivative of the prefix with the
suffix, and if it can, it's the alternative between that and the
derivative of the suffix.

There are excellent papers on the subject.  I recommend
[Klipse's](https://blog.klipse.tech/clojure/2016/10/02/parsing-with-derivatives-regular.html)
for more detail.  

## Status

Not really working.  The Repeat operator is broken, and I'm not sure
why.

### ToDo

* Fix Repeat()
* Implement Any()
* Implement character ranges
* Implement capture groups
** Implement backreferences
* Implement macros
* Implement higher-level tokenization
** Provide for Unicode and Char specalizations
* Implement recursion
** Implement combinators
** Implement regex parser

## NO WARRANTY GRANTED OR IMPLIED

Copyright (C) 2018 Elf M. Sternberg

Regex-Derivatives is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with this program; if not, write to the Free Software Foundation,
Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

	- Elf M. Sternberg <elf@pendorwright.com>


