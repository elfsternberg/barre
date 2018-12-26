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


