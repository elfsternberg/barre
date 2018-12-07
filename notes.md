Adams, et al (2016):

On its own, memoization does not prevent infinite loops due to cycles,
however, because derive adds memoization table entries only after it
finishes computing. This is where a second measure comes into
play. *Before doing any recursive calls, derive puts a partially
constructed grammar node that is missing its children into the
memoization table.* For the example of Dc (L), we know without having to
recur into L’s children that the resultant node is a ∪ . Thus, we can
place such a node in the memoization table before computing its children
and temporarily mark its children as unknown. Any recursive calls to Dc
(L) can find and use this memoized result even though the derivatives of
its children have not yet been calculated. When the derivatives for the
node’s children return, we update the children of the output node to
point to the results of those derivatives.

Like with the derivative, computing nullability must also deal with
cycles in grammars. However, memoization alone is not sufficient here. A
cycle means the derivative of some node must point to one of its
ancestors. With nullability, though, we must not only compute the
nullability of an ancestor but also inspect its value so we can compute
the nullability of the current node. This turns nullability into a least
fixed point problem over the lattice of booleans.

---

An efficient method of computing the fixed point of the nullability of a
derivative uses ideas from data-flow analysis (Kildall 1973) and tracks
which nodes depend on which others. When the computed nullability of a
node changes, only those nodes that depend on that node are revisited.

While the tracking of dependencies does incur an overhead, we can
minimize this by tracking dependencies only after discovering cycles
that prevent us from immediately computing the result. In other cases,
we directly compute nullability with a simple recursive traversal.  We
can further improve the performance of nullable?  by distinguishing
between nodes that are definitely not nullable and those that are merely
assumed to be not nullable because the fixed point has not yet shown
them to be nullable.

Assumed-not-nullable and definitely-not-nullable nodes behave almost
exactly alike except that we may re-traverse assumed-not-nullable nodes
but never re-traverse definitelynot-nullable nodes. This is because
definitely-not-nullable nodes have their final value, while
assumed-not-nullable nodes might not.

---

Taxonomy of Regex Dialects: 
* BRE (POSIX 1003.2, Section 2.8 regular expressions)
* ERE (GNU Extended Regular Expressions)
* EMACS (Whatever the hell is in Emacs)
* PCRE (Perl Compatible Regular Expressions)
* PSIX (Perl Six Regular Expressions)
* ROSIE (The Rosie Expression Language)

Given what I now know about regular expression construction, I very much
like Perl 6's approach to language construction.  It does mean that
we'll need 'Barre | Barre' and 'Barre + Barre' operators (union and
sequencing), but that doesn't seem impossible.
