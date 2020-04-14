# robinsons-unification-algorithm

Haskell program to implement Robinson's Unification Algorithm for types

Based on University of Oxford, Department of Computer Science
lecture notes on "Lambda Calculus & Types" Postgraduate Course 2015-2016
(part of MSc in Mathematics and Foundations of Computer Science)
http://www.cs.ox.ac.uk/teaching/courses/2015-2016/lambda/

See also: https://en.wikipedia.org/wiki/Unification_(computer_science)#A_unification_algorithm

Written April 2016, University of Oxford


# Usage

Accepts exactly two arguments of type 'String': `*Main> unifyTypes typeA typeB`

N.B. For input types, outer parentheses are assumed omitted; inner	parentheses must be present, i.e.,	association of nested arrows to	the right is not assumed.

If the input types are unifiable, returns the most general unifier U
and the most general unification of type A and type B.

If the input types are not unifiable, program returns "Not unifiable"
and specifies the type variables and terms that clash.

Example call: `*Main> unifyTypes "b -> b" "(a -> a) -> c"`

Returns: `["Unifier: [(a -> a)/b, (a -> a)/c]","(a -> a) -> (a -> a)"]`


# Current known issues

1. In general, does not recognise that nested arrows associate to the right and therefore requires parentheses in input types. 
For example: 
`unifyTypes "c -> c -> c" "b -> f"` returns an error, 
whilst: 
`unifyTypes "c -> (c -> c)" "b -> f"` correctly returns `["Unifier: [b/c, (b -> b)/f]","b -> (b -> b)"]`
