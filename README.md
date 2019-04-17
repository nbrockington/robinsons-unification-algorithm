# robinsonsUA

Haskell program to implement Robinson's Unification Algorithm for types

Based on University of Oxford, Department of Computer Science
lecture notes on "Lambda Calculus & Types" Postgraduate Course 2015-2016
(part of MSc in Mathematics and Foundations of Computer Science)
http://www.cs.ox.ac.uk/teaching/courses/2015-2016/lambda/

See also: https://en.wikipedia.org/wiki/Unification_(computer_science)#A_unification_algorithm

Accepts exactly two arguments of type 'String': *Main> controlFlow typeA typeB

If the input types are unifiable, returns the most general unifier U
and the most general unification of type A and type B.

If the input types are not unifiable, returns "Not unifiable"
and specifies the type variable and term that clash.

Example call: *Main> controlFlow "b -> b" "(a -> a) -> c"

Returns: ["Unifier: [(a -> a)/b, (a -> a)/c]","(a -> a) -> (a -> a)"]

Written by Nela Cicmil Brockington, April 2016, University of Oxford

Current known bugs: 

1. In general, does not recognise that nested arrows associate to the right and therefore requires parentheses in input types. 
For example: 
controlFlow "c -> c -> c" "b -> f" returns an error, 
whilst: 
control Flow "c -> (c -> c)" "b -> f" correctly returns ["Unifier: [b/c, (b -> b)/f]","b -> (b -> b)"].
