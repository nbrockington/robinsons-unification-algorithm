-- |Haskell program to implement Robinson's Unification Algorithm for types
--
-- Based on University of Oxford, Department of Computer Science 
-- lecture notes on "Lambda Calculus & Types" Postgraduate Course 2015-2016
-- (part of MSc in Mathematics and Foundations of Computer Science)
-- http://www.cs.ox.ac.uk/teaching/courses/2015-2016/lambda/
-- [See also: https://en.wikipedia.org/wiki/Unification_(computer_science)#A_unification_algorithm]
--
-- Accepts exactly two arguments of type 'String': *Main> unifyTypes typeA typeB
--
-- N.B. For input types, outer parentheses are assumed omitted; inner parentheses must be present
-- i.e. association of nested arrows to the right is not assumed. 
--
-- If the input types are unifiable, returns the most general unifier (m.g.u.) U
-- and the most general unification of type A and type B, i.e., U(typeA).
--
-- If the input types are not unifiable, returns "Not unifiable"
-- and specifies the type variable and term that clash.
-- 
-- Example call: *Main> unifyTypes "b -> b" "(a -> a) -> c"
--
-- Returns: ["Unifier: [(a -> a)/b, (a -> a)/c]","(a -> a) -> (a -> a)"]
--
-- Written by Nela Cicmil Brockington, April 2016, University of Oxford



-- | The 'unifyTypes' function controls the program's input and output.
-- It passes the input strings typeA and typeB to the function 'mainAlg'
-- and prints the output of 'mainAlg' to the command line as a list of strings:
-- "Unifier: ... " followed by the most general unification if m.g.u. is found, or, 
-- "Not unifiable: ...", with explanation, if m.g.u. cannot be found. 

unifyTypes :: String -> String -> [ String ]
unifyTypes typeA typeB 
 | head a == 'U'    = [ a , b ] 
 | otherwise        = [ a , b , c , d , e ]
   where ( a , b , c , d , e ) = mainAlg ( "[]" , typeA , typeB , typeA , typeB )



-- | The 'mainAlg' function designates whether types are unifiable or not.
-- It checks whether the types are not unifiable, and terminates the process if so;
-- it checks whether the substituted types are equal, and terminates the process if so;
-- or calls 'findDiff' to find the next difference in the type strings. 

mainAlg :: ( String , String , String , String , String ) -> ( String , String , String , String , String )
mainAlg ( unifier , uA , uB , utypeA , utypeB )
 | unifier == "Not unifiable: "         = ( "Not unifiable: " , uA , uB , " in " ++ utypeA , utypeB )
 | utypeA == utypeB                     = ( "Unifier: "++ unifier , uA , uB , "" , "" )
 | otherwise                            = mainAlg ( findDiff ( unifier , utypeA , utypeB , utypeA , utypeB ) )



-- | The 'findDiff' function finds the first character where the strings of typeA and typeB differ,
-- possibly after one or more unifying type substitutions have already taken place.
-- It reports failure of unification if the strings are not identical and at the point at which they differ
-- the type variable of one is present in the type of the other. 
-- Otherwise, it requests the appropriate substitution be made. 

findDiff :: ( String , String , String , String , String ) -> ( String , String , String , String , String )
findDiff ( unifier , c:xs , d:ys , utypeA , utypeB ) 
 | c /= d && c == '(' && d `elem` subTypec = ( "Not unifiable: " , subTypec , "clashes with " ++ [d] , utypeA , utypeB )
 | c /= d && d == '(' && c `elem` subTyped = ( "Not unifiable: " , [c] , "clashes with " ++ subTyped , utypeA , utypeB )
 | c /= d && c /= '(' && d /= '('          = makeSubst ( incUnifier ( unifier , [d] , [c] ) , [d] , [c] , utypeA , utypeB )
 | c /= d && c == '('                      = makeSubst ( incUnifier ( unifier , subTypec , [d] ) , subTypec , [d] , utypeA , utypeB )
 | c /= d && d == '('                      = makeSubst ( incUnifier ( unifier , subTyped , [c] ) , subTyped , [c] , utypeA , utypeB )
 | otherwise                               = findDiff ( unifier , xs , ys , utypeA , utypeB )
   where subTypec = returnSubType ( c:xs )
         subTyped = returnSubType ( d:ys )



-- | The 'returnSubType' function returns the first type of a composite type input i.e. returns A from A -> B
-- For a single type variable input, it returns the type variable. 
-- It finds the subtype by reading the string left to right to find the first set of closed parentheses.
-- N.B. Outer parentheses are assumed to be omitted, inner parentheses are assumed to be present. 

returnSubType :: String -> String
returnSubType ( y:ys ) 
 | y == '('         = take l ( y:ys )
 | otherwise        = [y]
   where l = length ( takeWhile ( >0 ) ( scanl ( \b x -> if x == '(' then b + 1 else if x == ')' then b - 1 else b ) 1 ys ) ) + 1



-- | The 'makeSubst' function takes in unifer information, a type typeC, a type variable cvar, and two type strings,
-- and calls 'substiTute' to replace cvar with typeC in both type strings

makeSubst :: ( String , String , String , String , String ) -> ( String , String , String , String , String )
makeSubst ( unifier , typeC , cvar , utypeA , utypeB ) = ( unifier , nextsubA , nextsubB , nextsubA , nextsubB )
   where nextsubA = substiTute ( typeC , cvar , utypeA ) 
         nextsubB = substiTute ( typeC , cvar , utypeB )



-- | The 'substiTute' function takes in type typeC, type variable cvar, and an input type,
-- in which it replaces all instances of type variable cvar with type typeC.

substiTute :: ( String , String , String ) -> String
substiTute ( typeC , cvar , utype ) = foldl (\acc x -> if x `elem` cvar then acc ++ typeC else acc ++ [x]) [] utype



-- | The 'incUnifier' function takes in the unifier (m.g.u.) constructed so far, a type typeC, and type variable cvar,
-- and appends a new component typeC/cvar to the unifier substitution. 

incUnifier :: ( String , String , String ) -> String
incUnifier ( unifier , typeC , cvar ) 
 | init unifier == "["       = ( init unifier ) ++ typeC ++ "/" ++ cvar ++ "]"
 | otherwise                 = ( init unifier ) ++ ", " ++  typeC ++ "/" ++ cvar ++ "]"
