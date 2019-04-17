-- |Haskell program to implement Robinson's Unification Algorithm for types
--
-- Based on University of Oxford, Department of Computer Science 
-- lecture notes on "Lambda Calculus & Types" Postgraduate Course 2015-2016
-- (part of MSc in Mathematics and Foundations of Computer Science)
-- http://www.cs.ox.ac.uk/teaching/courses/2015-2016/lambda/
-- [See also: https://en.wikipedia.org/wiki/Unification_(computer_science)#A_unification_algorithm]
--
-- Accepts exactly two arguments of type 'String': *Main> controlFlow typeA typeB
--
-- If the input types are unifiable, returns the most general unifier U
-- and the most general unification of type A and type B [U(typeA) _=  U(typeB)].
--
-- If the input types are not unifiable, returns "Not unifiable"
-- and specifies the type variable and term that clash.
-- 
-- Example call: *Main> controlFlow "b -> b" "(a -> a) -> c"
--
-- Returns: ["Unifier: [(a -> a)/b, (a -> a)/c]","(a -> a) -> (a -> a)"]
--
-- Written by Nela Cicmil Brockington, April 2016, University of Oxford


controlFlow :: String -> String -> [ String ]
controlFlow typeA typeB 
 | head a == 'U' = [ a , b ]
 | otherwise        = [ a , b , c , d , e ]
   where ( a , b , c , d , e ) = realAlg ( "[]" , typeA , typeB , typeA , typeB )


realAlg :: ( String , String , String , String , String ) -> ( String , String , String , String , String )
realAlg ( uk, ukA , ukB , uktypeA , uktypeB )
 | uk == "Not unifiable: "         = ( "Not unifiable: " , ukA , ukB , " in " ++ uktypeA , uktypeB )
 | uktypeA == uktypeB              = ( "Unifier: "++ uk , ukA , ukB , "" , "" )
 | otherwise  = realAlg ( findDiff ( uk , uktypeA , uktypeB , uktypeA , uktypeB ) )


findDiff :: ( String , String , String , String , String ) -> ( String , String , String , String , String )
findDiff ( uk, c:xs , d:ys , uktypeA , uktypeB )
 | c /= d && c == '(' && d `elem` subTypec = ( "Not unifiable: " , subTypec , "clashes with " ++ [d] , uktypeA , uktypeB )
 | c /= d && d == '(' && c `elem` subTyped = ( "Not unifiable: " , [c] , "clashes with " ++ subTyped , uktypeA , uktypeB )
 | c /= d && c /= '(' && d /= '('          = makeSubst ( incUnifier ( uk , [d] , [c] ) , [d] , [c] , uktypeA , uktypeB )
 | c /= d && c == '('                      = makeSubst ( incUnifier ( uk , subTypec , [d] ) , subTypec , [d] , uktypeA , uktypeB )
 | c /= d && d == '('                      = makeSubst ( incUnifier ( uk , subTyped , [c] ) , subTyped , [c] , uktypeA , uktypeB )
 | otherwise = findDiff ( uk , xs , ys , uktypeA , uktypeB )
   where subTypec = returnSubType ( c:xs )
         subTyped = returnSubType ( d:ys )


returnSubType :: String -> String
returnSubType ( y:ys ) 
 | y == '('  = take l ( y:ys )
 | otherwise = [y]
   where l = length ( takeWhile ( >0 ) ( scanl (\b x -> if x == '(' then b + 1 else if x == ')' then b - 1 else b ) 1 ys ) ) + 1


makeSubst :: ( String , String , String , String , String ) -> ( String , String , String , String , String )
makeSubst ( uk , typeC , cvar , uktypeA , uktypeB ) = ( uk , ukplus1A , ukplus1B , ukplus1A , ukplus1B )
   where ukplus1A = substiTute ( typeC , cvar , uktypeA ) 
         ukplus1B = substiTute ( typeC , cvar , uktypeB )


substiTute :: ( String , String , String ) -> String
substiTute ( typeC , cvar , uktype ) = foldl (\acc x -> if x `elem` cvar then acc ++ typeC else acc ++ [x]) [] uktype


incUnifier :: ( String , String , String ) -> String
incUnifier ( uk , typeC , cvar ) 
 | init uk == "[" = ( init uk ) ++ typeC ++ "/" ++ cvar ++ "]"
 | otherwise      = ( init uk ) ++ ", " ++  typeC ++ "/" ++ cvar ++ "]"










