suma :: (Int,Int) -> Int
suma (x,y) = x+y  

foo :: (Int,Int) -> Int
foo(x,y) = let w :: Bool = suma(x,True) == False    
           in True == (False==suma(False))

main = foo(2,4)

Expected: Int Actual: Bool -- el let es de tipo Bool pero foo retorna Int 
Expected: Int Actual: Bool -- True en suma(x,True), se espera un Int
Expected: Int Actual: Bool -- False en ...==False, se espera un Int
Expected: Bool Actual: Int -- suma(False) aparece donde se espera un Bool
The number of arguments in the application of: suma doesn't match the signature (1 vs 2)
Expected: Int Actual: Bool -- False en la aplicación suma(False), se espera un Int
