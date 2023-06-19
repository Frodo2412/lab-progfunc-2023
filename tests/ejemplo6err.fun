foo :: (Int,Int) -> Int
foo(x,y) = let w :: Bool = if (x==0) then 0 else False   
           in let z :: Int = 4
              in if (x==1) 
                    then if w then x==y else x 
                    else x+y

main = foo(2,4)

Expected: Int Actual: Bool -- el let externo es de tipo Bool y foo retorna Int
Expected: Bool Actual: Int -- if (x==0)... es de tipo Int y w de tipo Bool
Expected: Int Actual: Bool -- False aparece donde se espera un Int 
Expected: Bool Actual: Int -- se esper un Bool en else de if w...
Expected: Bool Actual: Int -- se espera un Bool en else de if (x==1)...

