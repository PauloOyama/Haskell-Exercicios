--Ex 1

paridade :: [Int] -> [Bool]
paridade l = map (even) l

--Ex2

prefixos :: [String] -> [String]
prefixos l = map (take 3) l

--Ex3

saudacao :: [String] -> [String]
saudacao l = map ("Oi " ++) l

--Ex4


filtrar :: ( a -> Bool) -> [a] -> [a]
filtrar f [] = [] 
filtrar f (x:xs) 
    | f x = x : filtrar f xs 
    | otherwise = filtrar f xs 

filtrarComp :: (a -> Bool) -> [a] -> [a] 
filtrarComp f xs = [ x | x <- xs, f x]


--Ex5

pares :: [Int] -> [Int]
pares lst = filter (even) lst


--Ex6

solucoes :: [Int] -> [Int]
solucoes l = filter (\x -> (5*x+6) < (x*x)) l 


--Ex7

maior :: Ord a => [a] -> a
maior l = foldr1 max l


--Ex8

menor_min10 :: [Int] -> Int
menor_min10 l = foldr min (10) l


--Ex9 

junta_silabasplural :: [String] -> String
junta_silabasplural l =   foldr (++) "s" l  


--Ex10

menores10 :: [Int] -> ([Int], Int)
menores10 l = let x = filter (<10) l in (x, length(x))

--Ex11

find :: Eq a => a -> [a] -> Int
find a [] = 0
find a (x:xs) | a == x = 1
              | otherwise = 1 + (find a xs)

              

busca_elem :: Eq a => a -> [a] -> (Bool,Int)
busca_elem  x l = ( elem x l, find True ( map ( x == ) l ) )  