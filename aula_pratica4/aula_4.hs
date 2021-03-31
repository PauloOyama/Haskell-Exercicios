--1)
conta_ch :: [Char] -> Int
conta_ch [] = 0
conta_ch (x:res) =  1 + conta_ch res

conta :: [t] -> Int
conta [] = 0
conta (_:r) = 1 + conta r

maior :: [Int] -> Int
maior [x] = x
maior (x:y:resto)
    |  x > y = maior(x:resto)
    |  otherwise = maior (y:resto)

primeiros :: Int -> [t] -> [t]
primeiros 0 _ = []
primeiros _ [] = []
primeiros n (x:xs) = x:primeiros ( n - 1 ) xs

pertence :: Eq t => t -> [t] -> Bool
pertence a [] = False
pertence a (x:z) = if(a == x) then True
                              else pertence a z


uniao :: Eq t => [t] -> [t] -> [t]
uniao as bs = as ++ [ b | b <- bs, not ( pertence b as)]

--2)

npares :: [Int] -> Int 
npares [] = 0
npares (x:resto) =  if( mod x 2 == 0 ) then ( 1 + npares resto)
                                        else ( 0 + npares resto)

--3)

produtorio :: [Int] -> Int
produtorio [x] = x
produtorio(x:resto) = x * produtorio resto

--4)

comprime :: [[Int]] -> [Int]
comprime [] = [] 
comprime (x:resto) =  x ++ comprime resto

--5)

tamanho :: [t] -> Int
tamanho [] = 0
tamanho (x:resto) = 1 + tamanho resto

--6)

uniaoRec2 :: Eq t => [t] -> [t] -> [t]
uniaoRec2 _ [] = []
uniaoRec2 primeiro segundo = primeiro ++ [ x | x <- (segundo), elem x primeiro == False] 


 