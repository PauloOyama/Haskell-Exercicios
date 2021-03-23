-- Ex1

meOr1 :: Bool -> Bool -> Bool
meOr1 x y = if x == True
                then True
                else if (y == True) 
                    then True
                    else False

meOr2 :: Bool -> Bool -> Bool
meOr2 x y 
    | x == True = True  
    | y == True = True
    | otherwise = False

meOr3 :: Bool -> Bool -> Bool
meOr3 False False = False
meOr3 False  True = True
meOr3 True  False = True
meOr3 True  True = True

meOr4 :: Bool -> Bool -> Bool
meOr4 False False   = False
meOr4 _ _ = True

meOr5 :: Bool -> Bool -> Bool
meOr5 False b = b
meOr5 True  _ = True

-- --Ex 2
dist :: (Float,Float) -> (Float,Float) -> Float
dist (a1,b2) (a,b)  = sqrt( (a1- a)^2 + (b2 + b)^2 )

-- --Ex 3

--Guardas
fatorialG :: Integer -> Integer
fatorialG n 
    | n == 0 = 1
    | otherwise = n * fatorialG(n-1)

-- --Casamento de Padrao
fatorialC 0 = 1
fatorialC n = n * fatorialC(n -1)

--Ex 4
-- --Guardas
fiboG :: Int -> Int
fiboG n 
    | n == 0 = 0
    | n == 1 = 1
    | n == 2 = 1
    | otherwise = fiboG(n - 2) + fiboG(n - 1)

--Casamento de Padrão
fiboC 0 = 0
fiboC 1 = 1
fiboC 2 = 1
fiboC n = fiboC(n - 2) + fiboC(n -1)

--Ex 5
n_tri :: Int -> Int
n_tri 0 = 0
n_tri n = n + n_tri(n-1)

--Ex 6
potencia2 :: Int -> Int
potencia2 1 = 2
potencia2 n = potencia2(n-1)*2 

--Ex 7 
--a
prodIntervalo :: (Int,Int) -> Int
prodIntervalo (m,n) 
    | n == m = n
    | otherwise =  m * prodIntervalo((m +1), n )

--b 
prodFatorial :: (Int,Int) -> Int
prodFatorial(m,_) = prodIntervalo(1,m)

--Ex 8 
division :: (Int,Int) -> (Int,Int)
division(m,n) = (restoDiv(m,n), divInteira(m,n))

divInteira :: (Int,Int) -> Int
divInteira(m,   n)
    | m < n = 0
    | otherwise = divInteira((m-n),n) + 1


restoDiv :: (Int,Int) -> Int
restoDiv(m,n)
    | m <= n = m
    | otherwise = restoDiv((m - n), n)

--Ex 9

--Guardas
mdcg :: (Int,Int) -> Int
mdcg (m,n)
    | n == 0 = m
    | otherwise = mdcg( n,(mod m n ))

--Casamento de padrão
mdcC :: (Int,Int) -> Int
mdcC (m,0) = m
mdcC (m,n) = mdcC(n,(mod m n))

--Ex 10
binog :: (Int,Int) -> Int
binog(n,k) 
    | k == 0 = 1
    | k == n = 1
    | otherwise = binog(n-1,k) + binog(n-1,k-1)

binoc :: (Int,Int) -> Int
binoc(n,0) = 1
binoc(n,k) = if(k ==n) then 1
                else binoc(n-1,k) + binoc(n-1,k-1)

--Ex 11
passo :: Int -> (Int,Int)
passo 1 = (1,1)
passo n = ( snd (passo (n-1)), fst (passo (n-1)) + snd( passo(n-1)))

fibo2 :: Int -> Int 
fibo2  1 = 1
fibo2 n =  fst (passo n)

