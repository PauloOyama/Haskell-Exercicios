--Ex 2 
--a
ex2a = [1..5]

--b
ex2b = ['a','c'..'e']

--c
ex2c = [1,4..16]

--d
aux1 = [1,-2..(-11)]
aux2 = [1,5..17]
ex2d = zip aux1 aux2

--Ex 3
--a
intervaloAB :: Int -> Int -> [Int]
intervaloAB a b 
    | a == b = [a]
    | a > b = []
    | otherwise = [a..(b)]

--b
intervaloAB2 :: Int -> Int -> [Int]
intervaloAB2 a b 
    | a == b = []
    | a > b = []
    | otherwise = [x | x <- [a..(b)], even x]

--Ex 5
quadrados :: Int -> Int -> [Int]
quadrados a b = [x^2 | x <- [a..(b)]]

--Ex 6
seleciona_impares :: [Int] -> [Int]
seleciona_impares arg = [x | x <- arg, odd x]

--Ex 7
tabuada :: Int -> [Int]
tabuada num = [num * x | x <- [1..10]]

--Ex 8 
auxBi :: Int -> Bool
auxBi ano 
    | ( mod ano 400 == 0 ) = True
    | ( mod ano 4 == 0 ) && ( mod ano 100 /= 0) = True
    | otherwise = False 

bissextos :: [Int] -> [Int]
bissextos bi = [x | x <- bi, auxBi(x) == True]

--Ex 9 
sublistas :: [[Int]] -> [Int]
sublistas list = [x | y <- list, x <- y]
    
--Ex 10 
type Data = (Int, Int, Int)
type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]
bdEmprestimo::Emprestimos
bdEmprestimo = [("H123C9","BSI945",(12,9,2009),(20,09,2009),"aberto"),
    ("L433C5","BCC021",(01,9,2009),(10,09,2009),"encerrado"),
    ("M654C3","BCC008",(04,9,2009),(15,09,2009),"aberto")]

precede :: Data -> Data -> Bool
precede (d1, m1, y1) (d2, m2, y2)
 | y1 > y2 = False
 | y1 == y2 && m1 > m2 = False
 | y1 == y2 && m1 == m2 && d1 > d2 = False
 | otherwise = True

returnData :: Emprestimo -> Data    
returnData (_,_,_,d,_) = d;

--Ex 11 

atrasados :: Emprestimos -> Data -> Emprestimos
atrasados emp hoje = [(a,b,c,dataEntrega,e) | (a,b,c,dataEntrega,e) <- emp, precede dataEntrega hoje  == True]

uniaoNRec :: [Int] -> [Int] -> [Int]
uniaoNRec listA listB =  listA ++ [ indexListB | indexListB <- listB, elem indexListB listA == False]