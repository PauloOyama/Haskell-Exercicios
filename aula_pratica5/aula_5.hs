type Data = (Int, Int,Int)
type Emprestimo = (String,String,Data,Data,String)
type Emprestimos = [Emprestimo]



--1)
--a

bissexto :: Int -> Bool
bissexto ano 
    | multiploDe400 = True
    | multiploDe4 = True
    | otherwise = False 
    where multiploDe400 = ( mod ano 400 == 0 )
          multiploDe4 = ( mod ano 4 == 0 ) && ( mod ano 100 /= 0)


valida  ::  Data -> Bool
valida  (dia, mes, ano)
    | diaInvalido = False 
    | fevNaoBissexto = False
    | fevBissexto = False
    | mes31Dias = False
    | mes30Dias = False
    | otherwise = True
    where  diaInvalido = dia < 1
           fevNaoBissexto = dia > 28 && mes == 2 && not (bissexto ano)
           fevBissexto = dia > 29 && mes == 2 && (bissexto ano) 
           mes31Dias = dia > 31 && (mes == 1 || mes == 3 || mes == 5 || mes == 7 || mes == 8 || mes == 10 || mes == 10 || mes == 12)
           mes30Dias = dia > 30 && ( mes == 4 || mes == 6 || mes == 9 ||mes == 11 )
           

--b

bissextos :: [Int] -> [Int]
bissextos list = [ x | x <- list, verificaBissexto x ]
    where verificaBissexto ano = bissexto ano



--c 

precede :: Data -> Data -> Bool
precede (dia1, mes1, ano1) (dia2, mes2, ano2)
    | comparaAnos = True
    | comparaMes = True
    | comparaDias = True
    | otherwise = False
    where comparaAnos =  ano1 < ano2 
          comparaMes =  ano1 == ano2 && mes1 < mes2
          comparaDias =  ano1 == ano2 && mes1 == mes2 && dia1 < dia2 


atrasados :: Emprestimos -> Data -> Emprestimos
atrasados emp hoje = [(a,b,c,dataEntrega,e) | (a,b,c,dataEntrega,e) <- emp, verificaPrecedencia dataEntrega hoje] 
    where verificaPrecedencia dataEntrega hoje = precede dataEntrega hoje  == True

-- d 

passo :: Int -> (Int,Int)
passo 1 = (1,1)
passo n = ( segundoTermo, primeiroTermo + segundoTermo)
    where primeiroTermo = fst (passo (n-1)) 
          segundoTermo = snd (passo (n-1))

fibo2 :: Int -> Int 
fibo2  1 = 1
fibo2 n =  primeiroPasso
    where primeiroPasso  = fst (passo n)

--E

prodIntervalo :: (Int,Int) -> Int
prodIntervalo (m,n) 
    | n == m = n
    | otherwise =  m * ( chamaRecursao (m+1) n )
    where chamaRecursao m n = prodIntervalo((m +1), n )


prodFatorial :: (Int,Int) -> Int
prodFatorial(m,_) = ( chamaIntervalo m )
    where chamaIntervalo m = prodIntervalo(1,m) 



--2)
--a

bissextoLET :: Int -> Bool
bissextoLET ano 
    | let multiploDe400 x = ( mod x 400 == 0 ) in multiploDe400 ano = True
    | let multiploDe4 x = ( mod x 4 == 0 ) && ( mod ano 100 /= 0) in multiploDe4 ano = True
    | otherwise = False 


validaLET  ::  Data -> Bool
validaLET  (dia, mes, ano)
    | let diaInvalido x = x < 1 in diaInvalido dia = False 
    | let fevNaoBissexto x y z = x > 28 && y == 2 && not (bissexto z) in fevNaoBissexto dia mes ano= False
    | let fevBissexto x y z  = x > 29 && y == 2 && (bissexto z) in fevBissexto dia mes ano= False
    | let mes31Dias x y = x > 31 && (y == 1 || y == 3 || y == 5 || y == 7 || y == 8 || y == 10 || y == 10 || y == 12) in mes31Dias dia mes = False
    | let mes30Dias x y = x > 30 && ( y == 4 || y == 6 || y == 9 ||y == 11 ) in mes30Dias dia mes = False
    | otherwise = True
    
           

--b

bissextosLET :: [Int] -> [Int]
bissextosLET list = [ x | x <- list, let verificaBissexto ano = bissexto ano in verificaBissexto x ]


--c 

precedeLET :: Data -> Data -> Bool
precedeLET (dia1, mes1, ano1) (dia2, mes2, ano2)
    | let comparaAnos x y = x < y in comparaAnos ano1 ano2 = True
    | let comparaMes x1 x2 y1 y2 = (x1 == y1) && (x2 < y2) in comparaMes ano1 mes1 ano2 mes2  = True
    | let comparaDias x1 x2 x3 y1 y2 y3 = (x1 == y1) && (x2 == y2) && (x3 < y3) in comparaDias ano1 mes1 dia1 ano2 mes2 dia2 = True
    | otherwise = False


atrasadosLET :: Emprestimos -> Data -> Emprestimos
atrasadosLET emp hoje = [(a,b,c,dataEntrega,e) | (a,b,c,dataEntrega,e) <- emp, verificaPrecedencia dataEntrega hoje] 
    where verificaPrecedencia dataEntrega hoje = precede dataEntrega hoje  == True

-- d 

passoLET :: Int -> (Int,Int)
passoLET 1 = (1,1)
passoLET n =  let subtraiN x = x - 1 in  ( snd (passo (subtraiN n)), fst (passo (subtraiN n)) + snd( passo(subtraiN n)))

fibo2LET :: Int -> Int 
fibo2LET  1 = 1
fibo2LET n =  let primeiroPasso x = fst (passo x) in primeiroPasso n

--E

prodIntervaloLET :: (Int,Int) -> Int
prodIntervaloLET (m,n) 
    | n == m = n
    | otherwise =  let chamaRecursao m n  = prodIntervaloLET((m +1), n ) in  m * ( chamaRecursao m n )


prodFatorialLET :: (Int,Int) -> Int
prodFatorialLET(m,_) = let chamaIntervalo m = prodIntervaloLET(1,m)  in   chamaIntervalo m 


--3)
--1)

--exp1 = (\x -> 2*x +1)3
-- exp1 = 2*3 +1

--2)
--exp2 = (\x y -> x - y) 5 7
-- exp2 = 5 - 7


--3)

--exp3 = (\y x -> x - y) 5 7
-- exp3 = 7 - 5

--4)

--exp4 = (\x y -> x-y) (\z -> (z/2) )
--exp4 = (\z -> (z/2) ) -y
--exp4 = -y/2

--5)
--exp5 =  (\x y -> x-y) ((\z -> (z/2)) 6) 1 
--exp5 =  ((\z -> (z/2)) 6) - 1 
--exp5 =  (6/2) -1


--6)

--exp6 = (\x -> \ y -> - ( x y) ) 9 4
--exp6 =  - (9 4)


--7)

--exp7 = (\x -> xx) (\y -> y)
--exp7 = (\y -> y)(\y -> y)
--exp7 = (\y -> y) 




--5)
--a)

a = (\x -> \y -> y)((\z -> z)(\z -> z))(\w -> w)5 

b = ((\f -> (\x -> f(f x)))(\y -> (y*y)))3
    
c = ((\f -> (\x -> f(f x)))(\y -> y+y))5

d = ((\x -> (\y -> x+y)5)((\y -> y-3)7))

e = (((\f -> (\x -> f(f(f x)))) (\y -> y*y))2)

f = (\x -> \y -> x + ((\x -> x - 3)y)) 5 6