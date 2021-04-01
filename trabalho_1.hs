--Ex 1

analisa_raizes :: Int -> Int -> Int -> [Char]
analisa_raizes a b c 
    | a == 0 = "4-equacao degenerada"
    | b^2 > 4*a*c = "1-possui duas raizes reais"
    | b^2 == 4*a*c = "2-possui uma raiz real"
    | otherwise = "3-nenhuma raiz real"

-- Ex 2

raizEquacao1 :: Double -> Double -> Double 
raizEquacao1 b c = -c/b

raizEquacao2 :: Double -> Double -> Double -> (Double, Double)
raizEquacao2 a b c = ( ( -b + (delta a b c) ) / 2*a , ( -b - (delta a b c) ) / 2*a )

delta :: Double -> Double -> Double -> Double
delta a b c = sqrt(b*b - 4*a*c) 

equacao :: Double -> Double -> Double -> (Double, Double)
equacao a b c 
    | a == 0 = (raizEquacao1 b c, a)
    | otherwise = raizEquacao2 a b c

--Negative Parameter (-4) (-c)

-- Ex 3

type Data = (Int, Int, Int)

idade :: Data -> Data -> Int
idade (dAtual,mAtual,aAtual) (dNas,mNasc,aNasc)
    | mAtual > mNasc = (aAtual - aNasc)
    | (mAtual == mNasc) && (dAtual>= dNas) = (aAtual - aNasc )
    | otherwise= (aAtual - aNasc - 1)

passagem_onibus :: Double -> Data -> Data -> Double
passagem_onibus valor dataAtual dataNascimento
    | idade dataAtual dataNascimento < 2 = valor * 0.15
    | idade dataAtual dataNascimento <= 10 = valor * 0.40 
    | idade dataAtual dataNascimento >= 70 = valor * 0.50 
    | otherwise = valor 


-- Ex 4

listINTEIROS = [1..20]

--a)

gera1 = [x^3|x <- listINTEIROS, even x, x > 3 , x < 11]

--b)

gera2 = [(x,y) | x <- listINTEIROS, x <= 5, y <- [x..3*x]]

--c)

listaL1=[15,16]

gera3 = [ y | x <- listaL1 , y<- listINTEIROS, y<=x ]

--d)

--(11) ? 
gera4 = [(x,x+1) | x <- listINTEIROS, x <= 10 ]

--e)

gera5 = [ fst x + snd x | x <- gera4 ]

--5)

--a)

contaNegM2 :: [Int] -> Int
contaNegM2 lista = sum [ 1 | x <- lista, mod x 3 == 0, x > 0 ]

--b)

listaNegM2 :: [Int] -> [Int]
listaNegM2 lista = [ x | x <- lista, mod x 3 == 0, x > 0 ]

--6)

fatores :: Int -> [Int]
fatores num = [ y | y <- [1..num], mod num y == 0 ]

primos :: Int -> Int -> [Int]
primos numA numB = [ x | x <- [numA..numB], length ( fatores x) == 2 ]

--7)

mmc :: Int -> Int -> Int -> Int 
mmc a b c = head [ x | x <- [1..(a*b*c)], ( mod x a == 0 ) && (mod x b == 0 ) && (mod x c == 0)]

--8)

listaSerie :: Int -> Int -> [Double]
listaSerie x n = [fromIntegral y / fromIntegral x | y  <- [1..n], odd y == True] ++  [fromIntegral x / fromIntegral y | y  <- [1..n], even y == True] 

somaSerie :: Int -> Int -> Double
somaSerie x n =  sum (listaSerie x n) 

--9)

fizzbuzz :: Int -> [[Char]]
fizzbuzz n = [
            if(mod x 2 /= 0 && mod x 3 /= 0)
                then "No" 
                else if(mod x 2 == 0 && mod x 3 == 0)
                    then "FizzBuzz"
                    else if(mod x 2 == 0)
                        then "Fizz"
                        else "Buzz"
            | x <- [1..n] ]

--10)

seleciona_multiplos :: Int -> [Int] -> [Int]
seleciona_multiplos n listaInteiros = [ x | x <- listaInteiros, mod x n == 0]

--11)

unica_ocorrencia :: Int -> [Int] -> Bool
unica_ocorrencia n lista =  length [ x | x <- lista, x==n ] == 1

--12)

intercala :: [t] -> [t] -> [t] 
intercala [] [] = []
intercala listaMaiorx [] = listaMaiorx
intercala [] listaMaiory  = listaMaiory
intercala (x:restox) (y:restoy) = x:( intercala (y:restoy) restox )  

--13)

zipar :: [t] -> [t] -> [[t]]
zipar [] _ = []
zipar _ [] = []
zipar (x:restox) (y:restoy) = [x,y]:(zipar restox restoy )

--14)

type Contatos = ([Char],[Char],Int,[Char])

agendaPessoal :: [Contatos]
agendaPessoal = [("Roberta","Minas Gerais",3496545789,"roberta_01@email.com"),
    ("Marcelo","Rio Grande do Sul",105456876546,"marcelo_d1@gmail.com"),
    ("Davi","São Paulo",21998654321,"davi_alves@outlook.com"),
    ("Daniela","Rio de Janeiro",45687915463,"daniela4503@gmail.com")] 

retornaNome :: Contatos -> [Char]
retornaNome (nome,b,c,d) = nome

retornaEmail :: Contatos -> [Char]
retornaEmail (a,b,c,email) = email

achaContato :: [Char] -> [Contatos] -> [Char]
achaContato _ [] = "Email desconhecido"   
achaContato email (head:tale) = if(email == (retornaEmail head) ) then  (retornaNome head)
                                                  else  (achaContato email tale)

--15)


--16)

insere_ord :: Ord t => t -> [t] -> [t]
insere_ord n [] = [n]
insere_ord n (x:resto) = if(n > x ) then x:(insere_ord n resto)
                                    else if(n < x) then ([n] ++ (x:resto))
                                                   else (x:resto)


--17)

reverte :: [t] -> [t]
reverte [] = []
reverte (x:resto) = reverte resto ++ [x]

--18)

tiraRepetido ::Eq t =>  t -> [t] -> [t]
tiraRepetido _ [] = []
tiraRepetido n (x:resto) = if(n == x) then (tiraRepetido n resto ) 
                                      else (x:tiraRepetido n resto)  

elimina_repet :: Eq t => [t] -> [t] 
elimina_repet [] = []
elimina_repet (x:resto) = [x] ++ elimina_repet ( tiraRepetido x resto )

--19)

disponiveis = [1,2,5,10,20,50,100]

arranjaNota :: Integer -> [Integer] -> [Integer]
arranjaNota 0 lista = []
arranjaNota n (x:resto) = if( n < 0 ) then []
                          else x:( arranjaNota (n-x) (x:resto) ) 

notasTroco :: Int -> [[Int]]
notasTroco n = [ (arranjaNota n disponiveis) | x <- disponiveis]

--TODO revisao codigo para casos base