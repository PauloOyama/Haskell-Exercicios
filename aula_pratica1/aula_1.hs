-- Ex 1 
double :: Int -> Int
double x = x * 2

quad :: Int -> Int
quad y = (double y) * 2  

hipotenusa :: Float -> Float -> Float
hipotenusa x y = sqrt(  x ^2 +  y^2)

distancia :: Float -> Float -> Float  -> Float  -> Float
distancia x1 y1 x2 y2 =  hipotenusa (x1 - x2) (y1 - y2)

--Ex 3 

conversao :: Float -> (Float, Float, Float) 
conversao real = (real, real * 3.96, real * 4.46)

--Ex 4

bissexto :: Int -> Bool
bissexto ano 
    | ( mod ano 400 == 0 ) = True
    | ( mod ano 4 == 0 ) && ( mod ano 100 /= 0) = True
    | otherwise = False 

-- Ex 5

type Data = (Int, Int,Int)
 
bissexto2 :: Data -> Bool
bissexto2 (dia, mes, ano)
    | dia >= 1 && dia <= 31 && (mes == 1 || mes == 3 || mes == 5 || mes == 7 || mes == 8 || mes == 10 || mes == 10 || mes == 12) = True 
    | dia >= 1 && dia <= 30 && ( mes == 4 || mes == 6 || mes == 9 ||mes == 11 ) = True  
    | dia >= 1 && dia <= 28 && mes == 2 && not (bissexto ano) = True 
    | dia >= 1 && dia <= 29 && mes == 2 && (bissexto ano) = True 
    | otherwise = False

-- Ex 6

valida  ::  Data -> Bool
valida  (dia, mes, ano)
    | dia < 1 = False 
    | dia > 28 && mes == 2 && not (bissexto ano) = False
    | dia > 29 && mes == 2 && (bissexto ano) = False
    | dia > 31 && (mes == 1 || mes == 3 || mes == 5 || mes == 7 || mes == 8 || mes == 10 || mes == 10 || mes == 12) = False
    | dia > 30 && ( mes == 4 || mes == 6 || mes == 9 ||mes == 11 ) = False
    | otherwise = True

--Ex 7

precede :: Data -> Data -> Bool
precede (dia1, mes1, ano1) (dia2, mes2, ano2)
    | ano1 < ano2 = True
    | ano1 == ano2 && mes1 < mes2 = True
    | ano1 == ano2 && mes1 == mes2 && dia1 < dia2 = True
    | ano1 == ano2 && mes1 == mes2 && dia1 < dia2 = True
    | otherwise = False


-- Ex 8

type Livro = (String, String,String,String, Data)
type Aluno = (String,String,String,String)
type Emprestimo = (String,String,Data,Data,String)

-- Ex 9

verificaEmprestimo :: Emprestimo -> Bool
verificaEmprestimo (cod,cod2,data1, data2,status) = ( precede data1 data2 ) 

