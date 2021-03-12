

-- Ex 1 


a = 1 + 2 * 3
b = 5 ^ 3
c = 5 ** 3
d = 5 / 3
e = div 5 3
f = mod 5 3
g = 5 < 3
h = mod 5 3 < 2
i = mod 5 3 == 2
j = sqrt 81
k = logBase 2 1024
l = floor 5.7
m = ceiling 5.7
n = abs (-5)
o = min 6 7
p = max 6 7
q = sin (pi/2)
r = sum [1..5]
s = not True
t = True && False

-- Ex 2 

double x = x * 2


-- Ex 3

quad y = (double y) * 2  

-- Ex 4

hipotenusa x y = sqrt(x^2 + y^2)

-- Ex 5

distancia x1 y1 x2 y2 =  hipotenusa (x1 - x2) (y1 - y2)