-- Zad 5
s n = 
    if n == 1 then
        1
    else
        1 / n + s (n - 1)


-- 5.b

s2 1 = 1
s2 n = 1 / n + s2 (n - 1)

-- 5.c

s' n = s'iter n 0 1 

s'iter n s i =
    if i  <= n then
        s'iter n (s + 1/i)(i+1)
    else
        s



-- Zad 6

silnia n =
    if n == 0 then
        1
    else 
        n * silnia ( n - 1)

-- 6.b

silnia2 0 = 1
silnia2 n = n * silnia ( n - 1)

-- 6.c

silnia' n = silnia'_iter n 1 0

silnia'_iter n s i = 
    if i < n then
        silnia'_iter n ((i + 1) * s)(i+1)
    else 
        s

-- Zad 7

fib 0 = 0
fib 1 = 1
fib n = fib(n - 1) + fib(n - 2)

-- 7.b

fib' 0 = 0
fib' n = fib'_iter n 1 1 0

fib'_iter n i f fp = 
    if i < n then
        fib'_iter n (i + 1)(f + fp) f
    else
        return f

-- Zad 8

g x = x ** 2

f x = 
    let 
        gx = g x
        gx1 = g (x + 1)
    in
        gx + gx ** 3