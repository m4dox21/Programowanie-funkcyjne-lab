-- lista pust []
-- lista jedno elementowa 1 : []

-- lista dwu elementowa 3 : (4 : [])
-- 3 : 4 : 5 : 6 : 7 : []


-- [1, 2, 3, 4] !! 2 - wypisze elemenet z listy o indeksie 2

glowa (h:t) = h
-- glowa (h:_) = h

ogon (_:t) = t

dlugosc [] = 0
dlugosc (_:t) = 1 + dlugosc t

suma_elementow [] = 0
suma_elementow (h:t) = h + suma_elementow t
 
nth_el 0 (h:_) = h
nth_el n (_:t) = nth_el (n - 1) t

konkatenacja [] lst = lst
konkatenacja (h:t) lst = h:(konkatenacja t lst)