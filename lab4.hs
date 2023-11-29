elems135 [] = []
-- elems135 [x] = [x]
-- elems135 [x, _] = [x]s
-- elems135 [x1, _, x2] = [x1, x2]
-- elems135 [x1, _, x2, _] = [x1, x2]
-- elems135 [x1, _, x2, _, x3] = [x1, x2, x3]

elems135 (x1: _: x3: _: x5: _) = [x1, x3, x5]



-- metoda indeksowania
-- indeks 0 odpowiada miejscu 1, indeks 2 miejscu 3, itd..
-- elems135 lst [lst !! 0, lst !! 2, lst !! 4]


co_drugi_od_pierwszego [] = []
co_drugi_od_pierwszego (h:t) =
    h:(co_drugi_od_drugiego t) 

co_drugi_od_drugiego [] = [] 
co_drugi_od_drugiego (h:t) =
    co_drugi_od_pierwszego t



usun_element _ [] = []
usun_element a (h:t)
    | a == h    = usun_element a t
    | otherwise = h:(usun_element a t)


zastap_element _ _ [] = []
zastap_element stary nowy (h:t)
    | stary == h    = nowy:(zastap_element stary nowy t)
    | otherwise     = h:(zastap_element stary nowy t)



-- usun_elementy_z_konca _ [] = []
usun_elementy_z_konca a [] = []
usun_elementy_z_konca a (h:t) =
    let
        ogon = usun_elementy_z_konca a t
    in
        if a == h then
            if ogon == [] then
                []
            else
                h:ogon
        else
            h:ogon

ustaw_element 0 element (_:t) = element:t
ustaw_element indeks _ [] = error "błędny indeks"
ustaw_element indeks element (h:t) =
    h:(ustaw_element (indeks - 1) element t)



reverse' [] = []
reverse' (h:t) = (reverse' t) ++ [h]

reverse'' lst = reverse_iter lst []

reverse_iter [] lst_out = lst_out
reverse_iter (h_in:t_in) lst_out =
    reverse_iter t_in (h_in:lst_out)


usun_element_2d :: Eq a => a -> [[a]] -> [[a]]
usun_element_2d element [] = []
usun_element_2d element (h:t) = 
    (usun_element element h):(usun_element_2d element t)

-- 07.11.2023 Słowniki
-- [(a, b)]

empty_dict :: Eq a => [(a, b)]
empty_dict = []

in_dict :: Eq a => a -> [(a, b)] -> Bool
in_dict _ [] = False
in_dict el ((k, _):t) 
    | el == k   = True
    | otherwise = in_dict el t

-- elem 'c' $ map fst [('a',5),('c',6),('d',7)]
in_dict' el lst = elem el $ map fst lst 
-- any ((=='a') . fst) [('a',5),('c',6),('d',7)]
in_dict'' el lst = any ((== el) . fst) lst
in_dict''' el = any ((== el) . fst)


get_elem :: Eq a => [(a, b)] -> a -> b
-- get_elem [] _ = error "brak klucza"
-- get_elem ((k, v): t) key
--     | key == k  = v
--     | otherwise = get_elem t key


get_elem lst key =
    snd $ head $ dropWhile ((/= key) . fst) lst
    -- snd (head (dropWhile ((/= key) . fst ) lst))


set_elem :: Eq a => [(a, b)] -> a -> b -> [(a, b)]
set_elem [] key value = [(key, value)]
set_elem ((k, v):t) key value
    | key == key    = (k, value):t
    | otherwise     = (k, v):(set_elem t key value)


del_elem :: Eq a => [(a, b)] -> a -> [(a, b)]
del_elem [] key = error "bra klucza"
del_elem ((k, v): t) key
    | key == k      = t
    | otherwise     = (k, v):(del_elem t key)
