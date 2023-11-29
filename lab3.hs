gt10 :: (Num a, Ord a) => a -> Bool
gt10 x = x > 10

-- sum :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)

empty :: a -> Bool

-- empty = \_ -> False
empty _ = False

-- zbior (a -> Bool) 
contains :: (a -> Bool) -> a -> Bool
-- zbior s, element e
-- contains s e = s e
-- contains s = s
contains = id



odd_numbers :: Integral a => a -> Bool 
-- odd_numbers n = mod n 2 == 1
-- odd_numbers n = odd n
odd_numbers = odd


even_numbers :: Integral a => a -> Bool
-- even_numbers n = mod n 2 == 1
-- even_numbers n = even n
even_numbers = even


-- f x = s x || x == s e

add :: Eq a => (a -> Bool) -> a -> (a -> Bool)
-- add s e = \x -> s x || x == e
add s e x = s x || x == e 

remove :: Eq a => (a -> Bool) -> a -> (a -> Bool)
remove s e x = s x && x /= e  

set_sum :: Eq a => (a -> Bool) -> (a -> Bool) -> (a -> Bool)
-- sum a b = \x -> (contains a x) || (contains b x)
set_sum a b x = a x || b x

intersection :: Eq a => (a -> Bool) -> (a -> Bool) -> (a -> Bool)
-- intersection a b = \x -> (contains a x) && (contains b x)
intersection a b x = a x && b x  

difference :: Eq a => (a -> Bool) -> (a -> Bool) -> (a -> Bool)
-- difference a b = \x -> (contains a x) && not (contains b x)
difference a b x = a x && not (b x)

complement :: (a -> Bool) -> (a -> Bool)
-- complement a = \x -> not (contains a x) 
-- complement a x = not (a x)

-- złożenie operacji not ze zbiorem a
complement a = not . a


one_el :: Eq a => a -> (a ->  Bool)
-- one_el el -> \x -> x == el
one_el el x = x == el



