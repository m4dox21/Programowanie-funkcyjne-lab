data BinaryTree a = Leaf | Node a (BinaryTree a) (BinaryTree a) 
    deriving (Eq, Ord)

exampleTree = 
    Node 10
        (Node 5
            (Node 3 Leaf Leaf)
            (Node 7
                (Node 6 Leaf Leaf)
                Leaf))
        (Node 15
            (Node 12 Leaf Leaf)
            Leaf)
                

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node val left right) =
    (inorder left) ++ [val] ++ (inorder right)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node val left right) =
     [val] ++ (preorder left) ++ (preorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node val left right) =
    (postorder left) ++ (postorder right) ++ [val]


breadthFirst :: BinaryTree a -> [a]
breadthFirst q = bfHlp (push q createEmpty) []

bfHlp :: Queue (BinaryTree a) -> [a] -> [a]
bfHlp qt xs = 
    if (empty qt)
    then xs 
    else
        let 
            (t, q) = pop qt
        in
            case t of
                Leaf -> bfHlp q xs
                (Node val left right) ->
                    bfHlp (push right (push left q)) (val:xs)


type Queue a = ([a], [a])

createEmpty :: Queue a
createEmpty = ([], [])

push :: a -> Queue a -> Queue a
push x (f, s) = (x:f, s)

pop :: Queue a -> (a, Queue a)
pop ([], []) = error "Pusta kolejka"
pop (e, []) = pop ([], reverse e)
pop (e ,(h:t)) = (h,(e, t))

empty :: Queue a -> Bool 
empty ([], []) = True
empty _ = False


data Ulamek = Integer :/ Integer 

infixl 7 :/

(//) :: Integer -> Integer -> Ulamek
_ // 0 = error "Dzielenie prze 0"
0 // _ = 0 :/ 1
a // b
    | b<0   = (-a) // (-b)
    | otherwise =
        let 
            d = gcd a b
        in 
            (div a d) :/ (div b d)



infixl 7 //

instance Show Ulamek where
    show (a :/ b) = (show a) ++ "/" ++ (show b)

instance Eq Ulamek where
    (a1 :/ b1) == (a2 :/ b2) = a1 * b2 == a2 * b1

instance Ord Ulamek where 
    (a1 :/ b1) <= (a2 :/ b2) = 
        let 
            p1 :/ q1 = a1 // b1
            p2 :/ q2 = a2 // b2 
        in 
            p1 * q2 <= p2 * q1 

instance Num Ulamek where 
    (a1 :/ b1) + (a2 :/ b2) = 
        (a1 * b2 + a2 * b1) // (b1 * b2)

    (a1 :/ b1) * (a2 :/ b2) = ( a1 * a2 ) // (b1 * b2)
 
    negate (a :/ b) = -a // b

    abs (a :/ b) = (abs a) // (abs b) 

    signum f 
        | f < 0     = -1
        | f == 0    = 0
        | otherwise = 1 
    
    fromInteger n = n :/ 1
