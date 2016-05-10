import System.Random
import Control.Monad
import Control.Concurrent.Async

data RC a = Bottom
          | Leaf [Bool] a 
          | Node [Bool] a (RC a) (RC a) 
          deriving (Show, Read, Eq)

data Coin = Heads 
          | Tails 
          deriving (Eq, Show, Enum, Bounded)
  
instance Random Coin where
  random g = case random g of 
                (r,g') | r < 1/2   = (Tails, g')
                       | otherwise = (Heads, g')

unit :: a -> RC a
unit x = Leaf [] x

instance Functor RC where 
    fmap f (Leaf xs x) = Leaf xs (f x)
    fmap f (Node xs x y z) = Node xs (f x) (fmap f y) (fmap f z)
	
getValue :: RC a -> [Bool] -> a
getValue (Leaf _ x) _ = x
getValue (Node _ x _ _) [] = x
getValue (Node _ _ l r) (False:xs) = getValue l xs
getValue (Node _ _ l r) (_:xs) = getValue r xs

getTree :: RC a -> [Bool] -> RC a
getTree (Leaf xs x) _ = (Leaf xs x)
getTree t [] = t
getTree (Node _ _ l r) (False:xs) = getTree l xs
getTree (Node _ _ l r) (_:xs) = getTree r xs

kleisli :: (a -> RC b) -> RC a -> RC b
kleisli f (Leaf xs x) = getTree (f x) xs
kleisli f (Node xs x l r) = Node xs (getValue (f x) xs) 
                                    (kleisli f l) 
                                    (kleisli f r)
	
instance Monad RC where
	return x = unit x
	m >>= f = kleisli f m
	
	
testF :: Int -> RC Int
testF x = Node [] x (Leaf [False] (x*2)) (Leaf [True] (x*3))

showRC :: RC a -> String
showRC (Leaf xs x) = "Leaf"
showRC (Node xs x l r) = (showRC l) ++ " | Node | " ++ (showRC r) 
							
toss :: IO Coin
toss = randomIO

choose :: RC a -> Int -> IO a
choose (Leaf _ x) _  = return x
choose (Node _ x _ _) 0 = return x
choose (Node _ x l r) n = do
                    c <- randomIO
                   (if (c==Heads) then (choose r (n-1))
                    else (choose l (n-1)))
							

tree::RC Int							
tree = Node [] 1 
           (Node [False] 2 
                 (Leaf [False, False] 3) 
                 (Leaf [False, True] 4)) 
           (Node [True] 5 
                 (Leaf [True, False] 6) 
                 (Leaf [True, True] 7))
tree2::RC Int
tree2 = Node [] 1 
            (Node [False] 2 
                  (Leaf [False, False] 3) 
                  (Leaf [False, True] 4)) 
            (Node [True] 5 
                  (Leaf [True, False] 6) 
                  (Leaf [True, True] 7))

add1 = do
	i <- (choose tree 2)
	j <- (choose tree 2)
	return (i+j)


add2 = choose (liftM2 (+) tree tree) 2	
	

add3 = do
	(i,j) <- concurrently (choose tree 2) (choose tree 2)
	return (i+j)
