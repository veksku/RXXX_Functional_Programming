class Takeable t where
    takeSome :: Int -> t a -> [a]

instance Takeable [] where
    takeSome = take 

data Tree a = 
        None 
    |   Node {l :: (Tree a), v:: a, r:: (Tree a)}

toList:: Tree a -> [a]
toList None = []
toList (Node l v r) = (toList l) ++ [v] ++ (toList r)

instance Takeable Tree where
    takeSome k = take k . toList

instance Takeable Maybe where
    takeSome _ Nothing = []
    takeSome k (Just x)
        | k <= 0 = []
        | otherwise = [x]

data Tree2 t = Tree2 [Tree2 t]