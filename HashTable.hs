module HashTable (
                 retrKeys, retrValues,
	         count, update, delete,
		 valueOf, keyOf
		 ) where
import SideChain

data By a b = Key a | Value b | Tuple a b

justCons :: Maybe a -> Maybe [a] -> Maybe [a]
justCons Nothing x = x
justCons (Just x) Nothing = Just [x]
justCons (Just x) (Just y) = Just (x:y)

retrKeys :: (Eq a) => [(a,b)] -> [a]
retrKeys [] = []
retrKeys x = ((fst . head) <~> (retrKeys . tail)) <::> (:) $ x

retrValues :: (Eq b) => [(a,b)] -> [b]
retrValues [] = []
retrValues x = ((snd . head) <~> (retrValues . tail)) <::> (:) $ x

retrSingl :: (Eq a, Eq b) => By a b -> [(a,b)] -> Maybe (a,b)
retrSingl _ [] = Nothing
retrSingl x ((y,z):xs) = let retrSinglBy cond = if cond then (Just (y,z)) else (retrSingl x xs) in
                         case x of (Key a) -> retrSinglBy (a == y) 
			           (Value a) -> retrSinglBy (a == z)
				   (Tuple a b) -> retrSinglBy ((a == y) && (b == z))

count :: (Eq a, Eq b) => By a b -> [(a,b)] -> Int
count _ [] = 0
count by ((x,y):xs) = let countBy cond = if cond then (1 + (count by xs)) else (count by xs) in
                      case by of (Key a) -> countBy (a == x)
                                 (Value a) -> countBy (a == y)
				 (Tuple a b) -> countBy ((a == x) && (b == y))

valueOf :: (Eq a) => a -> [(a,b)] -> Maybe b
valueOf _ [] = Nothing
valueOf x ((y,z):xs) = if x == y then Just z else (valueOf x xs)

keyOf :: (Eq b) => b -> [(a,b)] -> Maybe [a]
keyOf _ [] = Nothing
keyOf x ((y,z):xs) = if x == z then (justCons (Just y) (keyOf x xs)) else (keyOf x xs)

update :: (Eq a, Eq b) => (a,b) -> [(a,b)] -> [(a,b)]
update x [] = [x]
update t@(a,b) ((x,y):xs) = if a == x then ((a,b):xs) else ((x,y):(update t xs))

delete :: (Eq a, Eq b) => By a b -> [(a,b)] -> [(a,b)]
delete x [] = []
delete x ((y,z):xs) = let nextItem = ((y,z):(delete x xs))
                          deleteBy cond = if cond then xs else nextItem in
		      case x of (Key a) -> deleteBy (a == y)
			        (Value a) -> deleteBy (a == z)
				(Tuple a b) -> deleteBy ((a == y) && (b == z))
