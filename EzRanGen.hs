module EzRanGen (ranGen, shuffle, ranSeq) where
import System.Random

ranGen :: (Int,Int) -> Int -> Int -> [Int]
ranGen _ _ 0 = []
ranGen (x,y) genNo n = take n (randomRs (x,y) (mkStdGen genNo))

erase :: (Eq a) => a -> [a] -> [a]
erase x [] = []
erase x (y:ys) = if x == y then (erase x ys) else (y:(erase x ys))

range :: (Int,Int) -> [Int]
range (x,y) = case (compare x y) of GT -> (x:(range ((x-1),y)))
                                    EQ -> [x]
				    LT -> (x:(range ((x+1),y)))
				    
shuffle :: (Int,Int) -> Int -> [Int]
shuffle (x,y) z = shuffleInt (x,y) (mkStdGen z) [] (range (x,y))
                where shuffleInt (x,y) z a b = let rRes = randomR (x,y) z
		                                   rFst = fst rRes
					 	   rSnd = snd rRes in
		                               if b == [] then a
					       else (
					           if rFst `elem` a
		                                   then (shuffleInt (x,y) rSnd a b)
					           else (shuffleInt (x,y) rSnd (rFst:a) (erase rFst b))
						    )
singlSelect :: [a] -> Int -> a
singlSelect [] _ = error "Blank list or not found."
singlSelect (x:xs) y = if (y == 1) then x else (singlSelect xs (y-1))

listSelect :: [a] -> [Int] -> [a]
listSelect [] _ = []
listSelect _ [] = []
listSelect x (y:ys) = ((singlSelect x y):(listSelect x ys))

ranSeq :: [a] -> Int -> [a]
ranSeq [] _ = []
ranSeq x y = listSelect x (shuffle (1,(length x)) y)
