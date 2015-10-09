-- SideChain.hs
-- There are two operators that I found useful at some times.
-- Don't know if someone have invented the idea before, nor do I know if someone 
-- have realized it and put it into the haskell's standard library. So I put them here.
-- I call the first "SideChain", and the second "ChainCombination".
module SideChain ((<~>), (<::>)) where

(<~>) :: (a -> b) -> (a -> c) -> a -> (b,c)
(<~>) f1 f2 = (\x -> ((f1 x), (f2 x)))

(<::>) :: (a -> (b,c)) -> (b -> c -> d) -> (a -> d)
(<::>) f1 f2 = (\x -> (f2 (fst (f1 x)) (snd (f1 x)))) 
