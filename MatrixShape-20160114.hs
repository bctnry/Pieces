module MatrixShape (
    matrix,
    retrR, retrC, retrL,
    shapeOut, shapeFrom, reshape,
    breakByR, breakByC, combineByR, combineByC,
    mTranspose,
    isRVector, isCVector,
    vectorMult, mvMult, mMult,
    identity
) where

data MatrixT a = MShape Int Int [a] deriving (Eq,Show)

-- retrive related.
retrR :: (Num a) => MatrixT a -> Int
retrR (MShape x _ _) = x

retrC :: (Num a) => MatrixT a -> Int
retrC (MShape _ x _) = x

retrL :: (Num a) => MatrixT a -> [a]
retrL (MShape _ _ x) = x

-- shape related.
matrix :: (Num a) => (Int,Int) -> a -> [a] -> MatrixT a
matrix (x,y) z a = if (length a) /= (x*y)
                   then (MShape x y (a++(take ((x*y)-(length a)) $ repeat z)))
                   else (MShape x y a)

polish :: (Num a) => (Int,Int) -> a -> [a] -> [a]
polish (x,y) z a = if (length a) /= (x*y) then (a++(take ((x*y)-(length a)) $ repeat z)) else a

shapeWith :: (Num a) => (Int,Int) -> a -> [a] -> [[a]]
shapeWith (0,0) _ x = [[head x]]
shapeWith _ _ [] = []
shapeWith (x,y) z a = ((take x shaped):(shapeWith (x,y-1) z (drop x shaped))) where shaped = polish (x,y) z a

shapeOut :: (Num a) => MatrixT a -> [[a]]
shapeOut (MShape x y z) = shapeWith (x,y) 0 z

shapeFrom :: (Num a) => [[a]] -> MatrixT a
shapeFrom [] = error "cannot shape from empty list"
shapeFrom l@(x:xs) = if and (map (\t -> (length t) == (length x)) xs)
                     then (MShape (length l) (length x) (foldl1 (++) l))
                     else error "cannot shape from malformed list"

reshape :: (Num a) => MatrixT a -> (Int,Int) -> a -> MatrixT a
reshape (MShape _ _ x) (y,z) a = (MShape y z (polish (y,z) a x))

-- composition related aux^2.
dropWhich :: [a] -> Int -> [a]
dropWhich [] _ = []
dropWhich (x:xs) 1 = xs
dropWhich (x:xs) y = if y<1 then error "index too small" else (x:(dropWhich xs (y-1)))

multResp :: (Num a) => [a] -> [a] -> [a]
multResp [] [] = []
multResp (x:xs) (y:ys) = ((x*y):(multResp xs ys))

intersect :: (Eq a) => [a] -> [a] -> [a]
intersect _ [] = []
intersect [] _ = []
intersect (x:xs) y = if x `elem` y then (x:(intersect xs y)) else (intersect xs y)

complementOf :: (Eq a) => [a] -> [Int] -> [a]
complementOf x [] = x
complementOf x y = foldl1 MatrixShape.intersect (map (dropWhich x) y)

transpose :: (Eq a) => [[a]] -> [[a]]
transpose [] = []
transpose x = if (head x) == [] then [] else (map head x):(transpose $ (map tail x))

set :: [a] -> Int -> a -> [a]
set [] _ _ = []
set (x:xs) 1 y = y:xs
set (x:xs) y z = (x:(set xs (y-1) z))

-- composition related aux.
breakByR :: (Eq a, Num a) => MatrixT a -> [MatrixT a]
breakByR m@(MShape 1 y z) = [m]
breakByR (MShape x y z) = ((MShape 1 y (take y z)):(breakByR (MShape (x-1) y (drop y z))))

breakByC :: (Eq a, Num a) => MatrixT a -> [MatrixT a]
breakByC m@(MShape x 1 z) = [m]
breakByC (MShape x y z) = ((MShape x 1 (map (z!!) [0,y..x*y-1])):(breakByC (MShape x (y-1) (complementOf z [1,1+y..x*y]))))

combineByR :: (Eq a, Num a) => [MatrixT a] -> MatrixT a
combineByR [] = (MShape 0 0 [])
combineByR l@(x:_) = (MShape (length l) (length (retrL x)) (foldl1 (++) (map retrL l)))

combineByC :: (Eq a, Num a) => [MatrixT a] -> MatrixT a
combineByC [] = (MShape 0 0 [])
combineByC l@(x:xs) = (MShape (length (retrL x)) (length l) (foldl1 (++) $ MatrixShape.transpose (map retrL l)))

isRVector :: (Num a) => MatrixT a -> Bool
isRVector (MShape x _ _) = x == 1

isCVector :: (Num a) => MatrixT a -> Bool
isCVector (MShape _ x _) = x == 1

mTranspose :: (Eq a, Num a) => MatrixT a -> MatrixT a
mTranspose = combineByC . breakByR

vectorMult :: (Num a) => MatrixT a -> MatrixT a -> a
vectorMult m1@(MShape _ _ x) m2@(MShape _ _ y) = if (isRVector m1)&&(isCVector m2) then (sum $ multResp x y) else error "cannot multiply"

mvMult :: (Eq a, Num a) => MatrixT a -> MatrixT a -> MatrixT a
mvMult m1@(MShape x1 y1 z1) m2@(MShape x y _) =
    if not ((y==1)&&(x==y1)) then error "cannot multiply"
    else (MShape x1 1 (map (\a -> vectorMult a m2) (breakByR m1)))

mMult :: (Eq a, Num a) => MatrixT a -> MatrixT a -> MatrixT a
mMult m1@(MShape _ x _) m2@(MShape y _ _) =
    if x /= y then error "cannot multiply"
    else (combineByC (map (mvMult m1) (breakByC m2)))

identity :: (Num a) => Int -> MatrixT a
identity x = MShape x x (take (x*x) $ cycle (1:(take x $ repeat 0)))
