perm [x] = [[x]]
perm x = foldl1 (++) $ map (\z -> map (z:) (perm (filter (/=z) x))) x
raise x = zip [1..(length x)] x
take2 x = foldl1 (++) $ map (\z -> map ((,) z) (filter (/=z) x)) x
safe' ((x,y),(a,b)) = (x/=a)&&(y/=b)&&((abs (x-a))/=(abs (y-b)))
queen8 = filter (and . map safe' . take2 . raise) $ perm [1..8]
