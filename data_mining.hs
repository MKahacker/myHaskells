----------- Distance Between Vectors ----------------------

----------- L1 distance between vectors measures number of bits that are different in binary vectors -------
hamming :: (Num a) => [a] -> [a] -> a
hamming [] y = 0
hamming x [] = 0
hamming (x:xs) (y:ys) = abs(x-y) + hamming xs ys

----------- Jaccard Distance measures simmilarities between binary vectors disregarding negative simmilarities ----
numones :: (Eq a, Num a) => [a] -> [a] -> a
numones [] y = 0
numones x [] = 0
numones (1:xs) (1:ys) = 1 + numones xs ys
numones (x:xs) (y:ys)  = 0 + numones xs ys

allbutzeros :: (Eq a, Num a) => [a] -> [a] -> a
allbutzeros [] y = 0
allbutzeros x [] = 0
allbutzeros (0:xs) (0:ys) = 0 + allbutzeros xs ys
allbutzeros (x:xs) (y:ys) = 1 + allbutzeros xs ys

jaccard :: (Fractional a, Eq a, Num a) => [a] -> [a] -> a
jaccard x y = (numones x y)/(allbutzeros x y)
