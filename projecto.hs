sigmoid :: Double -> Double 
sigmoid x = 1 / (1 + exp (-x))

sigmoid' :: Double -> Double
sigmoid' x = x * (1 - x)

somaVectorial :: [Double] -> [Double] -> [Double]
somaVectorial xs ys = zipWith (+) xs ys

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n list
    | n <= 0 = error "O tamanho do grupo deve ser maior que zero"
    | otherwise = take n list : chunksOf n (drop n list)

transpose' :: [[a]] -> [[a]]
transpose' [] = []
transpose' ([] : _) = []
transpose' matrix = map head matrix : transpose' (map tail matrix)
