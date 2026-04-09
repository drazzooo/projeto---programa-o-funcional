sigmoid :: Double -> Double 
sigmoid x = 1 / (1 + exp (-x))

sigmoid' :: Double -> Double
sigmoid' x = x * (1 - x)

somaVectorial :: [Double] -> [Double] -> [Double]
somaVectorial xs ys = zipWith (+) xs ys

-- Exemplo: 3 [2, 4, 6, 8] == [[2, 4, 6], [8]]
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n list
    | n <= 0 = error "O tamanho do grupo deve ser maior que zero"
    | otherwise = take n list : chunksOf n (drop n list)

-- Exemplo: [[4, 8], [3, 6]] == [[4, 3],[8, 6]]
transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([] : _) = []
transpose matrix = map head matrix : transpose (map tail matrix)

-- Exemplo : multMatrix [[2.0, 3.0], [1.0, 2.0], [8.0, 9.0]] [4.0, 8.0] == [32.0, 20.0, 104.0]
multMatrix :: [[Double]] -> [Double] -> [Double]
multMatrix matriz v = map (\linha -> sum (zipWith (*) linha v)) matriz
