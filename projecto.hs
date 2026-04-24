-- Autores: 
-- Ruan Vieira Damasceno Pimentel - 66502
-- Alexandre Felli Chimello - 66474

-- Exemplo: sigmoid 1 == 0.7310585786300049
sigmoid :: Double -> Double 
sigmoid x = 1 / (1 + exp (-x))

-- Exemplo: sigmoid' 0.75 == 0.1875
sigmoid' :: Double -> Double
sigmoid' x = x * (1 - x)

-- Exemplo: somaVectorial [33,25] [17,15] == [50,40]
somaVectorial :: [Double] -> [Double] -> [Double]
somaVectorial xs ys = zipWith (+) xs ys

-- Exemplo: chunksOf 3 [2, 4, 6, 8] == [[2, 4, 6], [8]]
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n list
    | n <= 0 = error "O tamanho do grupo deve ser maior que zero"
    | otherwise = take n list : chunksOf n (drop n list)

-- Exemplo: transpose [[4, 8], [3, 6]] == [[4, 3],[8, 6]]
transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([] : _) = []
transpose matrix = map head matrix : transpose (map tail matrix)

-- Exemplo : multMatrix [[2.0, 3.0], [1.0, 2.0], [8.0, 9.0]] [4.0, 8.0] == [32.0, 20.0, 104.0]
multMatrix :: [[Double]] -> [Double] -> [Double]
multMatrix matriz v = map (\linha -> sum (zipWith (*) linha v)) matriz

type Vector = [Double]
type Matrix = [Vector]
type Layer = (Matrix, Vector)
type Network = [Layer]

-- Exemplo: buildNetwork 2 [2] [0.1, 0.2, 0.3, 0.4, 0.5, 0.6] == [([[0.1, 0.2], [0.3, 0.4]] , [0.5, 0.6])]
buildNetwork :: Int -> [Int] -> [Double] -> Network
buildNetwork numInputs layerSizes weights =
    let
        buildLayer :: (Int, [Double], Network) -> Int -> (Int, [Double], Network)
        buildLayer (inputs, currentWeights, net) neurons =
            let
                (matrixWeights, rest1) = splitAt (neurons * inputs) currentWeights
                
                (bias, rest2) = splitAt neurons rest1
                
                matrix = chunksOf inputs matrixWeights
                
                currentLayer = (matrix, bias)
            in
                (neurons, rest2, net ++ [currentLayer])

        (_, _, finalNetwork) = foldl buildLayer (numInputs, weights, []) layerSizes
    in
        finalNetwork
-- Exemplo:  outputError [0.9, 0.2] [1.0, 0.0] == [-0.1, 0.2]
outputError :: [Double] -> [Double] -> [Double]
outputError predictions target = zipWith (-) predictions target

-- Exemplo: mse [0.9, 0.2] [1.0, 0.0] == 0.025
mse :: [Double] -> [Double] -> Double
mse predictions targets = sum (map (^2) (zipWith (-) predictions targets)) / fromIntegral (length targets)

--Exemplo: msePredictions [[0.9], [0.1]] [[1.0], [0.0]] == 0.01
msePredictions :: [[Double]] -> [[Double]] -> Double
msePredictions setPredictions setTargets = sum (zipWith mse setPredictions setTargets) / fromIntegral (length setTargets)

-- Exemplo: length (forwardPass [0,1] (buildNetwork 2 [2,1] (repeat 0.5))) == 3
forwardPass :: [Double] -> Network -> [[Double]]
forwardPass input rede = scanl calcularCamada input rede
   where 
     calcularCamada :: [Double] -> Layer -> [Double]
     calcularCamada x (w,b) =
       let 
         wx = multMatrix w x
         wxb = somaVectorial wx b
        in
         map sigmoid wxb
         
-- Exemplo: backPropagation 0.5 [1,0] [1] (buildNetwork 2 [2,1] (repeat 0.5))
backPropagation :: Double -> [Double] -> [Double] -> Network -> Network
backPropagation taxa input esperado rede =
   let
        ativacoes = forwardPass input rede
        previsao = last ativacoes
        deltaSaida = outputError previsao esperado
        entradasCamada = init ativacoes
        redeInvertida = reverse rede
        entradasInvertidas = reverse entradasCamada
     
        retroceder :: [Double] -> Network -> [[Double]] -> Network
        retroceder _ [] [] = []
        retroceder delta ((w,b) : restoRede) (a_ant:restoEntrada) =
            let
                w_novo = zipWith (\w_i d_i -> zipWith (\w_ij a_j -> w_ij - taxa * d_i * a_j) w_i a_ant) w delta
                b_novo = zipWith (\b_i d_i -> b_i - taxa * d_i) b delta
                somaErros = multMatrix (transpose w) delta
                delta_ant = zipWith (*) somaErros (map sigmoid' a_ant)
   
            in 
                (w_novo, b_novo) : retroceder delta_ant restoRede restoEntrada
 
    in 
        reverse (retroceder deltaSaida redeInvertida entradasInvertidas)
     
     
