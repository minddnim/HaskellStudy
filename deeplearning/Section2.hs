import System.IO

myAND1 :: Double -> Double -> Int
myAND1 x1 x2 | tmp <= theta = 0
            | otherwise = 1
  where (w1, w2, theta) = (0.5, 0.5, 0.7)
        tmp = x1 * w1 + x2 * w2

perceptron :: Double -> Double -> Double -> Double -> Double -> Int
perceptron w1 w2 b x1 x2 | tmp <= 0.0 = 0
                         | otherwise = 1
  where x = [x1, x2]
        w = [w1, w2]
        tmp = b + (sum $ zipWith (*) x w)

main :: IO()
main = do
  putStrLn "myAND1"
  putStrLn $ show $ myAND1 0.0 0.0
  putStrLn $ show $ myAND1 0.0 1.0
  putStrLn $ show $ myAND1 1.0 0.0
  putStrLn $ show $ myAND1 1.0 1.0

  let myAND2 = perceptron 0.5 0.5 (-0.7)
  putStrLn "myAND2"
  putStrLn $ show $ myAND2 0.0 0.0
  putStrLn $ show $ myAND2 0.0 1.0
  putStrLn $ show $ myAND2 1.0 0.0
  putStrLn $ show $ myAND2 1.0 1.0

  let myNAND = perceptron (-0.5) (-0.5) 0.7
  putStrLn "myNAND"
  putStrLn $ show $ myNAND 0.0 0.0
  putStrLn $ show $ myNAND 0.0 1.0
  putStrLn $ show $ myNAND 1.0 0.0
  putStrLn $ show $ myNAND 1.0 1.0

  let myOR = perceptron 0.5 0.5 (-0.2)
  putStrLn "myOR"
  putStrLn $ show $ myOR 0.0 0.0
  putStrLn $ show $ myOR 0.0 1.0
  putStrLn $ show $ myOR 1.0 0.0
  putStrLn $ show $ myOR 1.0 1.0

  let myXOR x1 x2 = myAND2 (fromIntegral (myNAND x1 x2)) (fromIntegral (myOR x1 x2))
  putStrLn "myXOR"
  putStrLn $ show $ myXOR 0.0 0.0
  putStrLn $ show $ myXOR 0.0 1.0
  putStrLn $ show $ myXOR 1.0 0.0
  putStrLn $ show $ myXOR 1.0 1.0

  putStrLn "finish"