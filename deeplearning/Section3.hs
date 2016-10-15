import System.IO

stepFunction :: Double -> Int
stepFunction x | x > 0.0 = 1
               | otherwise = 0

main :: IO()
main = do
  putStrLn "finish"
  