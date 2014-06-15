import CV.Image
import CV.Edges

main :: IO()
main = do
  image <- readFromFile "smallLena.jpg" :: IO (Image GrayScale D32)
  let result = sobel (1,0) s5 image
  saveImage "Result.png" result
