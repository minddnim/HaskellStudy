import CV.Image
import CV.HighGUI

main :: IO()
main = do
  image <- readFromFile "smallLena.jpg" :: IO (Image RGB D32)
  display image
