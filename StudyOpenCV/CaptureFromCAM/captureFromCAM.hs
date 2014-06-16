import CV.Image
import CV.Video
import CV.HighGUI

main :: IO()
main = do
  Just srcCam <- captureFromCam 2
  Just frame <- getFrame srcCam
  display frame