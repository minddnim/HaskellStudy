import Control.Monad
import CV.Video
import CV.HighGUI

windowName :: String
windowName = "camera Window"

capture :: Capture -> IO ()
capture srcCam = do
  Just frame <- getFrame srcCam
  showImage windowName frame
  keyVal <- waitKey 33
  unless (keyVal == 27) $ capture srcCam

main :: IO()
main = do
  makeWindow windowName
  Just srcCam <- captureFromCam 1
  capture srcCam