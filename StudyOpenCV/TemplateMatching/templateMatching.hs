import System.Environment(getArgs)
import CV.Image
import CV.Drawing
import CV.HighGUI
import CV.TemplateMatching
import Utils.Rectangle

main :: IO()
main = do
  [srcFile, tmpFile] <- getArgs
  srcImg <- readFromFile srcFile :: IO (Image GrayScale D32)
  opSrcImg <- readFromFile srcFile :: IO (Image RGB D32)
  tmpImg <- readFromFile tmpFile :: IO (Image GrayScale D32)
  (pt, _, _) <- getImageInfo tmpImg
  let (pm, _) = simpleTemplateMatch CCOEFF_NORMED srcImg tmpImg
      searchRect = mkRectangle pm pt
  opImg <- rectangle (0, 0, 255) 3 searchRect opSrcImg
  display $ (rgbToBgr.unsafeImageTo8Bit) opImg