import qualified Data.ByteString.Lazy as LB
import System.Environment( getArgs )
import Codec.Picture
import Codec.Picture.Types

transformImage :: Image PixelYCbCr8 -> Image PixelYCbCr8
transformImage img = img -- Transform the image here

transformRGBImage :: Image PixelRGB8 -> Image PixelRGB8
transformRGBImage img = resize 300 500 img -- Transform the RGB image here

main :: IO ()
main = do
  commandArguments <- getArgs
  case commandArguments of
    [] -> putStrLn "Not enough arguments"
    (filename : _) -> do
      dynImg <- readImage filename
      case dynImg of
        Left err -> putStrLn err

        -- the Jpeg decoder return an image in the YCbCr colorspace
        -- Right (ImageYCbCr8 img) ->
        --     LB.writeFile (filename ++ "_transformed.jpg")
        --           . encodeJpegAtQuality 100 $ transformImage img

        -- If you prefer to work in the RGB8 colorspace, uncomment
        -- the following
        Right (ImageYCbCr8 img) ->
            writePng (filename ++ "_transformed.png")
                      . transformRGBImage $ convertImage img


        Right _ -> putStrLn "Unhandled image colorspace"

bilinear :: (Integral (PixelBaseComponent a), Pixel a) => a -> a -> a -> a -> Float -> Float -> a
bilinear p q r s u v = mixWith (f v) (mixWith (f u) p q) (mixWith (f u) r s) where
        f t _ x y = floor $ fromIntegral x * (1 - t) + fromIntegral y * t

resize ::  (Integral (PixelBaseComponent a), Pixel a) => Int -> Int -> Image a -> Image a
resize w h img@(Image w0 h0 _) = generateImage f w h where
    f x y = let x' = fromIntegral x / fromIntegral w * fromIntegral w0
                y' = fromIntegral y / fromIntegral h * fromIntegral h0
            in bilinear
                (pixelAt img (floor x') (floor y'))
                (pixelAt img (floor x' + 1) (floor y'))
                (pixelAt img (floor x') (floor y' + 1))
                (pixelAt img (floor x' + 1) (floor y' + 1))
                    (x' - fromIntegral (floor x'))
                    (y' - fromIntegral (floor y'))
