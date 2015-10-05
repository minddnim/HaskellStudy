import Graphics.UI.GLUT
import Data.IORef
import Control.Monad
import qualified Data.Vector as V
import Text.Printf

data Pos2D = Pos2D {
  _px2D :: GLdouble
, _py2D :: GLdouble
}

data Pos3D = Pos3D {
  _px3D :: GLdouble
, _py3D :: GLdouble
, _pz3D :: GLdouble
}

data ColorRGB = ColorRGB {
  _red :: GLdouble
, _green :: GLdouble
, _bule :: GLdouble
}

data VerInfo = VerInfo {
  _v3D :: Pos3D
, _clrRGB :: ColorRGB
}

data FaceInfo = FaceInfo {
  _vNum :: Int
, _vIdxs :: V.Vector Int
}

data ImgFileInfo = ImgFileInfo {
  _imgIdx :: Int
, _imgFileName :: String
}

data ImgInfo = ImgInfo {
  _imgNum :: Int
, _imgFileList :: V.Vector ImgFileInfo
}

data TextureRegionInfo = TextureRegionInfo {
  _imgNo :: Int
, _v2D :: Pos2D
, _v3DIdx :: Int
}

data TextureRegionInfos = TextureRegionInfos {
  _textureRegionInfos :: V.Vector TextureRegionInfo
}

data TextureInfo = TextureInfo {
  _vInfos :: V.Vector VerInfo
, _fInfos :: V.Vector FaceInfo
, _iInfo :: ImgInfo
, _trInfos :: V.Vector TextureRegionInfos
}

data ProjectionInfo = ProjectionInfo {
  _windowSize :: Size
, _zoomRatio :: GLdouble
} deriving (Show)

color3d :: ColorRGB -> IO()
color3d (ColorRGB r g b) = color $ Color3 r g b

vertex3d :: Pos3D -> IO()
vertex3d (Pos3D x y z) = vertex $ Vertex3 x y z

vertex2d :: GLdouble -> GLdouble -> IO()
vertex2d x y = vertex $ Vertex2 x y

readPlyFile :: FilePath -> IO TextureInfo
readPlyFile filePath = do
  str <- readFile filePath
  let (elemVerNumStr:remStrsTmp1) = drop 2 $ lines str
      verNum = read ((last . words) elemVerNumStr) :: Int
      (elemFaceNumStr:remStrsTmp2) = drop 6 remStrsTmp1
      faceNum = read ((last . words) elemFaceNumStr) :: Int
      (verInfoStr, contStrTmp1) = splitAt verNum (drop 2 remStrsTmp2)
      (faceInfoStr, imgNumStr:contStrTmp2) = splitAt faceNum contStrTmp1
      imgNum = read imgNumStr :: Int
      (imgInfoStr, contStrTmp3) = splitAt imgNum contStrTmp2
      verInfos = V.fromList $ map getVertexInfo verInfoStr
      faceInfos = V.fromList $ map getFaceInfo faceInfoStr
      imgFileInfos = V.fromList $ map getImgFileInfo imgInfoStr
      imgInfo = ImgInfo imgNum imgFileInfos
      texRegInfos = getTextureRegionInfosVector contStrTmp3
  return (TextureInfo verInfos faceInfos imgInfo texRegInfos)

getVertexInfo :: String -> VerInfo
getVertexInfo str = VerInfo ver clr
  where [x, y, z, r, g, b] = (map read . words) str
        ver = Pos3D x y z
        clr = ColorRGB ((r + 1.0)/256.0) ((g + 1.0)/256.0) ((b + 1.0)/256.0)

getFaceInfo :: String -> FaceInfo
getFaceInfo str = FaceInfo num (V.fromList idxs)
  where (num:idxs) = (map read . words) str

getImgFileInfo :: String -> ImgFileInfo
getImgFileInfo str = ImgFileInfo idx fileName
  where [idxStr, fileName] = words str
        idx = read idxStr

getTextureRegionInfosVector :: [String] -> V.Vector TextureRegionInfos
getTextureRegionInfosVector strs = V.fromList $ map getTextureRegionInfos textureRegionInfosStrs
  where textureRegionInfosStrs = divStr strs []

divStr :: [String] -> [[String]] -> [[String]]
divStr [] ret = ret
divStr strs ret = divStr remStr (element:ret)
  where (element, remStr) = splitAt 3 strs

getTextureRegionInfos :: [String] -> TextureRegionInfos
getTextureRegionInfos strs = TextureRegionInfos $ V.fromList $ map getTextureRegionInfo strs

getTextureRegionInfo :: String -> TextureRegionInfo
getTextureRegionInfo str = TextureRegionInfo imgIdx (Pos2D px py) pos3DIdx
  where [imgIdxStr, pxStr, pyStr, pos3DIdxStr] = words str
        imgIdx = read imgIdxStr :: Int
        px = read pxStr :: GLdouble
        py = read pyStr :: GLdouble
        pos3DIdx = read pos3DIdxStr :: Int


postDisplay :: IO()
postDisplay = do
  window <- get currentWindow
  postRedisplay window

resize :: IORef ProjectionInfo -> ReshapeCallback
resize projInfoIORef size = do
  ProjectionInfo s z <- readIORef projInfoIORef
  writeIORef projInfoIORef (ProjectionInfo size z)
  postDisplay

renderProjection :: IORef ProjectionInfo -> IO()
renderProjection projInfoIORef = do
  projInfo <- readIORef projInfoIORef
  let size@(Size width height) = _windowSize projInfo
      zoom = _zoomRatio projInfo
      ratio = w / h
      viewRangeX = 1.0
      viewRangeY = 1.0
      w = fromIntegral width
      h = fromIntegral height
  viewport $= (Position 0 0, size)
  loadIdentity 
  ortho (-ratio * viewRangeX) (ratio * viewRangeX) (-viewRangeY) (viewRangeY) (-100.0) (100.0)
  scale zoom zoom zoom

renderModel :: TextureInfo -> DisplayCallback
renderModel (TextureInfo vInfo fInfo iInfo trInfos) = do
  clear [ColorBuffer, DepthBuffer, StencilBuffer]
  forM_ [0..(V.length fInfo - 1)] $ \i -> do
    renderPrimitive Polygon $ do
      let FaceInfo num idxs = (fInfo V.! i)
      forM_ [0..(num - 1)] $ \j -> do
        let vIdx = idxs V.! j
            VerInfo v c = vInfo V.! vIdx
        color3d c
        vertex3d v
  swapBuffers

renderScene :: TextureInfo -> IORef ProjectionInfo -> DisplayCallback
renderScene texInfo projInfoIORef = do
  renderProjection projInfoIORef
  renderModel texInfo

motion :: MotionCallback
motion (Position x y) = return ()



mouse :: IORef ProjectionInfo -> MouseCallback
mouse projInfoIORef b st (Position x y) = do
  projInfo <- readIORef projInfoIORef
  let size = _windowSize projInfo
      zoom = _zoomRatio projInfo
      upProjInfo = ProjectionInfo size (min (zoom + step) zoomMax)
      downProjInfo = ProjectionInfo size (max (zoom - step) zoomMin)
  case b of
    LeftButton -> return ()
    MiddleButton -> return ()
    RightButton -> return ()
    WheelUp -> writeIORef projInfoIORef upProjInfo
    WheelDown -> writeIORef projInfoIORef downProjInfo
  postDisplay
  where zoomMax = 5.0
        zoomMin = 0.0001
        step = 0.001

clearMyColor :: Color4 GLfloat
clearMyColor = Color4 0.0 0.0 0.0 1.0

myInit :: IO()
myInit = clearColor $= clearMyColor

main :: IO ()
main = do
  let winSize = Size 400 400
  projInfo <- newIORef (ProjectionInfo winSize 1.0)
  initialWindowPosition $= Position 100 100
  initialWindowSize $= winSize
  (progName, args) <- getArgsAndInitialize
  initialDisplayMode $= [RGBAMode, DoubleBuffered]
  window <- createWindow "3D Reconst Viewer"
  texInfo <- readPlyFile "outputFace.ply"
  reshapeCallback $= Just (resize projInfo)
  displayCallback $= (renderScene texInfo projInfo)
  mouseCallback $= Just (mouse projInfo)
  motionCallback $= Just motion
  myInit
  mainLoop
