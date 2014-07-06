import Graphics.UI.GLUT

myPoints :: [(GLfloat, GLfloat, GLfloat)]
myPoints = [ (sin (2*pi*k/12), cos (2*pi*k/12), 0) | k <- [1..12] ]

main :: IO()
main = do
  (progName, args) <- getArgsAndInitialize
  window <- createWindow "Point8"
  displayCallback $= display
  mainLoop

display :: DisplayCallback
display = do
  clear [ColorBuffer]
  renderPrimitive Polygon $
    mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) myPoints
  flush
