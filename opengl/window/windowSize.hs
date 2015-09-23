import Graphics.UI.GLUT

points :: [(GLdouble, GLdouble)]
points = [(-0.9, -0.9), (0.9, -0.9), (0.9, 0.9), (-0.9, 0.9)]

lineColor :: Color3 GLdouble
lineColor = Color3 1.0 0.0 0.0

clearMyColor :: Color4 GLfloat
clearMyColor = Color4 1.0 1.0 1.0 1.0

resize :: Size -> IO()
resize size@(Size width height) = do
  viewport $= (Position 0 0, size)
  loadIdentity 
  ortho (-w / 200.0) (w / 200.0) (-h / 200.0) (h / 200.0) (-1.0) (1.0)
  where w = fromIntegral width
        h = fromIntegral height

myInit :: IO()
myInit = do
  clearColor $= clearMyColor

display :: DisplayCallback
display = do
  clear [ColorBuffer]
  color lineColor
  renderPrimitive Polygon $ -- renderPrimitiveがglBeginとglEndの役割を担う(ラッパーみたいなもの)
    mapM_ (vertex . uncurry Vertex2) points
--  mapM_ (\(x, y) -> vertex $ Vertex2 x y) points と等価
  swapBuffers -- flush関数だと上手く表示されなかったためswapBuffersにした

main :: IO()
main = do
  initialWindowPosition $= Position 100 100
  initialWindowSize $= Size 320 240
  (progName, argv) <- getArgsAndInitialize
  initialDisplayMode $= [RGBAMode, DoubleBuffered]
  createWindow progName
  displayCallback $= display
  reshapeCallback $= Just resize
  myInit
  mainLoop

-- #include <GL/glut.h>

-- void display(void)
-- {
--   glClear(GL_COLOR_BUFFER_BIT);
--   glColor3d(1.0, 0.0, 0.0);
--   glBegin(GL_POLYGON);
--   glVertex2d(-0.9, -0.9);
--   glVertex2d(0.9, -0.9);
--   glVertex2d(0.9, 0.9);
--   glVertex2d(-0.9, 0.9);
--   glEnd();
--   glFlush();
-- }

-- void resize(int w, int h)
-- {
--   /* ウィンドウ全体をビューポートにする */
--   glViewport(0, 0, w, h);

--   /* 変換行列の初期化 */
--   glLoadIdentity();

--   /* スクリーン上の表示領域をビューポートの大きさに比例させる */
--   glOrtho(-w / 200.0, w / 200.0, -h / 200.0, h / 200.0, -1.0, 1.0);
-- }

-- void init(void)
-- {
--   glClearColor(1.0, 1.0, 1.0, 1.0);
-- }

-- int main(int argc, char *argv[])
-- {
--   glutInitWindowPosition(100, 100);
--   glutInitWindowSize(320, 240);
--   glutInit(&argc, argv);
--   glutInitDisplayMode(GLUT_RGBA);
--   glutCreateWindow(argv[0]);
--   glutDisplayFunc(display);
--   glutReshapeFunc(resize);
--   init();
--   glutMainLoop();
--   return 0;
-- }
