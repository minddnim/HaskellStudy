import Graphics.UI.GLUT

color3d :: GLdouble -> GLdouble -> GLdouble -> IO()
color3d r g b = color $ Color3 r g b

vertex2d :: GLdouble -> GLdouble -> IO()
vertex2d x y = vertex $ Vertex2 x y

clearMyColor :: Color4 GLfloat
clearMyColor = Color4 1.0 1.0 1.0 1.0

myInit :: IO()
myInit = do
  clearColor $= clearMyColor

display :: DisplayCallback
display = do
  clear [ColorBuffer]
  renderPrimitive Polygon $ do -- renderPrimitiveがglBeginとglEndの役割を担う(ラッパーみたいなもの)
    color3d 1.0 0.0 0.0
    vertex2d (-0.9) (-0.9)
    color3d 0.0 1.0 0.0
    vertex2d 0.9 (-0.9)
    color3d 0.0 0.0 1.0
    vertex2d 0.9 0.9
    color3d 1.0 1.0 0.0
    vertex2d (-0.9) 0.9
  swapBuffers -- flush関数だと上手く表示されなかったためswapBuffersにした

main :: IO()
main = do
  (progName, argv) <- getArgsAndInitialize
  initialDisplayMode $= [RGBAMode, DoubleBuffered]
  createWindow progName
  displayCallback $= display
  myInit
  mainLoop

-- #include <GL/glut.h>

-- void display(void)
-- {
--   glClear(GL_COLOR_BUFFER_BIT);
--   glBegin(GL_POLYGON);
--   glColor3d(1.0, 0.0, 0.0); /* 赤 */
--   glVertex2d(-0.9, -0.9);
--   glColor3d(0.0, 1.0, 0.0); /* 緑 */
--   glVertex2d(0.9, -0.9);
--   glColor3d(0.0, 0.0, 1.0); /* 青 */
--   glVertex2d(0.9, 0.9);
--   glColor3d(1.0, 1.0, 0.0); /* 黄 */
--   glVertex2d(-0.9, 0.9);
--   glEnd();
--   glFlush();
-- }

-- void init(void)
-- {
--   glClearColor(1.0, 1.0, 1.0, 1.0);
-- }

-- int main(int argc, char *argv[])
-- {
--   glutInit(&argc, argv);
--   glutInitDisplayMode(GLUT_RGBA);
--   glutCreateWindow(argv[0]);
--   glutDisplayFunc(display);
--   init();
--   glutMainLoop();
--   return 0;
-- }
