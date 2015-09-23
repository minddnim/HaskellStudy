import Graphics.UI.GLUT

points :: [(GLdouble, GLdouble)]
points = [(-0.9, -0.9), (0.9, -0.9), (0.9, 0.9), (-0.9, 0.9)]

myInit :: IO()
myInit = do
  clearColor $= Color4 0.0 0.0 1.0 1.0

display :: DisplayCallback
display = do
  clear [ColorBuffer]
  renderPrimitive LineLoop $ -- renderPrimitiveがglBeginとglEndの役割を担う(ラッパーみたいなもの)
    mapM_ (vertex . uncurry Vertex2) points
--  mapM_ (\(x, y) -> vertex $ Vertex2 x y) points と等価
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
--   glBegin(GL_LINE_LOOP);
--   glVertex2d(-0.9, -0.9);
--   glVertex2d(0.9, -0.9);
--   glVertex2d(0.9, 0.9);
--   glVertex2d(-0.9, 0.9);
--   glEnd();
--   glFlush();
-- }

-- void init(void)
-- {
--   glClearColor(0.0, 0.0, 1.0, 1.0);
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
