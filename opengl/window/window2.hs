import Graphics.UI.GLUT

myInit :: IO()
myInit = do
  clearColor $= Color4 0.0 0.0 1.0 1.0

display :: DisplayCallback
display = do
  clear [ColorBuffer]
  swapBuffers

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
