import Graphics.UI.GLUT

display :: DisplayCallback
display = swapBuffers

main :: IO()
main = do
  (progName, argv) <- getArgsAndInitialize
  createWindow progName
  displayCallback $= display
  mainLoop

-- #include <GL/glut.h>

-- void display(void)
-- {
-- }

-- int main(int argc, char *argv[])
-- {
--   glutInit(&argc, argv);
--   glutCreateWindow(argv[0]);
--   glutDisplayFunc(display);
--   glutMainLoop();
--   return 0;
-- }
