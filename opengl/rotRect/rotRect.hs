import Graphics.UI.GLUT as UT
import Graphics.Rendering.OpenGL.GLU.Matrix as M
import Graphics.Rendering.OpenGL.GL.Tensor as T

import System.Exit
import Data.IORef

timerInterval :: Int
timerInterval = 1

main :: IO()
main = do
  (_, _) <- getArgsAndInitialize -- GLUTを使うときは、この関数で初期化しなければいけない。
  rot <- newIORef 0.0
  dir <- newIORef 0.1
  createWindow "RotRect"
  initialDisplayMode $= [RGBAMode, DoubleBuffered]
  initialWindowSize $= Size 640 480 -- 初期window size指定
  reshapeCallback $= Just reshape -- 現在のwindowが再形成されたときに実行される
  addTimerCallback timerInterval $ timerProc (display rot dir) -- display表示の関数を登録
  keyboardMouseCallback $= Just (keyboardProc dir)
  mainLoop -- GLUTのevent処理loopに入る。登録されている全てのcallbackを呼び出す。

--key入力で回転角度、回転速度を操作する
keyboardProc :: IORef GLdouble -> Key -> KeyState -> Modifiers -> Position -> IO()
keyboardProc dir k s _ _ | k == Char 'r' = modifyIORef dir (\x -> -abs x)
                         | k == Char 'l' = modifyIORef dir (\x -> abs x)
                         | k == Char 'u' = modifyIORef dir (+0.1)
                         | k == Char 'd' = modifyIORef dir (+(-0.1))
                         | k == Char 'q' = exitWith ExitSuccess
                         | otherwise = return()

display :: IORef GLdouble -> IORef GLdouble -> IO()
display rot dir = do
  d <- readIORef dir
  modifyIORef rot (+d) -- 回転角度を更新する
  r <- readIORef rot
  clearColor $= Color4 0.0 0.0 0.0 0.0 -- 背景の色設定
  clear [ColorBuffer] -- 指定したbufferを特定の色で消去
  loadIdentity -- 現在の行列を単位行列にする。
  preservingMatrix $ do -- 複数の図形を描く
    rotate r (UT.Vector3 0.0 0.0 1.0 :: UT.Vector3 GLdouble) -- glRotated関数を呼ぶ。z軸を中心にrot分回転
    let color3f r g b = color $ Color3 r g (b :: GLfloat)
        vertex3f x y z = vertex $ UT.Vertex3 x y (z :: GLfloat)
    renderPrimitive TriangleStrip $ do -- TriangleStripで四角形を描く
      color3f 1 0 0
      vertex3f (-0.1) (-0.1) 0
      color3f 0 1 0
      vertex3f (-0.1) 0.1 0
      color3f 0 0 1
      vertex3f 0.1 (-0.1) 0
      color3f 1 1 1
      vertex3f 0.1 0.1 0.0 -- プリミティブを描く
  swapBuffers -- glutSwapBuffers関数呼び出し

timerProc :: IO() -> IO()
timerProc act = do
  act -- actを実行する
  addTimerCallback timerInterval $ timerProc act -- timerInterval[msec]ごとに実行

reshape :: Size -> IO()
reshape size@(Size w h) = do
  viewport $= (Position 0 0, size) -- glViewport関数を呼ぶ。左下原点でsizeの領域分描画
  matrixMode $= Projection -- 射影変換を行なう
  loadIdentity
  M.perspective 60.0 (fromIntegral w / fromIntegral h) 0.001 50.0 -- 別資料に記述
  M.lookAt (T.Vertex3 0.0 0.0 (-1.0)) (T.Vertex3 0.0 0.0 0.0) (T.Vector3 0.0 1.0 0.0) -- gluLookAt関数を呼ぶ。 (0,0,-1)の位置から(0,0,0)を見ていてy軸+方向が上となる視点。
  matrixMode $= Modelview 0 -- 別資料に記述

