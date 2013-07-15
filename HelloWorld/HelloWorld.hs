import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
 
main :: IO ()
main = do
  (progname, _) <- getArgsAndInitialize -- OpenGLの初期化
  createWindow "Hello World"            -- windowを開く。この引数はwindowのタイトル
  displayCallback $= display            -- windowに関するメイン画面の関数。$=を使ってdisplay関数を設定
  mainLoop                              -- mainLoopがOpenGLの責任を引き受ける。displayCallbackはwindowの内容を描く
 
display :: IO ()                        -- これはdisplay関数の定義。OpenGLのいくつかの関数を呼び出している
display = do
  clear [ColorBuffer]                   -- clear関数は描画の色の状態を空にする(空のキャンバスを得ることができる)
  flush                                 -- flush関数は描画処理を行った後更新する役目がある

