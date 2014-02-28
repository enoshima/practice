import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL.GLU

import Data.IORef

-- timer interval
timerInterval = 40

main = do
    -- initialize rotation angle
    rot <- newIORef 0.0

    -- initialize GLUT
    initialDisplayMode $= [RGBMode,  DoubleBuffered]
    initialWindowSize  $= Size 640 480

    -- make window
    createWindow "guruGuru"

    -- set callback function for display
    displayCallback $= display rot

    -- set callback to call when changing window size
    reshapeCallback $= Just reshape

    -- make timer
    addTimerCallback timerInterval $ timerProc (display rot)

    -- in GLUT loop main
    mainLoop

display rot = do
    -- rotate
    modifyIORef rot (+14.4)
    r <- readIORef rot

    -- black background
    clearColor $= Color4 0.0 0.0 0.0 0.0
    clear [ColorBuffer]

    -- read identical matrix
    loadIdentity

    -- display
    preservingMatrix $ do
      rotate r (Vector3 0.0 0.0 1.0 :: Vector3 GLdouble)
      renderPrimitive Quads $ mapM_ vertex [
                  Vertex3 0.10 0.10 0.0,
                  Vertex3 (-0.10) 0.10 0.0,
          Vertex3 (-0.10) (-0.10) 0.0,
          Vertex3 0.10 (-0.10) 0.0 :: Vertex3 GLfloat]

    -- change Buffer
    swapBuffers

-- repeat act when timer called
timerProc act = do
    act
    addTimerCallback timerInterval $ timerProc act

-- twiddle when changing window size
reshape size@(Size w h) = do
    viewport $= (Position 0 0,  size) -- use entire window

    -- set view volume
    matrixMode $= Projection
    loadIdentity
    perspective 60.0 (fromIntegral w / fromIntegral h) 0.001 50.0

    -- view behind
    lookAt (Vertex3 0.0 0.0 (-1.0)) (Vertex3 0.0 0.0 0.0) (Vector3 0.0 1.0 0.0)
    matrixMode $= Modelview 0










