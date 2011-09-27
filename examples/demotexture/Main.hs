module Main where

import Data.IORef (newIORef,readIORef)
import Control.Monad (forM_,unless)
import Graphics.Rendering.OpenGL (($=),get)
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

import Graphics.UI.Font

fontLocation :: String
fontLocation = "C:\\Windows\\Fonts\\verdana.ttf"

display ::
  GL.TextureObject
  -> IO ()
display tex = do
  (_, GL.Size w h) <- get GL.viewport

  GL.clear [GL.ColorBuffer,GL.DepthBuffer]

  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.texture GL.Texture2D $= GL.Enabled
  GL.textureBinding GL.Texture2D $= Just tex

  GL.renderPrimitive GL.Quads $ do
    GL.color $ GL.Color4 0.0 0.0 0.0 (1.0 :: GL.GLdouble)
    glTexCoord2f (0,1) >> glVertex2i (0,0)
    glTexCoord2f (0,0) >> glVertex2i (0,fromIntegral h)
    glTexCoord2f (1,0) >> glVertex2i (fromIntegral w,fromIntegral h)
    glTexCoord2f (1,1) >> glVertex2i (fromIntegral w,0)

  GL.textureBinding GL.Texture2D $= Nothing
  GL.texture GL.Texture2D $= GL.Disabled
  GL.blend $= GL.Disabled


reshape :: 
  Int 
  -> Int 
  -> IO ()
reshape w h = do
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  GL.ortho 0 (realToFrac w) 0 (realToFrac h) (-1) 1
  GL.matrixMode $= GL.Modelview 0

main :: IO ()
main = do
  _ <- GLFW.initialize
  _ <- GLFW.openWindow $
        GLFW.defaultDisplayOptions
          { GLFW.displayOptions_width  = 512
          , GLFW.displayOptions_height = 512 
          }
  
  GLFW.setWindowDimensions 512 512

  GLFW.setWindowTitle "FreeType OpenGL"

  GLFW.setWindowSizeCallback reshape

  let cache = " !\"#$%&'()*+,-./0123456789:;<=>?" ++
              "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_" ++
              "`abcdefghijklmnopqrstuvwxyz{|}~"
  
  atlas <- newIORef $ newAtlas 512 512 1

  forM_ [8..25] $ \i -> do
    font <- newFont atlas fontLocation i
    cacheGlyphs font cache

  GL.clearColor $= GL.Color4 1.0 1.0 1.0 1.0

  tex <- fmap atlasTexId $ readIORef atlas
  loop (display tex)

  GLFW.closeWindow

loop ::
  IO ()
  -> IO ()
loop render = do
  render
  GLFW.swapBuffers
  p <- GLFW.keyIsPressed GLFW.KeyEsc
  unless p $ do
    GLFW.sleep 0.001
    windowOpenStatus <- GLFW.windowIsOpen
    unless (not windowOpenStatus) $
      loop render

glTexCoord2f ::
  (GL.GLfloat, GL.GLfloat)
  -> IO ()
glTexCoord2f (x,y) = GL.texCoord (GL.TexCoord2 x y :: GL.TexCoord2 GL.GLfloat)

glVertex2i ::
  (GL.GLint, GL.GLint)
  -> IO ()
glVertex2i (x,y)   = GL.vertex (GL.Vertex2 x y :: GL.Vertex2 GL.GLint)
