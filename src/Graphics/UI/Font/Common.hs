module Graphics.UI.Font.Common where

import qualified Graphics.Rendering.OpenGL.GL as GL
import Unsafe.Coerce

import Graphics.UI.Font.Types

gsizei :: Int -> GL.GLsizei
{-# INLINE gsizei #-}
gsizei x = unsafeCoerce x

gi :: Int -> GL.GLint
{-# INLINE gi #-}
gi x = unsafeCoerce x

gf :: Float -> GL.GLfloat
{-# INLINE gf #-}
gf x = unsafeCoerce x

glTexCoord2f :: 
  (Float,Float)
  -> IO ()
glTexCoord2f (x,y) = GL.texCoord (GL.TexCoord2 (gf x) (gf y) :: GL.TexCoord2 GL.GLfloat)

glVertex2i ::
  (Int,Int)
  -> IO ()
glVertex2i (x,y)   = GL.vertex (GL.Vertex2 (gi x) (gi y) :: GL.Vertex2 GL.GLint)

rgbaOfColor :: Color -> (Float, Float, Float, Float)
rgbaOfColor (RGBA r g b a) = (r,g,b,a)