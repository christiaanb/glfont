module Graphics.UI.Font.TextureGlyph 
  ( newGlyph
  , glyphRender
  , getKerning
  , addToVertexBuffer
  )
where

import Data.Maybe (fromJust)
import qualified Graphics.Rendering.OpenGL.GL as GL

import Graphics.UI.Font.Common
import Graphics.UI.Font.Types

newGlyph :: TextureGlyph
newGlyph =
  TextureGlyph
    { glyphCharCode     = error $ "CharCode not set"
    , glyphWidth        = 0
    , glyphHeight       = 0
    , glyphOffsetX      = 0
    , glyphOffsetY      = 0
    , glyphAdvanceX     = 0.0
    , glyphAdvanceY     = 0.0
    , glyphU0           = 0.0
    , glyphV0           = 0.0
    , glyphU1           = 0.0
    , glyphV1           = 0.0
    , glyphKerning      = []
    }

glyphRender ::
  TextureGlyph
  -> Markup
  -> Pen
  -> IO Pen
glyphRender tGlyph markup pen = do
  let x = (fromEnum $ penX pen) + (glyphOffsetX tGlyph) :: Int
  let y = (fromEnum $ penY pen) + (glyphOffsetY tGlyph) + (fromEnum $ muRise markup) :: Int
  let w = glyphWidth  tGlyph :: Int
  let h = glyphHeight tGlyph :: Int

  let u0 = glyphU0 tGlyph :: Float
  let v0 = glyphV0 tGlyph :: Float
  let u1 = glyphU1 tGlyph :: Float
  let v1 = glyphV1 tGlyph :: Float

  GL.renderPrimitive GL.Triangles $ do
    glTexCoord2f (u0, v0); glVertex2i (x,y);
    glTexCoord2f (u0, v1); glVertex2i (x,y-h);
    glTexCoord2f (u1, v1); glVertex2i (x+w,y-h);

    glTexCoord2f (u0, v0); glVertex2i (x, y);
    glTexCoord2f (u1, v1); glVertex2i (x+w,h-h);
    glTexCoord2f (u1, v0); glVertex2i (x+w,y);

  let pen' = Pen
        { penX = penX pen + glyphAdvanceX tGlyph + muSpacing markup 
        , penY = penY pen + glyphAdvanceY tGlyph
        }

  return pen'

addToVertexBuffer ::
  TextureFont
  -> TextureAtlas
  -> TextureGlyph
  -> VertexBuffer
  -> Maybe Markup
  -> Pen
  -> Float
  -> (Pen,VertexBuffer)
addToVertexBuffer tFont atlas tGlyph vBuffer markupM pen kerning = (pen'',vBuffer'')
  where
    (rise,spacing) = case markupM of
      Nothing -> (0,0)
      Just markup -> (muRise markup, muSpacing markup)

    pen' = pen {penX = penX pen + kerning}

    bgColor@(_,_,_,bgColorAlpha) = rgbaOfColor $ muBackgroundColor (fromJust markupM)

    -- Background
    vBuffer' = if (markupM /= Nothing && bgColorAlpha > 0)
      then
        let (Just markup) = markupM
            u0        = (fromIntegral . regionX . atlasBlack $ atlas) / 
                        (fromIntegral . atlasWidth $ atlas)
            v0        = (fromIntegral . regionY . atlasBlack $ atlas) / 
                        (fromIntegral . atlasHeight $ atlas)
            u1        = u0 + ((fromIntegral . regionWidth . atlasBlack $ atlas) / 
                                    (fromIntegral . atlasWidth $ atlas))
            v1        = v0 + ((fromIntegral . regionHeight . atlasBlack  $ atlas) / 
                                    (fromIntegral . atlasHeight $ atlas))
            x0        = penX pen
            y0        = penY pen + fontAscender tFont
            x1        = x0 + glyphAdvanceX tGlyph + muSpacing markup + kerning
            y1        = y0 + fontHeight tFont - fontLineGap tFont
            index     = length (vbVertices vBuffer)
            (r,g,b,a) = bgColor
            indices   = [index,index+1,index+2
                        ,index,index+2,index+3
                        ]
            vertices  = [TextureGlyphVertex (x0,y0,0) (u0,v0) (r,g,b,a)
                        ,TextureGlyphVertex (x0,y1,0) (u0,v1) (r,g,b,a)
                        ,TextureGlyphVertex (x1,y1,0) (u1,v1) (r,g,b,a)
                        ,TextureGlyphVertex (x1,y0,0) (u1,v0) (r,g,b,a)
                        ]
        in vBuffer { vbVertices = vbVertices vBuffer ++ vertices
                   , vbIndices  = vbIndices  vBuffer ++ map toEnum indices
                   }
      else
        vBuffer
    
    -- Actual glyph
    vBuffer'' = 
        let (r,g,b,a)     = case markupM of
                              Nothing     -> (1,1,1,1)
                              Just markup -> rgbaOfColor $ muBackgroundColor markup
            x0            = penX pen' + (fromIntegral $ glyphOffsetX tGlyph)
            y0            = penY pen' + (fromIntegral $ glyphOffsetY tGlyph) + rise
            x1            = x0        + (fromIntegral $ glyphWidth  tGlyph)
            y1            = y0        + (fromIntegral $ glyphHeight tGlyph)
            u0            = glyphU0 tGlyph
            v0            = glyphV0 tGlyph
            u1            = glyphU1 tGlyph
            v1            = glyphV1 tGlyph
            index         = length $ vbVertices vBuffer'
            indices       = [index,index+1,index+2
                            ,index,index+2,index+3
                            ]
            vertices      = [TextureGlyphVertex (x0,y0,0) (u0,v0) (r,g,b,a)
                            ,TextureGlyphVertex (x0,y1,0) (u0,v1) (r,g,b,a)
                            ,TextureGlyphVertex (x1,y1,0) (u1,v1) (r,g,b,a)
                            ,TextureGlyphVertex (x1,y0,0) (u1,v0) (r,g,b,a)
                            ]
        in vBuffer' { vbVertices = vbVertices vBuffer' ++ vertices
                    , vbIndices  = vbIndices  vBuffer' ++ map toEnum indices
                    }
    
    pen'' = pen' {penX = penX pen' + glyphAdvanceX tGlyph + spacing
                 ,penY = penY pen' + glyphAdvanceY tGlyph
                 }

getKerning ::
  TextureGlyph
  -> Char
  -> Float
getKerning tGlyph charCode = k
  where
    k = case (lookup charCode (glyphKerning tGlyph)) of
      Nothing -> 0
      Just a  -> a
