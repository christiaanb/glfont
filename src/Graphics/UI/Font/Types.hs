{-# LANGUAGE NamedFieldPuns #-}
module Graphics.UI.Font.Types where

import qualified Graphics.Rendering.OpenGL.GL as GL
import Data.ByteString (ByteString)
import Data.IORef (IORef)
import Data.Word (Word8)

data Node = Node
  { nodeX     :: Int
  , nodeY     :: Int
  , nodeWidth :: Int
  } deriving (Eq,Show)

data Region = Region
  { regionX :: Int
  , regionY :: Int
  , regionWidth :: Int
  , regionHeight ::Int
  } deriving (Eq,Show)

data TextureAtlas = 
  TextureAtlas 
  { atlasNodes   :: [Node]
  , atlasWidth   :: Int
  , atlasHeight  :: Int
  , atlasDepth   :: Int
  , atlasUsed    :: Int
  , atlasTexId   :: GL.TextureObject
  , atlasImgData :: ByteString
  , atlasBlack   :: Region
  } deriving (Eq)

instance Show TextureAtlas where
  show (TextureAtlas{atlasWidth,atlasHeight,atlasDepth,atlasUsed,atlasNodes}) =
    "= TextureAtlas =" ++ "\n" ++
    "  Width x Height x Depth: " ++ show (atlasWidth,atlasHeight,atlasDepth) ++ "\n" ++
    "  Used                  : " ++ show atlasUsed ++ "\n" ++
    "  Nodes                 : " ++ show atlasNodes ++ "\n"

data TextureFont = TextureFont
  { fontGlyphs     :: [TextureGlyph]
  , fontAtlas      :: IORef TextureAtlas
  , fontFileName   :: String
  , fontBold       :: Bool
  , fontItalic     :: Bool
  , fontSize       :: Float
  , fontGamma      :: Float
  , fontBlack      :: Region
  , fontAntiAlias  :: Bool
  , fontSubPixel   :: Bool
  , fontHinting    :: Bool
  , fontHeight     :: Float
  , fontLineGap    :: Float
  , fontAscender   :: Float
  , fontDescender  :: Float
  , fontLcdFilter  :: Bool
  , fontLcdWeights :: [Word8]
  }

type KerningPair = (Char, Float)

data Pen = Pen
  { penX :: Float
  , penY :: Float
  }

data TextureGlyph = TextureGlyph
  { glyphCharCode     :: Char
  , glyphWidth        :: Int
  , glyphHeight       :: Int
  , glyphOffsetX      :: Int
  , glyphOffsetY      :: Int
  , glyphAdvanceX     :: Float
  , glyphAdvanceY     :: Float
  , glyphU0           :: Float
  , glyphV0           :: Float
  , glyphU1           :: Float
  , glyphV1           :: Float
  , glyphKerning      :: [KerningPair]
  }

data TextureGlyphVertex = TextureGlyphVertex
  { glyphVertexXYZ  :: (Float,Float,Float)
  , glyphVertexUV   :: (Float,Float)
  , glyphVertexRGBA :: (Float,Float,Float,Float)
  }

data Color = RGBA Float Float Float Float
  deriving Eq

data Markup = Markup
  { muFamily             :: String
  , muSize               :: Float
  , muBold               :: Bool
  , muItalic             :: Bool
  , muRise               :: Float
  , muSpacing            :: Float
  , muForegroundColor    :: Color
  , muBackgroundColor    :: Color
  , muOutlineColor       :: Maybe Color
  , muUnderlineColor     :: Maybe Color
  , muOverlineColor      :: Maybe Color
  , muStrikethroughColor :: Maybe Color
  } deriving Eq

data VertexAttribute = VertexAttribute
  { vaTarget     :: GL.GLenum
  , vaCTargert   :: GL.GLchar
  , vaIndex      :: GL.GLuint
  , vaSize       :: GL.GLint
  , vaType       :: GL.GLenum
  , vaNormalized :: GL.GLboolean
  , vaStride     :: GL.GLsizei
  }

data VertexBuffer = VertexBuffer
  { vbFormat     :: String
  , vbVertices   :: [TextureGlyphVertex]
  , vbVerticesId :: GL.GLuint
  , vbIndices    :: [GL.GLuint]
  , vmIndicesId  :: GL.GLuint
  , vmDirty      :: Bool
  , vmAttributes :: [VertexAttribute] 
  }

data FontManager = FontManager
  { fmAtlas :: IORef TextureAtlas
  , fmFonts :: [TextureFont]
  , fmCache :: String
  , fmBlack :: Region
  }
