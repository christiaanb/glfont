module Graphics.UI.Font.Types where

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
  } deriving (Eq,Show)

data TextureFont = TextureFont
  { fontGlyps :: [TextureGlyph]
  , fontAtlas :: TextureAtlas
  , fontFileName :: String
  , fontBold  :: Bool
  , fontItalic :: Bool
  , fontSize :: Float
  , fontGamm :: Float
  , fontBlack :: Region
  , fontAntiAlias :: Bool
  , fontSubPixel :: Bool
  , fontHinting :: Bool
  , fontHeight :: Float
  , fontLineGap :: Float
  , fontAscender :: Float
  , fontDescender :: Float
  }

data KerningPair = KerningPair
  { kerningCharCode :: Char
  , kerning         :: Float
  }

data Pen = Pen
  { penX :: Float
  , penY :: Float
  }

data TextureGlyph = TextureGlyph
  { glyphCharCode :: Char
  , glyphWidth    :: Int
  , glyphHeight   :: Int
  , glyphOffsetX  :: Int
  , glyphOffsetY  :: Int
  , glyphAdvanceX :: Float
  , glyphAdvanceY :: Float
  , glyphU0 :: Float
  , glyphV0 :: Float
  , glyphU1 :: Float
  , glyphV1 :: Float
  , glyphKerning :: KerningPair
  , glyphKerningCount :: Int
  , glyphTextureFont :: TextureFont
  }

data TextureGlyphVertex = TextureGlyphVertex
  { glyphVertexXYZ  :: (Float,Float,Float)
  , glyphVertexUV   :: (Float,Float)
  , glyphVertexRGBA :: (Float,Float,Float,Float)
  }
