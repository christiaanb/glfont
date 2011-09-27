module Graphics.UI.Font.FontManager
  ( newFontManager
  , deleteFontManager
  , getFromFilename
  , FontManager(..)
  )
where

import Data.IORef(IORef,writeIORef,readIORef)

import Graphics.UI.Font.TextureAtlas
import Graphics.UI.Font.TextureFont
import Graphics.UI.Font.Types

newFontManager ::
  IORef TextureAtlas
  -> Int
  -> Int
  -> Int
  -> IO FontManager
newFontManager atlas width height depth = do
  let atlas' = newAtlas width height depth
  writeIORef atlas atlas'

  let fontManager = FontManager
        { fmAtlas = atlas
        , fmFonts = []
        , fmCache = " "
        , fmBlack = error "Black region not defined"
        }
  
  return fontManager

deleteFontManager ::
  FontManager
  -> IO ()
deleteFontManager fm = (readIORef $ fmAtlas fm) >>= deleteTexture

getFromFilename ::
  FontManager
  -> FilePath
  -> Float
  -> IO (TextureFont,FontManager)
getFromFilename fm fileName size = do
  let fontM = filter (\f -> fontFileName f == fileName && fontSize f == size) (fmFonts fm)
  case fontM of
    (f:_) -> return (f,fm)
    [] -> do
      font  <- newFont (fmAtlas fm) fileName size
      font' <- cacheGlyphs font (fmCache fm)
      return (font', fm {fmFonts = font':(fmFonts fm)})
