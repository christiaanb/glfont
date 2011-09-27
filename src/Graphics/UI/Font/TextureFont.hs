module Graphics.UI.Font.TextureFont
  ( newFont
  , generateKerning
  , cacheGlyphs
  , getGlyph
  )
where

-- External Modules
import Control.Monad (foldM)
import Data.Bits ((.|.))
import qualified Data.Bits as Bits
import qualified Data.ByteString as B
import Data.IORef (IORef,readIORef,writeIORef)
import Debug.Trace (putTraceMsg)
import Foreign (Ptr,peek,poke,mallocForeignPtr,alloca,withForeignPtr,nullPtr)
import Foreign.C.String (withCString)
import Foreign.C.Types (CUInt)

-- FreeType2 Library
import qualified Graphics.Rendering.FreeType.Internal                as FT
import qualified Graphics.Rendering.FreeType.Internal.Bitmap         as FT_Bitmap
import Graphics.Rendering.FreeType.Internal.Face (FT_Face)
import qualified Graphics.Rendering.FreeType.Internal.Face           as FT_Face
import qualified Graphics.Rendering.FreeType.Internal.GlyphSlot      as FT_GlyphSlot
import Graphics.Rendering.FreeType.Internal.Library (FT_Library)
import qualified Graphics.Rendering.FreeType.Internal.Matrix         as FT_Matrix
import Graphics.Rendering.FreeType.Internal.PrimitiveTypes (FT_Error)
import qualified Graphics.Rendering.FreeType.Internal.PrimitiveTypes as FT_Prim
import qualified Graphics.Rendering.FreeType.Internal.Size           as FT_Size
import qualified Graphics.Rendering.FreeType.Internal.SizeMetrics    as FT_SizeMetics
import Graphics.Rendering.FreeType.Internal.Vector (FT_Vector)
import qualified Graphics.Rendering.FreeType.Internal.Vector         as FT_Vector

-- Internal Modules
import Graphics.UI.Font.TextureAtlas
import Graphics.UI.Font.TextureGlyph
import Graphics.UI.Font.Types

newFont ::
  IORef TextureAtlas
  -> FilePath
  -> Float
  -> IO TextureFont
newFont atlas fileName size = do
  libraryFPtr <- mallocForeignPtr
  faceFPtr    <- mallocForeignPtr

  withForeignPtr libraryFPtr $ \libraryPtr -> do
  withForeignPtr faceFPtr    $ \facePtr -> do
    face <- fontLoadFace libraryPtr facePtr fileName (size*100)

    faceSize <- peek $ FT_Face.size face
    metrics  <- peek $ FT_Size.metrics faceSize

    let height    = fromIntegral (FT_SizeMetics.height    metrics `Bits.shiftR` 6) / 100
    let ascender  = fromIntegral (FT_SizeMetics.ascender  metrics `Bits.shiftR` 6) / 100
    let descender = fromIntegral (FT_SizeMetics.descender metrics `Bits.shiftR` 6) / 100

    let font = TextureFont
                { fontGlyphs     = []
                , fontAtlas      = atlas
                , fontFileName   = fileName
                , fontBold       = error "Bold info not set"
                , fontItalic     = error "Italic info not set"
                , fontSize       = size
                , fontGamma      = 1
                , fontBlack      = error "Font black region not set"
                , fontAntiAlias  = error "Font AntiAlias not set"
                , fontSubPixel   = error "Font SubPixel not set"
                , fontHinting    = True
                , fontHeight     = height
                , fontLineGap    = height - ascender + descender
                , fontAscender   = ascender
                , fontDescender  = descender
                , fontLcdFilter  = True
                , fontLcdWeights = [0,0,255,0,0]
                }
    
    freeFace face
    library' <- peek libraryPtr
    freeLibrary library'

    return font

generateKerning ::
  TextureFont
  -> IO TextureFont
generateKerning tFont = do
  libraryFPtr <- mallocForeignPtr
  faceFPtr    <- mallocForeignPtr

  withForeignPtr libraryFPtr $ \libraryPtr -> do
  withForeignPtr faceFPtr    $ \facePtr -> do
    face <- fontLoadFace 
      libraryPtr 
      facePtr 
      (fontFileName tFont) 
      (fontSize tFont)
    
    fontGlyphs' <- alloca $ \kerningPtr -> do
      mapM (generateGlyphKerning face tFont kerningPtr) (fontGlyphs tFont)

    freeFace face
    library' <- peek libraryPtr
    freeLibrary library'

    return $ tFont {fontGlyphs = fontGlyphs'}

generateGlyphKerning ::
  FT_Face
  -> TextureFont
  -> Ptr FT_Vector
  -> TextureGlyph
  -> IO TextureGlyph
generateGlyphKerning face tfont kerningPtr glyph = do
  
  glyphIndex <- FT.ft_Get_Char_Index face (fromIntegral . fromEnum $ glyphCharCode glyph)

  kpairs <- foldM (\ks g -> do
      prevIndex <- FT.ft_Get_Char_Index face (fromIntegral . fromEnum $ glyphCharCode g)
      kernError <- FT.ft_Get_Kerning face prevIndex glyphIndex 1 kerningPtr
      kerning <- if kernError == 0 
        then peek kerningPtr
        else fail "Failed to get kerning information"
      case (FT_Vector.x kerning) of
        0  -> return ks
        xK -> return ((glyphCharCode g, (fromIntegral xK) / (64*64)):ks))
    []
    (fontGlyphs tfont)
  
  case kpairs of
    []  -> return glyph
    kps -> return $ glyph {glyphKerning = kps}

cacheGlyphs ::
  TextureFont
  -> String
  -> IO TextureFont
cacheGlyphs tFont charCodes = do
  libraryFPtr <- mallocForeignPtr
  faceFPtr    <- mallocForeignPtr

  withForeignPtr libraryFPtr $ \libraryPtr -> do
  withForeignPtr faceFPtr    $ \facePtr -> do
    face <- fontLoadFace 
      libraryPtr 
      facePtr 
      (fontFileName tFont) 
      (fontSize tFont)

    depth <- fmap atlasDepth $ readIORef $ fontAtlas tFont
    width <- fmap atlasDepth $ readIORef $ fontAtlas tFont
    height <- fmap atlasHeight $ readIORef $ fontAtlas tFont

    tfont' <- foldM (loadGlyph face width height depth) tFont charCodes

    freeFace face
    library' <- peek libraryPtr
    freeLibrary library'

    atlas' <- readIORef (fontAtlas tfont') >>= uploadTexture
    writeIORef (fontAtlas tfont') atlas'
    tfont'' <- generateKerning tfont'

    return tfont''

loadGlyph ::
  FT_Face
  -> Int
  -> Int
  -> Int
  -> TextureFont
  -> Char
  -> IO TextureFont
loadGlyph face width height depth tfont charCode = do
  glyphIndex <- FT.ft_Get_Char_Index face (fromIntegral . fromEnum $ charCode)
  let flags = FT_Prim.ft_LOAD_RENDER .|.
        if (fontHinting tfont)
          then FT_Prim.ft_LOAD_FORCE_AUTOHINT
          else FT_Prim.ft_LOAD_NO_HINTING .|. FT_Prim.ft_LOAD_NO_AUTOHINT

  flags' <- if (depth == 3) 
    then do
      --ft_Library_SetLcdFilter library ft_LCD_FILTER_LIGHT >>= printOnFail "SetLcdFilter"
      --when (fontLcdFilter tfont)
      --  ft_Library_SetLcdFilterWeights library fontLcdWeights >>= printOnFail "SetLcdFilterWeights"
      return $ flags .|. (fromIntegral $ fromEnum FT_Prim.ft_LOAD_TARGET_LCD)
    else
      return flags
  
  glyphError <- FT.ft_Load_Glyph face glyphIndex flags'
  if glyphError == 0
    then do
      slot <- peek $ FT_Face.glyph face
      
      -- Gamma correction (sort of)
      bitmapWidth   <- fmap FT_Bitmap.width  $ peek $ FT_GlyphSlot.bitmap slot
      bitmapRows    <- fmap FT_Bitmap.rows   $ peek $ FT_GlyphSlot.bitmap slot
      bitmapBuffer  <- fmap FT_Bitmap.buffer $ peek $ FT_GlyphSlot.bitmap slot
      bitmapBuffer' <- B.packCStringLen (bitmapBuffer,fromEnum $ bitmapWidth*bitmapRows)
      let bitmapBuffer'' = B.map 
            (\c -> round $ ((fromIntegral c / 255.0) ** (fontGamma tfont)) * 255 ) 
            bitmapBuffer'

      -- We want each pixel to be seperated by at least one black pixel
      let w = (fromEnum bitmapWidth) `div` depth + 1
      let h = (fromEnum bitmapRows) + 1
      (atlas',region) <- fmap (\a -> getRegion a w h) $ readIORef (fontAtlas tfont)

      if regionX region < 0 
        then
          fail "Atlas if full"
        else do
          bitmapPitch <- fmap FT_Bitmap.pitch $ peek $ FT_GlyphSlot.bitmap slot
          let atlas'' = setRegion 
                atlas' 
                (Region (regionX region) (regionY region) (w-1) (h-1))
                bitmapBuffer''
                (fromEnum bitmapPitch)
          
          bitmapL <- peek $ FT_GlyphSlot.bitmap_left slot
          bitmapT <- peek $ FT_GlyphSlot.bitmap_top slot

          let glyph = newGlyph
                { glyphCharCode = charCode
                , glyphKerning  = []
                , glyphWidth    = (w - 1)
                , glyphHeight   = (h - 1)
                , glyphOffsetX  = fromEnum bitmapL
                , glyphOffsetY  = fromEnum bitmapT
                , glyphU0       = (fromIntegral (regionX region)) / (fromIntegral width)
                , glyphV0       = (fromIntegral (regionY region)) / (fromIntegral height)
                , glyphU1       = (fromIntegral (regionX region + (w-1))) / (fromIntegral width)
                , glyphV1       = (fromIntegral (regionY region + (h-1))) / (fromIntegral height)
                }
          
          let noHintFlags = FT_Prim.ft_LOAD_RENDER .|. FT_Prim.ft_LOAD_NO_HINTING

          FT.ft_Load_Glyph face glyphIndex noHintFlags >>= printOnFail "Load Glyph no hinting failed"
          slot' <- peek $ FT_Face.glyph face

          advX <- fmap FT_Vector.x $ peek $ FT_GlyphSlot.advance slot'
          advY <- fmap FT_Vector.y $ peek $ FT_GlyphSlot.advance slot'

          let glyph' = glyph
                { glyphAdvanceX = (fromIntegral advX) / 64.0
                , glyphAdvanceY = (fromIntegral advY) / 64.0
                }
          
          writeIORef (fontAtlas tfont) atlas''
          let tfont' = tfont {fontGlyphs = glyph':(fontGlyphs tfont)}

          return tfont'
    else
      fail $ "Can't load glyph: " ++ show charCode

getGlyph ::
  TextureFont
  -> Char
  -> IO (TextureFont, TextureGlyph)
getGlyph tFont charCode = do
  let glyphM = filter (\g -> glyphCharCode g == charCode) (fontGlyphs tFont)
  case glyphM of
    [g] -> return (tFont, g)
    []  -> do
      tFont' <- cacheGlyphs tFont [charCode]
      return (tFont', head $ fontGlyphs tFont')
    _ -> fail "found more than one glyph" 

fontLoadFace ::
  Ptr FT_Library
  -> Ptr FT_Face
  -> FilePath
  -> Float
  -> IO FT_Face
fontLoadFace libraryPtr facePtr filePath size = do
  let hres = 64 :: CUInt

  matrix <- mallocForeignPtr
  withForeignPtr matrix $ \p -> poke p
    (FT_Matrix.FT_Matrix
      {FT_Matrix.xx = round $ (1.0 / toRational hres) * 0x10000
      ,FT_Matrix.xy = round $ (0.0 :: Double)         * 0x10000
      ,FT_Matrix.yx = round $ (0.0 :: Double)         * 0x10000
      ,FT_Matrix.yy = round $ (1.0 :: Double)         * 0x10000
      }
    )
  
  -- Lood library
  libraryError <- FT.ft_Init_FreeType libraryPtr

  if (libraryError == 0) 
    then do
      -- Load Face
      library <- peek libraryPtr
      faceError <- withCString filePath $ \str -> do
        FT.ft_New_Face library str 0 facePtr
      
      -- Select CharMap
      if (faceError == 0)
        then do
          face <- peek facePtr
          charError <- FT.ft_Select_Charmap face FT_Prim.ft_ENCODING_UNICODE

          -- Set CharSize
          if (charError == 0)
            then do
              let size' = fromIntegral $ fromEnum size
              sizeError <- FT.ft_Set_Char_Size face (size' * 64) 0 (72 * hres) 72
              
              -- Transform
              if (sizeError == 0)
                then do
                  withForeignPtr matrix $ \mp ->
                    FT.ft_Set_Transform face mp nullPtr

                  return face
                else do
                  freeFace face
                  freeLibrary library
                  fail $ "Failed to set char size to: " ++ show size
            else do
              freeFace face 
              freeLibrary library
              fail "Failed to set unicode encoding"
        else do
          freeLibrary library
          fail $ "Failed to load face: " ++ filePath
    else 
      fail "Failed to initialize FreeType2 library."

printOnFail :: 
  String ->
  FT_Error ->
  IO ()
printOnFail msg e
  | e == 0    = return ()
  | otherwise = putTraceMsg msg

freeFace ::
  FT_Face
  -> IO ()
freeFace face = FT.ft_Done_Face face >>= printOnFail "Failed destroying face"

freeLibrary ::
  FT_Library
  -> IO ()
freeLibrary library = FT.ft_Done_FreeType library >>= printOnFail "Failed destroying library"
