{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NamedFieldPuns #-}
-- | This TextureAtlas structure is responsible for the packing of small 
-- regions into a bigger texture. It is based on the skyline bottom left
-- algorith which appears to be well suited for storing glyps.
module Graphics.UI.Font.TextureAtlas 
  ( TextureAtlas (..)
  , newAtlas
  , deleteTexture
  , uploadTexture
  , setRegion
  , merge
  , getRegion
  , clear
  )
where

import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.List as List
import Graphics.Rendering.OpenGL.GL (($=))
import qualified Graphics.Rendering.OpenGL.GL as GL

import Graphics.UI.Font.Common
import Graphics.UI.Font.Types

specialsDim :: Int
specialsDim = 4

-- | Create a new atlas
newAtlas ::
  Int -- ^ Width of the atlas texture
  -> Int -- ^ Height of the atlas texture
  -> Int -- ^ Color depth, should be 1 or 3
  -> TextureAtlas
newAtlas 
  width
  height
  depth 
  | depth == 1 || depth == 3 = atlas3
  where
    node = Node 0 0 width

    atlas = TextureAtlas
      { atlasNodes   = [node]
      , atlasUsed    = 0
      , atlasWidth   = width
      , atlasHeight  = height
      , atlasDepth   = depth
      , atlasTexId   = GL.TextureObject (-1)
      , atlasImgData = B.replicate (width * height * depth) 0
      , atlasBlack   = undefined
      }
    
    -- This is a special region that is used for background and underlined 
    -- decorations of glyps
    buffer  = B.replicate (specialsDim^(2::Int)) 255
    (atlas',r@(Region x y w h)) = 
      getRegion atlas specialsDim specialsDim
    atlas'' = setRegion atlas' r buffer specialsDim

    atlas3  = atlas'' {atlasBlack = Region (x+1) (y+1) (w-2) (h-2)}

newAtlas _ _ depth 
  = error $ "Invalid depth, should be 1 or 3, but is: " ++ show depth

-- | Delete the GL Texture reference
deleteTexture ::
  TextureAtlas
  -> IO ()
deleteTexture (TextureAtlas{atlasTexId}) = do
  isNotDeleted <- GL.isObjectName atlasTexId
  when isNotDeleted $
    GL.deleteObjectNames [atlasTexId]

-- | Create or refresh OpenGL text reference to the bitmap information
-- stored in the atlas
uploadTexture ::
  TextureAtlas
  -> IO TextureAtlas
uploadTexture atlas@(TextureAtlas{atlasTexId,atlasImgData,atlasWidth,atlasHeight,atlasDepth}) = do
  texIdDefined <- GL.isObjectName $ atlasTexId

  (texId,atlas') <- if texIdDefined
    then return (atlasTexId, atlas)
    else do
      [texId] <- GL.genObjectNames 1
      return (texId,atlas {atlasTexId = texId})
  
  B.useAsCString atlasImgData $ \ptr -> do
      GL.texture GL.Texture2D $= GL.Enabled
      GL.textureBinding GL.Texture2D $= Just texId

      case atlasDepth of
        1 -> GL.texImage2D
              Nothing
              GL.NoProxy
              0
              GL.Alpha'
              (GL.TextureSize2D
                (gsizei atlasWidth)
                (gsizei atlasHeight)
                )
              0
              (GL.PixelData GL.Alpha GL.UnsignedByte ptr)
        _ -> GL.texImage2D
              Nothing
              GL.NoProxy
              0
              GL.RGB'
              (GL.TextureSize2D
                (gsizei atlasWidth)
                (gsizei atlasHeight)
                )
              0
              (GL.PixelData GL.RGB GL.UnsignedByte ptr)

      GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')

      GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.ClampToEdge)
      GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.ClampToEdge)
      GL.textureBinding GL.Texture2D $= Nothing
      GL.texture GL.Texture2D $= GL.Disabled
  
  return atlas'

setRegion ::
  TextureAtlas
  -> Region
  -> ByteString -> Int
  -> TextureAtlas
setRegion atlas@(TextureAtlas{atlasWidth,atlasHeight,atlasDepth,atlasImgData}) (Region x y width height) buf stride
  | x < atlasWidth
  , (x + width) <= atlasWidth
  , y < atlasHeight
  , (y + height) <= atlasHeight
  = atlas'
  where
    imgData     = splitRows atlasImgData (atlasWidth * atlasDepth)
    buf'        = splitRows buf          (stride     * atlasDepth)
    (pre,rep)   = splitAt y imgData
    (rep',post) = splitAt height rep
    rep''       = zipWith (replace x width) rep' buf'
    atlas'      = atlas {atlasImgData = B.concat $ pre ++ rep'' ++ post}
  


setRegion _ _ _ _ = error $ "Region does not fit"

splitRows ::
  ByteString
  -> Int
  -> [ByteString]
splitRows b w 
  | not (B.null b) 
  = let (pre,post) = B.splitAt w b
    in   pre:(splitRows post w)

  | otherwise    
  = []

replace ::
  Int -> Int
  -> ByteString
  -> ByteString
  -> ByteString
replace x w b1 b2 = B.concat [pre,b2,post]
  where
    (pre,rep) = B.splitAt x b1
    (_,post)  = B.splitAt w rep

-- | Decide, given an atlas, a node index, and the with and height of the
-- rectangle you want to fit, will fit at that node index. A rectangle fits
-- if, nodeX + width < atlaswidth, and max { nodeY all nodes covered by 
-- nodeX + width } + height < atlasheight. If it fits, the bottem-left
-- rectangle coord is returned.
atlasFit ::
  TextureAtlas
  -> Int -> Int
  -> Node 
  -> Int
  -> Maybe Int
atlasFit TextureAtlas{atlasWidth,atlasHeight,atlasNodes} width height (Node x y _) index = fit
  where
    fit = if (x + width) > atlasWidth || yMax + height > atlasHeight
      then Nothing
      else Just yMax
    
    -- Get node with highest y, in x range [nodeX .. nodeX+width]
    (_,yMax,_) =
      until (\(n,_,_) -> n <= 0)    
      (\(width',yMax',((Node _ y' w'):ns)) -> (width' - w', max yMax' y', ns))
      (width, y,drop index atlasNodes)

merge ::
  TextureAtlas
  -> TextureAtlas
merge atlas = atlas {atlasNodes = nodes'}
  where
    nodes = atlasNodes atlas
    nodes' = mergeNodes nodes

    mergeNodes []       = []
    mergeNodes [x]      = [x]
    mergeNodes (x:y:xs) = let
        (Node x1 y1 w1) = x
        (Node _  y2 w2) = y
      in
        if (y1 == y2)
          then mergeNodes ((Node x1 y1 (w1+w2)):xs)
          else x : mergeNodes (y:xs)

getRegion ::
  TextureAtlas
  -> Int -> Int
  -> (TextureAtlas,Region)
getRegion atlas@(TextureAtlas {atlasUsed,atlasNodes}) width height = (atlas',region)
  where
    potentialFits = zipWith (atlasFit atlas width height) atlasNodes [0..]
    (bestIndex,bestFit,maxYM) = head $ 
      List.sortBy betterFit (zip3 [0..] atlasNodes potentialFits)

    region = case (bestFit,maxYM) of
      (_,Nothing) -> Region (-1) (-1) 0 0
      ((Node x _ _), Just maxY) -> Region x maxY width height
    
    atlas' = case (bestFit,maxYM) of
      (_,Nothing) -> atlas
      ((Node x _ _), Just maxY) -> let
          newNode    = Node x (maxY+height) width
          (pre,post) = List.splitAt bestIndex atlasNodes
          post'      = shrink newNode post
        in
          atlas { atlasNodes = pre ++ (newNode:post')
                , atlasUsed  = atlasUsed + width * height
                }

betterFit ::
  (Int,Node,Maybe Int)
  -> (Int,Node,Maybe Int)
  -> Ordering
betterFit (_,_,Nothing) (_,_,Nothing) = EQ
betterFit (_,_,Nothing) (_,_,Just _ ) = GT
betterFit (_,_,Just _ ) (_,_,Nothing) = LT
betterFit (_,Node _ _ w1,Just y1)
          (_,Node _ _ w2,Just y2) = compare (y1,w1) (y2,w2)

-- | Shrink '(n:ns)' to fit 'np' 
shrink :: 
  Node 
  -> [Node] 
  -> [Node]    
shrink np@(Node xP _ wP) ((n@(Node x y w)):ns)
  | x >= xP + wP = n:ns
  | w' <= 0      = shrink np ns
  | otherwise    = (Node x' y w'):ns
  where
    s  = xP + wP - x
    x' = x + s
    w' = w - s

shrink _ [] = []

clear ::
  TextureAtlas
  -> TextureAtlas
clear atlas@(TextureAtlas{atlasWidth,atlasHeight,atlasDepth}) = atlas4
  where
    node = Node 0 0 atlasWidth

    atlas' = atlas
      { atlasNodes   = [node]
      , atlasUsed    = 0
      , atlasImgData = B.replicate (atlasWidth * atlasHeight * atlasDepth) 0
      }

    (atlas'',r@(Region x y w h)) = 
      getRegion atlas' specialsDim specialsDim

    buffer  = B.replicate (specialsDim^(2 :: Int)) 255

    atlas3 = setRegion atlas'' r buffer 1

    atlas4  = atlas3 {atlasBlack = Region (x+1) (y+1) (w-2) (h-2)}
