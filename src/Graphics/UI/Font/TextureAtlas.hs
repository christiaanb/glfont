module Graphics.UI.Font.TextureAtlas 
  ( atlasNew
  , atlasDelete
  , atlasUpload
  , atlasSetRegion
  , atlasMerge
  , atlasGetRegion
  )
where

import Graphics.Rendering.OpenGL.GL (($=))
import qualified Graphics.Rendering.OpenGL.GL as GL
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.List as List
import qualified Data.Maybe as Maybe

import Unsafe.Coerce

import Foreign
import Foreign.C.Types

import Graphics.UI.Font.Types

atlasNew ::
  Int
  -> Int
  -> Int
  -> TextureAtlas
atlasNew width height depth | depth == 1 || depth == 3 = atlas3
  where
    node = Node 0 0 width

    atlas = TextureAtlas
      { atlasNodes  = [node]
      , atlasUsed   = 0
      , atlasWidth  = width
      , atlasHeight = height
      , atlasDepth  = depth
      , atlasTexId  = GL.TextureObject 0
      , atlasImgData = B.replicate (width * height * depth) 0
      , atlasBlack = undefined
      }
    
    n       = 4
    (atlas',Region x y w h) = atlasGetRegion atlas n n
    buffer  = B.replicate (n*n) 255
    atlas'' = atlasSetRegion atlas x y w h buffer 1

    atlas3  = atlas'' {atlasBlack = Region (x+1) (y+1) (w-2) (h-2)}

atlasDelete ::
  TextureAtlas
  -> IO ()
atlasDelete atlas = do
  let texId = atlasTexId atlas
  isNotDeleted <- GL.isObjectName texId
  if isNotDeleted
    then GL.deleteObjectNames [texId]
    else return ()

atlasUpload ::
  TextureAtlas
  -> IO TextureAtlas
atlasUpload atlas = do
  texIdDefined <- GL.isObjectName $ atlasTexId atlas

  (texId,atlas') <- if texIdDefined
    then return (atlasTexId atlas, atlas)
    else do
      [texId] <- GL.genObjectNames 1
      return (texId,atlas {atlasTexId = texId})
  
  GL.textureBinding GL.Texture2D $= Just texId

  GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.Clamp)
  GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.Clamp)
  GL.textureFilter   GL.Texture2D      $= ((GL.Linear',Nothing),GL.Linear')

  imgData <- newArray $ B.unpack (atlasImgData atlas')

  case (atlasDepth atlas') of
    3 -> GL.texImage2D
          Nothing
          GL.NoProxy
          0
          GL.RGB'
          (GL.TextureSize2D
            (gsizei $ atlasWidth atlas')
            (gsizei $ atlasHeight atlas')
            )
          0
          (GL.PixelData GL.RGB GL.UnsignedByte imgData)
    _ -> GL.texImage2D
          Nothing
          GL.NoProxy
          0
          GL.Alpha'
          (GL.TextureSize2D
            (gsizei $ atlasWidth atlas')
            (gsizei $ atlasHeight atlas')
            )
          0
          (GL.PixelData GL.Alpha GL.UnsignedByte imgData)


       
  return atlas'  

gsizei :: Int -> GL.GLsizei
{-# INLINE gsizei #-}
gsizei x = unsafeCoerce x

atlasSetRegion ::
  TextureAtlas
  -> Int -> Int
  -> Int -> Int
  -> ByteString -> Int
  -> TextureAtlas
atlasSetRegion atlas x y width height buf stride
  | x < atlasWidth atlas
  , (x + width) < atlasWidth atlas
  , y < atlasHeight atlas
  , (y + height) < atlasHeight atlas
  = atlas {atlasImgData = B.concat rep''}
  where
    depth       = atlasDepth atlas
    widthA      = atlasWidth atlas
    imgData     = atlasImgData atlas
    imgData'    = splitRows imgData (widthA * depth)
    buf'        = splitRows buf     (width  * depth)
    (pre,rep)   = splitAt y imgData'
    (rep',post) = splitAt height rep
    rep''       = zipWith (doRep x width) rep' buf'

splitRows ::
  ByteString
  -> Int
  -> [ByteString]
splitRows b w | not (B.null b) = 
  let (pre,post) = B.splitAt w b
  in  pre:(splitRows post w)
              | otherwise    = []

doRep ::
  Int -> Int
  -> ByteString
  -> ByteString
  -> ByteString
doRep x w b1 b2 = B.concat [pre,b2,post]
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
  -> Int -> Int -> Int
  -> Maybe (Int,Int,Int)
atlasFit atlas index width height = out
  where
    -- Get node from atlas
    node         = (atlasNodes atlas)!!index
    -- Get x and y location of the index node
    (Node x y w) = node
    out = if (x + width) > (atlasWidth atlas) 
      then
        Nothing
      else
        if yMax + height > (atlasHeight atlas)
          then
            Nothing
          else
            Just nodeMax
    
    -- Get node with highest y, in x range [nodeX .. nodeX+width]
    (_,nodeMax@(_,yMax,_)) = foldl reduceWmaxY (width,(x,y,w)) 
      (drop index (atlasNodes atlas))
    reduceWmaxY (widthLeft,n@(x, y, w)) (Node x' y' w') = 
      if widthLeft > 0 
        then (widthLeft - w', (x, max y y', w)) 
        else (widthLeft,n)

atlasMerge ::
  TextureAtlas
  -> TextureAtlas
atlasMerge atlas = atlas {atlasNodes = nodes'}
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

atlasGetRegion ::
  TextureAtlas
  -> Int -> Int
  -> (TextureAtlas,Region)
atlasGetRegion atlas width height = (atlas',region)
  where
    potentialFits = map (\i -> (i,atlasFit atlas i width height)) [0..length (atlasNodes atlas) - 1]
    (bestIndex,bestFit) = head $ List.sortBy betterFit potentialFits

    region = case bestFit of
      Nothing -> Region (-1) (-1) 0 0
      Just (x, y, w) -> Region x y width height
    
    atlas' = case bestFit of
      Nothing -> atlas
      Just (x, y, w) -> let
          newNode    = Node x (y+height) width
          (pre,post) = List.splitAt bestIndex (atlasNodes atlas)
          post'      = shrink newNode post
        in
          atlas {atlasNodes = pre ++ newNode:(post')}


betterFit :: 
  (Int,Maybe (Int,Int,Int))
  -> (Int,Maybe (Int,Int,Int))
  -> Ordering
betterFit (_,Nothing) (_,Nothing) = EQ
betterFit (_,Nothing) (_,Just _)  = GT
betterFit (_,Just _)  (_,Nothing) = LT
betterFit (_,Just (_,y1,w1)) (_,Just (_,y2,w2)) =
  if (y1 < y2) || (y1 == y2) && (w1 < w2) 
    then LT
    else GT

shrink :: 
  Node 
  -> [Node] 
  -> [Node]    
shrink _            []              = []
shrink np@(Node xP yP wP) ((n@(Node x y w)):ns) = 
  if x < xP + wP 
    then n:ns
    else let
        s  = xP + wP - x
        x' = x + s
        w' = w - s
    in if (w' <= 0)
      then shrink np ns
      else (Node x' y w'):ns

