module Graphics.UI.Font.Markup
  ( newMarkup
  , Markup (..)
  )
where

import Graphics.UI.Font.Types

newMarkup :: Markup
newMarkup = markup
  where
    black = RGBA 0 0 0 1
    white = RGBA 1 1 1 0

    markup = Markup
      { muFamily  = "monotype"
      , muItalic  = False
      , muBold    = False
      , muSize    = 16
      , muRise    = 0
      , muSpacing = 0
      , muForegroundColor = black
      , muBackgroundColor = white
      , muUnderlineColor  = Just black
      , muOverlineColor   = Just black
      , muStrikethroughColor = Just black
      , muOutlineColor = error "Outline color not set"
      }
