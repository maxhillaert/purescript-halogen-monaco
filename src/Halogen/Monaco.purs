module Halogen.Monaco
  ( editor
  , EditorQuery(..)
  , EditorMessage(..)
  ) where

import Prelude
import Data.Int (toNumber)
import Data.Foldable (for_)
import Data.Maybe (Maybe(Nothing, Just))

import Effect.Aff.Class (class MonadAff)

import CSS.Geometry (width, height)
import CSS.Size (px)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Properties as HP

import Monaco.Editor as ME
import Monaco.Types (EditorConstructionOptions)
import Monaco.Types as MT

type EditorState =
  { editor ∷ Maybe MT.Editor
  , width ∷ Maybe Int
  , height ∷ Maybe Int
  }

data EditorAction

data EditorQuery a
  = Init EditorConstructionOptions a

data EditorMessage
  = Initialized 

editor :: forall i m. MonadAff m => H.Component HH.HTML EditorQuery i EditorMessage m
editor =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleQuery = handleQuery
      }
    }

initialState :: forall i. i -> EditorState
initialState _ =
  { editor: Nothing
  , width: Nothing
  , height: Just 1000
  }

render :: forall m. EditorState -> H.ComponentHTML EditorAction () m
render state =
  HH.div
    [ HP.ref $ H.RefLabel "monacoEditor"
    , HP.class_ $ H.ClassName "monaco-editor-container"
    , style do 
        for_ state.height (\x -> height $ px $ toNumber x)
        for_ state.width (\x -> width $ px $ toNumber x)
    ]
    [  ]

handleQuery ∷ forall m a. MonadAff m => EditorQuery a → H.HalogenM EditorState EditorAction () EditorMessage m (Maybe a)
handleQuery (Init options next) = do
  el' <- H.getHTMLElementRef (H.RefLabel "monacoEditor")
  H.modify_ (_ { editor = Nothing })
  for_ el' \el -> do
    e <- H.liftAff (ME.create options el)
    H.modify_ (_ { editor = Just e })
  pure (Just next)
