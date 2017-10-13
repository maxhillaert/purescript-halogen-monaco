module Halogen.Monaco where

import Prelude

import CSS.Geometry (width, height)
import CSS.Size
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (EXCEPTION)
import DOM (DOM)
import Data.Int
import Data.Foldable (for_)
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Properties as HP
import Monaco.Editor as ME
import Monaco.Types (EditorConstructionOptions, MONACO)
import Monaco.Types as MT

type EditorState =
  { editor ∷ Maybe MT.Editor
   , width ∷ Maybe Int
   , height ∷ Maybe Int
  }

type MonacoEffects eff = (monaco :: MONACO, exception :: EXCEPTION, dom :: DOM | eff)

data EditorQuery a = 
    Init EditorConstructionOptions a

data EditorMessage = Initialized 

editor :: forall eff. H.Component HH.HTML EditorQuery Unit EditorMessage (Aff (MonacoEffects eff))
editor =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: EditorState
  initialState = {
      editor: Nothing,
      width: Nothing,
      height: Nothing
  }

  render :: EditorState -> H.ComponentHTML EditorQuery
  render state =
    HH.div
            [ HP.ref $ H.RefLabel "monacoEditor",
              HP.class_ $ H.ClassName "monaco-editor-container",
              style do 
                      for_ state.height (\x->height $ px $ toNumber x)
                      for_ state.width (\x->width $ px $ toNumber x)
            ]
            [  ]

  eval :: EditorQuery ~> H.ComponentDSL EditorState EditorQuery EditorMessage (Aff (MonacoEffects eff))
  eval (Init options next) = do 
    el' <- H.getHTMLElementRef (H.RefLabel "monacoEditor")
    H.modify (\s -> s{ editor = Nothing})
    for_ el' \el -> do
        e <- H.liftAff (ME.create options el)
        H.modify (\s -> s{editor=Just e})
    pure next


  