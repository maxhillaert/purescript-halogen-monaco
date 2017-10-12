module Halogen.Monaco where

import Prelude

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Aff as A
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Class as AC
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Maybe.Trans (MaybeT)
import DOM (DOM)
import Data.Foldable (elem, for_)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(Nothing, Just))
import Data.Maybe (Maybe)
import Debug.Trace (traceAnyM)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Monaco.Editor as ME
import Monaco.Types (EditorConstructionOptions, MONACO)
import Monaco.Types as MT

type EditorState =
  { editor ∷ Maybe MT.Editor
--   , width ∷ Int
--   , height ∷ Int
  }

-- | Effects embedding the Ace editor requires.
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
      editor: Nothing
  }

  render :: EditorState -> H.ComponentHTML EditorQuery
  render state =
    HH.div
            [ HP.ref $ H.RefLabel "monacoEditor"
            ]
            []

  eval :: EditorQuery ~> H.ComponentDSL EditorState EditorQuery EditorMessage (Aff (MonacoEffects eff))
  eval (Init options next) = do 
    el' <- H.getHTMLElementRef (H.RefLabel "monacoEditor")
    H.modify (\s -> s{ editor = Nothing})
    for_ el' \el -> do
        e <- H.liftAff (ME.create options el)
        H.modify (\s -> s{editor=Just e})
    pure next


  