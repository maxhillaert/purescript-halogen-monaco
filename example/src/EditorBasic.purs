module EditorBasic where

import Prelude

import Control.Monad.Eff (Eff)
import Halogen.Aff as HA
import Halogen.Monaco as B
import Halogen.VDom.Driver (runUI)
import Monaco.Types (MONACO, defaultConstuctorOptions)
import Data.Maybe

main :: Eff (HA.HalogenEffects (monaco :: MONACO)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  io <- runUI B.editor unit body
  let opts = defaultConstuctorOptions 
                { value = Just "function x() {\n\tconsole.log(\"Hello world!\");\n}" 
                    , language = Just "javascript"
                    , theme = Just "vs-dark"
                }
  r <- io.query (B.Init opts unit)
  pure r