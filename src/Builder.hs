{-# LANGUAGE TypeFamilies #-}
module Builder where

import qualified Graph as G
import qualified Relation as R
import Relation ( Relation ( .. ), Weight ( .. ) )
import Graph ( Graph ( .. ), Tree ( .. ) )

import Control.Monad.Trans.State ( StateT ( .. ) )

------------------------type declaration------------------
type Building generalTp monad = StateT ( Graph generalTp () ) monad ( Graph generalTp () )

-------------------------features--------------------------
--- - - - - - - - - - - inside Building - - - - - - - - ---

