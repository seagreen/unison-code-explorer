module UCE.StackGen where

import UCE.Prelude
import qualified UCE.Code
import qualified Unison.Util.Relation as Relation
import qualified Data.Map.Strict as Map

build dest codeinfo =
    let names = codeinfo & UCE.Code.codeDeclarationNames & Relation.domain & Map.toAscList
        refs = map fst names
    in
        10

    













