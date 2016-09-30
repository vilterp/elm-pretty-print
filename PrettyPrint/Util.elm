module PrettyPrint.Util exposing (..)

{-|
@docs bracket
-}

import Html exposing (..)

import PrettyPrint exposing (..)

{-| For when you want an open marker, an indented portion (by the given indent),
and a close marker -}
bracket : Int -> (List (Attribute a), String) -> Doc a -> (List (Attribute a), String) -> Doc a
bracket indent (leftAttrs, left) doc (rightAttrs, right) =
  group
    (PrettyPrint.text leftAttrs left
      `concat` nest indent (line `concat` doc) `concat` line
      `concat` PrettyPrint.text rightAttrs right)
