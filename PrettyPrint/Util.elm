module PrettyPrint.Util where

{-|
@docs bracket
-}

import Html exposing (..)

import PrettyPrint exposing (..)

{-| For when you want an open marker, an indented portion (by the given indent),
and a close marker -}
bracket : Int -> (List Attribute, String) -> Doc -> (List Attribute, String) -> Doc
bracket indent (leftAttrs, left) doc (rightAttrs, right) =
  flattenable
    (PrettyPrint.text leftAttrs left
      `concat` nest indent (line `concat` doc) `concat` line
      `concat` PrettyPrint.text rightAttrs right)
