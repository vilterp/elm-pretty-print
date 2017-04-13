module PrettyPrint.Util exposing (..)

{-|
@docs bracket
-}

import Html exposing (..)

import PrettyPrint exposing (..)

{-| For when you want an open marker, an indented portion (by the given indent),
and a close marker -}
bracket : Int -> (List (Attribute msg), String) -> Doc msg -> (List (Attribute msg), String) -> Doc msg
bracket indent (leftAttrs, left) doc (rightAttrs, right) = group <| hcat
    [ PrettyPrint.text leftAttrs left
    , nest indent (concat line doc)
    , line
    , PrettyPrint.text rightAttrs right
    ]
