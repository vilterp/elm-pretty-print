module PrettyPrint exposing
  ( Doc
  , empty, concat, nest, text, line, hcat
  , group
  , prettyHtml, prettyString
  )

{-| Based on [Wadler's paper][wadler]

Perhaps some of the primitives and combinators from
[Daan Leijen's Haskell library][leijen] which builds on it could be added.

[wadler]: http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf
[leijen]: https://hackage.haskell.org/package/wl-pprint-1.2/docs/Text-PrettyPrint-Leijen.html

# Constructors
@docs Doc, empty, concat, hcat, nest, text, line

# Combinators
@docs group

# Render
@docs prettyHtml, prettyString

-}

import String
import Html exposing (..)
import Html.Attributes exposing (style)

{-|-}
type Doc msg
  = Empty
  | Concat (Doc msg) (Doc msg)
  | Nest Int (Doc msg)
  | Text (List (Attribute msg)) String
  | Line
  | Union (Doc msg) (Doc msg)


-- CONSTRUCTORS


{-|-}
empty : Doc msg
empty =
  Empty


{-|-}
concat : Doc msg -> Doc msg -> Doc msg
concat =
  Concat

{-|-}
hcat : List (Doc msg) -> Doc msg
hcat = List.foldr concat empty


{-| Indent the given doc by the given number of spaces. -}
nest : Int -> Doc msg -> Doc msg
nest =
  Nest


{-|-}
text : List (Attribute msg) -> String -> Doc msg
text =
  Text


{-| Line break -}
line : Doc msg
line =
  Line


-- not exposed
union : Doc msg -> Doc msg -> Doc msg
union =
  Union


-- COMBINATORS

{-| Specify that the Doc should be printed without newlines if it fits in the
available space, or without them if it doesn't -}
group : Doc msg -> Doc msg
group doc =
  union (flatten doc) doc


flatten : Doc msg -> Doc msg
flatten doc =
  case doc of
    Empty ->
      Empty

    Concat docA docB ->
      Concat (flatten docA) (flatten docB)

    Nest _ theDoc ->
      flatten theDoc

    Text _ _ ->
      doc

    Line ->
      text [] " "

    Union docA docB ->
      Union (flatten docA) (flatten docB)


-- RENDER


type NormalForm msg
  = SText (List (Attribute msg)) String (NormalForm msg)
  | SEmpty
  | SLine Int (NormalForm msg)


{-| Pretty print the document to HTML which fits within the
max width (in characters; font size up to you) -}
prettyHtml : Int -> Doc msg -> Html msg
prettyHtml maxWidth doc =
  layoutHtml (best maxWidth doc)


{-| Pretty print the document to string which fits within the
max width (in characters) -}
prettyString : Int -> Doc msg -> String
prettyString maxWidth doc =
  layoutString (best maxWidth doc)


best : Int -> Doc msg -> NormalForm msg
best maxWidth doc =
  let
    recurse : Int -> List (Int, Doc msg) -> NormalForm msg
    recurse already pairs =
      --let d = Debug.log "mw,a,p" (maxWidth, already, pairs) in
      case pairs of
        [] ->
          SEmpty

        (indent, Empty) :: rest ->
          recurse already rest

        (indent, Concat docA docB) :: rest ->
          recurse already ((indent, docA) :: (indent, docB) :: rest)

        (indent, Nest nestIndent doc) :: rest ->
          recurse already ((indent + nestIndent, doc) :: rest)

        (indent, Text attrs string) :: rest ->
          SText attrs string (recurse (already + String.length string) rest)

        (indent, Line) :: rest ->
          SLine indent (recurse indent rest)

        (indent, Union docA docB) :: rest ->
          better
            already
            (recurse already ((indent, docA)::rest))
            (recurse already ((indent, docB)::rest))

    better : Int -> NormalForm msg -> NormalForm msg -> NormalForm msg
    better already nfA nfB =
      if fits (maxWidth - already) nfA then nfA else nfB

    fits : Int -> NormalForm msg -> Bool
    fits width nf =
      if width < 0 then
        False
      else
        case nf of
          SEmpty ->
            True

          SText _ string next ->
            fits (width - String.length string) next

          SLine _ _ ->
            True
  in
    recurse 0 [(0, doc)]


layoutHtml : NormalForm msg -> Html msg
layoutHtml normalForm =
  let
    recurse : NormalForm msg -> List (Html msg)
    recurse nf =
      case nf of
        SEmpty ->
          []

        SText attrs textContent doc ->
          (span attrs [ Html.text textContent ]) :: (recurse doc)

        SLine indent doc ->
          br [] [] :: (span [] [Html.text (String.repeat indent "\x00A0")]) :: (recurse doc) -- nbsp
  in
    div
      [style [("font-family", "monospace")]]
      (recurse normalForm)


layoutString : NormalForm msg -> String
layoutString normalForm =
  let
    recurse : NormalForm msg -> String
    recurse nf =
      case nf of
        SEmpty ->
          ""

        SText attrs textContent doc ->
          textContent ++ recurse doc

        SLine indent doc ->
          "\n" ++ String.repeat indent " " ++ recurse doc
  in
    recurse normalForm
