module PrettyPrint exposing
  ( Doc
  , empty, concat, nest, text, line
  , group
  , prettyHtml, prettyString
  )

{-| Based on [Wadler's paper][wadler]

Perhaps some of the primitives and combinators from
[Daan Leijen's Haskell library][leijen] which builds on it could be added.

[wadler]: http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf
[leijen]: https://hackage.haskell.org/package/wl-pprint-1.2/docs/Text-PrettyPrint-Leijen.html

# Constructors
@docs Doc, empty, concat, nest, text, line

# Combinators
@docs group

# Render
@docs prettyHtml, prettyString

-}

import String
import Html exposing (..)
import Html.Attributes exposing (style)

{-|-}
type Doc a
  = Empty
  | Concat (Doc a) (Doc a)
  | Nest Int (Doc a)
  | Text (List (Attribute a)) String
  | Line
  | Union (Doc a) (Doc a)


-- CONSTRUCTORS


{-|-}
empty : Doc a
empty =
  Empty


{-|-}
concat : Doc a -> Doc a -> Doc a
concat =
  Concat


{-| Indent the given doc by the given number of spaces. -}
nest : Int -> Doc a -> Doc a
nest =
  Nest


{-|-}
text : List (Attribute a) -> String -> Doc a
text =
  Text


{-| Line break -}
line : Doc a
line =
  Line


-- not exposed
union : Doc a -> Doc a -> Doc a
union =
  Union


-- COMBINATORS

{-| Specify that the Doc should be printed without newlines if it fits in the
available space, or without them if it doesn't -}
group : Doc a -> Doc a
group doc =
  union (flatten doc) doc


flatten : Doc a -> Doc a
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


type NormalForm a
  = SText (List (Attribute a)) String (NormalForm a)
  | SEmpty
  | SLine Int (NormalForm a)


{-| Pretty print the document to HTML which fits within the
max width (in characters; font size up to you) -}
prettyHtml : Int -> Doc a -> Html a
prettyHtml maxWidth doc =
  layoutHtml (best maxWidth doc)


{-| Pretty print the document to string which fits within the
max width (in characters) -}
prettyString : Int -> Doc a -> String
prettyString maxWidth doc =
  layoutString (best maxWidth doc)


best : Int -> Doc a -> NormalForm a
best maxWidth doc =
  let
    recurse : Int -> List (Int, Doc a) -> NormalForm a
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

    better : Int -> NormalForm a -> NormalForm a -> NormalForm a
    better already nfA nfB =
      if fits (maxWidth - already) nfA then nfA else nfB

    fits : Int -> NormalForm a -> Bool
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


layoutHtml : NormalForm a -> Html a
layoutHtml normalForm =
  let
    recurse : NormalForm a -> List (Html a)
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


layoutString : NormalForm a -> String
layoutString normalForm =
  let
    recurse : NormalForm a -> String
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
