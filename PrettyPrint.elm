module PrettyPrint
  ( Doc
  , empty, concat, nest, text, line
  , group
  , prettyHtml, prettyString
  ) where

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
type Doc
  = Empty
  | Concat Doc Doc
  | Nest Int Doc
  | Text (List Attribute) String
  | Line
  | Union Doc Doc


-- CONSTRUCTORS


{-|-}
empty : Doc
empty =
  Empty


{-|-}
concat : Doc -> Doc -> Doc
concat =
  Concat


{-| Indent the given doc by the given number of spaces. -}
nest : Int -> Doc -> Doc
nest =
  Nest


{-|-}
text : List Attribute -> String -> Doc
text =
  Text


{-| Line break -}
line : Doc
line =
  Line


-- not exposed
union : Doc -> Doc -> Doc
union =
  Union


-- COMBINATORS

{-| Specify that the Doc should be printed without newlines if it fits in the
available space, or without them if it doesn't -}
group : Doc -> Doc
group doc =
  union (flatten doc) doc


flatten : Doc -> Doc
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


type NormalForm
  = SText (List Attribute) String NormalForm
  | SEmpty
  | SLine Int NormalForm


{-| Pretty print the document to HTML which fits within the
max width (in characters; font size up to you) -}
prettyHtml : Int -> Doc -> Html
prettyHtml maxWidth doc =
  layoutHtml (best maxWidth doc)


{-| Pretty print the document to string which fits within the
max width (in characters) -}
prettyString : Int -> Doc -> String
prettyString maxWidth doc =
  layoutString (best maxWidth doc)


best : Int -> Doc -> NormalForm
best maxWidth doc =
  let
    recurse : Int -> List (Int, Doc) -> NormalForm
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

    better : Int -> NormalForm -> NormalForm -> NormalForm
    better already nfA nfB =
      if fits (maxWidth - already) nfA then nfA else nfB

    fits : Int -> NormalForm -> Bool
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


layoutHtml : NormalForm -> Html
layoutHtml normalForm =
  let
    recurse : NormalForm -> List Html
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


layoutString : NormalForm -> String
layoutString normalForm =
  let
    recurse : NormalForm -> String
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
