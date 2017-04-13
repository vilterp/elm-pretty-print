module Example exposing (..)

import Window
import String

import Html exposing (Html, div)
import Html.Attributes exposing (style)

import PrettyPrint as PP exposing (..)
import PrettyPrint.Util exposing (..)

type Tree =
  Node String (List Tree)

type Msg = Resize Window.Size

type alias Model = { size : Window.Size }

initialModel : Model
initialModel = { size = { width = 1000, height = 1000 } }

showTree tree =
  case tree of
    Node s ts ->
      group (concat (text [] s) (nest (String.length s) (showBracket ts)))

showBracket trees =
  case trees of
    [] ->
      empty

    ts -> hcat
      [ text [] "["
      , nest 1 (showTrees ts)
      , text [] "]"
      ]


showTrees trees =
  case trees of
    [] ->
      empty

    [t] ->
      showTree t

    t :: ts -> hcat
      [ showTree t
      , text [] ","
      , line
      , showTrees ts
      ]

showTree_ node =
  case node of
    Node s ts ->
      concat (text [] s) (showBracket_ ts)

showBracket_ ts =
  case ts of
    [] ->
      empty

    ts ->
      bracket 2 ([], "[") (showTrees_ ts) ([], "]")


showTrees_ trees =
  case trees of
    [] ->
      empty

    [t] ->
      showTree t

    t :: ts -> hcat
      [ showTree t
      , text [] ","
      , line
      , showTrees ts
      ]


-- render

exampleTree =
  Node "aaaaaaaaaaaaaaaaaa" [
    Node "bbbbbbbbbbbbbbbbbbbbbbbb" [
      Node "ccccccccccccccccccccccccccccccccccccccccccccc" [],
      Node "dddddddddddddddddddddddddddddddddddddddddddddddddddddddd" []
    ],
    Node "eeeee" []
  ]


render : Doc msg -> Window.Size -> Html msg
render doc size =
  div
    [ style [("font-size", "12px")] ]
    [ prettyHtml (round (toFloat size.width / 7.3)) doc ] -- experimentally determined width of char...


exampleDoc =
  bracket 2 ([], "[") (text [] "foo") ([], "]")

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Resize size ->
      ( { model | size = size }, Cmd.none )

view : Doc Msg -> Model -> Html Msg
view doc model = Html.div []
    [ render doc model.size
    ]

main : Program Never Model Msg
main = Html.program
  { init = (initialModel, Cmd.none)
  , subscriptions = always (Window.resizes Resize)
  , update = update
  , view = view (showTree exampleTree)
           -- view exampleDoc
  }
