module Lagun (..) where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Json exposing ((:=))
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Task


-- MODEL


type alias Model =
  { specUrl : String, spec : Maybe Spec }


init : String -> ( Model, Effects Action )
init url =
  ( Model url Maybe.Nothing, getJsonSpec url )



-- UPDATE


type Action
  = FetchSpec (Maybe String)
  | RenderSpec (Maybe Spec)
  | TryRequest


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    FetchSpec maybeUrl ->
      ( model
      , getJsonSpec (Maybe.withDefault model.specUrl maybeUrl)
      )

    RenderSpec maybeSpec ->
      ( Model model.specUrl maybeSpec
      , Effects.none
      )

    TryRequest ->
      ( model, Effects.none )



-- VIEW


view : Signal.Address Action -> Model -> Html
view address { specUrl, spec } =
  case spec of
    Maybe.Just spec ->
      div
        []
        [ input [ placeholder specUrl, value specUrl ] []
        , button [ onClick address (FetchSpec (Maybe.Just specUrl)) ] [ text "Fetch & Render spec" ]
        , div
            []
            [ h1 [] [ text spec.info.title ]
            , h2 [] [ text spec.info.description ]
            , p [] [ text spec.info.version ]
            , hr [] []
            , pathList address spec.paths
            ]
        ]

    Maybe.Nothing ->
      div
        []
        [ p [] [ text "No API specification found. Try fetching one!" ]
        , input [ placeholder specUrl, value specUrl ] []
        , button [ onClick address (FetchSpec (Maybe.Just specUrl)) ] [ text "Fetch & Render spec" ]
        ]


pathEntry : Signal.Address Action -> Path -> Html
pathEntry address p =
  li
    []
    [ text (fst p) ]


pathList : Signal.Address Action -> List Path -> Html
pathList address paths =
  div
    []
    [ h3 [] [ text "Paths" ]
    , ul [] (List.map (pathEntry address) paths)
    ]



-- Effects


debugOutput : a -> a
debugOutput foo =
  let
    log =
      Debug.log "Http error spec" foo
  in
    foo


getJsonSpec : String -> Effects Action
getJsonSpec url =
  Http.get decodeSpec url
    |> Task.toMaybe
    |> Task.map RenderSpec
    |> Effects.task



-- JSON decoders


type alias Spec =
  { info : Info, paths : List Path }


type alias Info =
  { title : String, description : String, version : String }


type alias Path =
  ( String, List ( String, String ) )


type alias Parameter =
  { paramIn : String, name : String, description : String }


type alias Response =
  { description : String }


type alias Method =
  { summary : String, description : String, consumes : List String, produces : List String, parameters : List Parameter, responses : List ( String, Response ) }


decodeParameter : Json.Decoder Parameter
decodeParameter =
  Json.object3
    Parameter
    ("in" := Json.string)
    ("name" := Json.string)
    ("description" := Json.string)


decodeResponse : Json.Decoder Response
decodeResponse =
  Json.object1
    Response
    ("description" := Json.string)


decodeResponses : Json.Decoder (List ( String, Response ))
decodeResponses =
  Json.keyValuePairs decodeResponse


decodeMethod : Json.Decoder Method
decodeMethod =
  Json.object6
    Method
    ("summary" := Json.string)
    ("description" := Json.string)
    ("consumes" := Json.list Json.string)
    ("produces" := Json.list Json.string)
    ("parameters" := Json.list decodeParameter)
    decodeResponses


decodePath : Json.Decoder (List ( String, String ))
decodePath =
  Json.keyValuePairs
    ("summary" := Json.string)


decodePaths : Json.Decoder (List ( String, List ( String, String ) ))
decodePaths =
  Json.at
    [ "paths" ]
    <| Json.keyValuePairs
        decodePath


decodeInfo : Json.Decoder Info
decodeInfo =
  Json.at
    [ "info" ]
    <| Json.object3
        Info
        ("title" := Json.string)
        ("description" := Json.string)
        ("version" := Json.string)


decodeSpec : Json.Decoder Spec
decodeSpec =
  Json.object2
    Spec
    decodeInfo
    decodePaths



--title description version
