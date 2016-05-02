module Lagun (..) where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Json exposing ((:=))
import Task
import Dict exposing (Dict)
import String


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
            , pathList spec.paths
            ]
        ]

    Maybe.Nothing ->
      div
        []
        [ p [] [ text "No API specification found. Try fetching one!" ]
        , input [ placeholder specUrl, value specUrl ] []
        , button [ onClick address (FetchSpec (Maybe.Just specUrl)) ] [ text "Fetch & Render spec" ]
        ]


methodEntry : ( String, Method ) -> Html
methodEntry ( name, m ) =
  li
    []
    [ text (name ++ ": " ++ m.summary) ]


methodList : Methods -> Html
methodList ms =
  ul [] (List.map methodEntry (Dict.toList ms))


pathEntry : ( String, Methods ) -> Html
pathEntry ( p, ms ) =
  li
    []
    [ div
        []
        [ h4 [] [ text p ]
        , methodList ms
        ]
    ]


pathList : Paths -> Html
pathList paths =
  div
    []
    [ h3 [] [ text "Paths" ]
    , ul [] (List.map pathEntry (Dict.toList paths))
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
  { info : Info, paths : Paths }


type alias Info =
  { title : String, description : String, version : String }


type alias Paths =
  -- ( String, List ( String, String ) )
  Dict String Methods


type alias Parameter =
  { paramIn : String, name : String, description : String }


type alias Response =
  { description : String }


type alias Methods =
  Dict String Method


type alias Method =
  { summary : String, description : String }



-- parameters : List Parameter, responses : List ( String, Response ) }


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
  Json.object2
    Method
    ("summary" := Json.string)
    ("description" := Json.string)



--("parameters" := Json.list decodeParameter)
-- decodeResponses


decodeMethods : Json.Decoder Methods
decodeMethods =
  Json.dict decodeMethod


decodePaths : Json.Decoder Paths
decodePaths =
  Json.at
    [ "paths" ]
    <| Json.dict decodeMethods


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
