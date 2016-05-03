module Lagun (..) where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (placeholder, value, class, type')
import Html.Events exposing (onClick, on, targetValue)
import Http
import Json.Decode as Json exposing ((:=), decodeString)
import Task
import Dict exposing (Dict)
import String
import Markdown


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
      let
        url =
          (Maybe.withDefault model.specUrl maybeUrl)
      in
        ( Model url model.spec
        , getJsonSpec url
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
        [ class "container" ]
        [ div
            [ class "row" ]
            (headerHtml address specUrl)
        , div
            []
            [ h1 [] [ text spec.info.title ]
            , Markdown.toHtml spec.info.description
            , p [] [ text ("Version: " ++ spec.info.version) ]
            , hr [] []
            , pathList spec.paths
            ]
        ]

    Maybe.Nothing ->
      div
        [ class "container" ]
        [ p [] [ text "No API specification found, or something went wrong while parsing it. U_U" ]
        , div
            [ class "row" ]
            (headerHtml address specUrl)
        ]


headerHtml : Signal.Address Action -> String -> List Html
headerHtml address specUrl =
  [ div
      [ class "column column-80" ]
      [ specUrlInput address specUrl ]
  , div
      [ class "column column-20" ]
      [ button [ onClick address (FetchSpec (Maybe.Just specUrl)) ] [ text "Lagun it!" ]
      ]
  ]


specUrlInput : Signal.Address Action -> String -> Html
specUrlInput address specUrl =
  input
    [ type' "text"
    , placeholder specUrl
    , value specUrl
    , on "input" targetValue (\newurl -> Signal.message address (FetchSpec (Just newurl)))
    ]
    []


methodEntry : ( String, Method ) -> Html
methodEntry ( methodName, m ) =
  dt
    []
    [ div
        [ class "row" ]
        [ div
            [ class ("column column-10 method-" ++ methodName) ]
            [ text methodName ]
        , div
            [ class ("column column-90 method-summary-" ++ methodName) ]
            [ text m.summary ]
        ]
    , div
        [ class "row" ]
        [ div
            [ class ("column column-100 params-and-request method-summary-" ++ methodName) ]
            [ div
                []
                [ h6 [] [ text "Parameters" ]
                , p [] [ text "TODO" ]
                ]
            ]
        ]
    ]



-- text (name ++ ": " ++ m.summary)


methodList : Methods -> Html
methodList ms =
  dl [] (List.map methodEntry (Dict.toList ms))


pathEntry : ( String, Methods ) -> Html
pathEntry ( p, ms ) =
  dt
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
    [ dl [] (List.map pathEntry (Dict.toList paths)) ]



-- Effects


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
  Dict String Methods


type alias Parameter =
  { paramIn : String, name : String, description : String }


type alias Response =
  { description : String }


type alias Methods =
  Dict String Method


type alias Method =
  { summary : String, description : String }


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
