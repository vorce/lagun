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


operationEntry : ( String, Operation ) -> Html
operationEntry ( opName, op ) =
  dt
    []
    [ div
        [ class "row" ]
        [ div
            [ class ("column column-10 method-" ++ opName) ]
            [ text opName ]
        , div
            [ class ("column column-90 method-summary-" ++ opName) ]
            [ text op.summary ]
        ]
    , div
        [ class "row" ]
        [ div
            [ class ("column column-100 params-and-request method-summary-" ++ opName) ]
            [ div
                []
                [ h6 [] [ text "Parameters" ]
                , parametersTable op.parameters
                , h6 [] [ text "Responses" ]
                , responsesTable op.responses
                , button [ class "button" ] [ text "Send request" ]
                ]
            ]
        ]
    ]


parametersTable : List Parameter -> Html
parametersTable ps =
  table
    []
    [ thead
        []
        [ tr
            []
            [ th
                []
                [ text "Parameter" ]
            , th
                []
                [ text "Value" ]
            , th
                []
                [ text "Description" ]
            , th
                []
                [ text "Parameter Type" ]
            , th
                []
                [ text "Data Type" ]
            ]
        ]
    , tbody
        []
        (List.map parameterEntry ps)
    ]


parameterEntry : Parameter -> Html
parameterEntry param =
  tr
    []
    [ td
        []
        [ text param.name ]
    , td
        []
        [ input [ type' "text" ] [] ]
    , td
        []
        [ text param.description ]
    , td
        []
        [ text param.in' ]
    , td
        []
        [ text "TODO: schema" ]
    ]


operationList : Operations -> Html
operationList ms =
  dl [] (List.map operationEntry (Dict.toList ms))


responseEntry : ( String, Response ) -> Html
responseEntry ( httpCode, r ) =
  tr
    []
    [ td
        []
        [ text httpCode ]
    , td
        []
        [ text r.description ]
      -- Markdown.toHtml
    , td
        []
        [ text "TODO: Response model" ]
    , td
        []
        [ text "TODO: Headers" ]
    ]


responsesTable : Dict String Response -> Html
responsesTable rs =
  table
    []
    [ thead
        []
        [ tr
            []
            [ th
                []
                [ text "HTTP Code" ]
            , th
                []
                [ text "Reason" ]
            , th
                []
                [ text "Response model" ]
            , th
                []
                [ text "Headers" ]
            ]
        ]
    , tbody
        []
        (List.map responseEntry (Dict.toList rs))
    ]


pathEntry : ( String, Operations ) -> Html
pathEntry ( p, ms ) =
  dt
    []
    [ div
        []
        [ h4 [] [ text p ]
        , operationList ms
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
  { info : Info, paths : Paths, swagger : String }


type alias Info =
  { title : String, description : String, version : String }


type alias Paths =
  Dict String Operations


type alias Parameter =
  { in' : String, name : String, description : String }


type alias Response =
  { description : String }


type alias Operations =
  Dict String Operation


type alias Operation =
  { summary : String
  , description : String
  , parameters : List Parameter
  , responses : Dict String Response
  }


decodeParameter : Json.Decoder Parameter
decodeParameter =
  Json.object3
    Parameter
    ("in" := Json.string)
    ("name" := Json.string)
    (optionalField "description")


decodeResponse : Json.Decoder Response
decodeResponse =
  Json.object1
    Response
    ("description" := Json.string)


decodeOperation : Json.Decoder Operation
decodeOperation =
  Json.object4
    Operation
    (optionalField "summary")
    (optionalField "description")
    (Json.at [ "parameters" ] <| Json.oneOf [ Json.list decodeParameter, Json.succeed [] ])
    (Json.at [ "responses" ] <| Json.dict decodeResponse)


decodeOperations : Json.Decoder Operations
decodeOperations =
  Json.dict decodeOperation


decodePaths : Json.Decoder Paths
decodePaths =
  Json.at
    [ "paths" ]
    <| Json.dict decodeOperations


decodeInfo : Json.Decoder Info
decodeInfo =
  Json.at
    [ "info" ]
    <| Json.object3
        Info
        ("title" := Json.string)
        (optionalField "description")
        ("version" := Json.string)


optionalField : String -> Json.Decoder String
optionalField field =
  Json.oneOf [ field := Json.string, Json.succeed "" ]


decodeSpec : Json.Decoder Spec
decodeSpec =
  Json.object3
    Spec
    decodeInfo
    decodePaths
    ("swagger" := Json.string)



--title description version
