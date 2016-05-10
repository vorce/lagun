module Lagun (..) where

import Effects exposing (Effects, Never)
import Http
import Json.Decode as Json exposing ((:=), decodeString)
import Task
import Dict exposing (Dict)
import Set exposing (Set)


-- MODEL


type alias Model =
  { specUrl : String, spec : Maybe Spec, expanded : Set String, paramValues : ParameterValues }


init : String -> ( Model, Effects Action )
init url =
  ( Model url Maybe.Nothing Set.empty Dict.empty, getJsonSpec url )



-- UPDATE


type Action
  = FetchSpec (Maybe String)
  | RenderSpec (Maybe Spec)
  | TryRequest Http.Request
  | ExpansionToggled (Set String)
  | RequestResult (Result Http.RawError Http.Response)
  | ParameterInput ParameterValues


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    FetchSpec maybeUrl ->
      let
        url =
          (Maybe.withDefault model.specUrl maybeUrl)
      in
        ( Model url model.spec model.expanded model.paramValues
        , getJsonSpec url
        )

    RenderSpec maybeSpec ->
      ( Model model.specUrl maybeSpec model.expanded model.paramValues
      , Effects.none
      )

    ExpansionToggled expanded ->
      ( Model model.specUrl model.spec expanded model.paramValues
      , Effects.none
      )

    TryRequest request ->
      ( model, tryRequest request )

    RequestResult result ->
      ( model, Effects.none )

    ParameterInput paramValues ->
      ( Model model.specUrl model.spec model.expanded paramValues
      , Effects.none
      )



-- Effects


tryRequest : Http.Request -> Effects Action
tryRequest req =
  let
    settings =
      Http.defaultSettings
  in
    Http.send settings req
      |> Task.toResult
      |> Task.map RequestResult
      |> Effects.task


getJsonSpec : String -> Effects Action
getJsonSpec url =
  Http.get decodeSpec url
    |> Task.toMaybe
    |> Task.map RenderSpec
    |> Effects.task



-- JSON decoders


type alias ParameterKey =
  ( String, String, String, String )


type alias ParameterValues =
  Dict ParameterKey String


type alias Spec =
  { info : Info, paths : Paths, swagger : String, host : String }


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
  Json.object4
    Spec
    decodeInfo
    decodePaths
    ("swagger" := Json.string)
    (optionalField "host")
