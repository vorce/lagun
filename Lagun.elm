module Lagun exposing (..)

import Http
import Json.Decode as Json exposing ((:=), decodeString)
import Task
import Dict exposing (Dict)
import Set exposing (Set)


-- MODEL


type alias Model = -- TODO model spec as Result spec instead?
  { specUrl : String, spec : Maybe Spec, expanded : Set String, paramValues : ParameterValues }


init : String -> ( Model, Cmd Msg )
init url =
  ( Model url Maybe.Nothing Set.empty Dict.empty, getJsonSpec url )



-- UPDATE


type Msg
  = FetchSpec (Maybe String)
  | FetchSpecFail Http.Error
  | FetchSpecOk Spec
  | TryRequest Http.Request
  | ExpansionToggled (Set String)
  | RequestFail Http.RawError
  | RequestResult Http.Response
  | ParameterInput ParameterValues


update : Msg -> Model -> ( Model, Cmd Msg )
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

    FetchSpecOk spec ->
      ( Model model.specUrl (Maybe.Just spec) model.expanded model.paramValues
      , Cmd.none
      )

    FetchSpecFail errorMsg ->
      (Model model.specUrl Maybe.Nothing model.expanded model.paramValues
      , Cmd.none) -- TODO: Actually show the error message

    ExpansionToggled expanded ->
      ( Model model.specUrl model.spec expanded model.paramValues
      , Cmd.none
      )

    TryRequest request ->
      ( model, tryRequest request )

    RequestResult result ->
      ( model, Cmd.none )

    RequestFail errorMsg ->
      (model, Cmd.none) -- TODO Actually show the error message

    ParameterInput paramValues ->
      ( Model model.specUrl model.spec model.expanded paramValues
      , Cmd.none
      )



-- Cmd


tryRequest : Http.Request -> Cmd Msg
tryRequest req =
  let
    settings =
      Http.defaultSettings
  in
    -- Task.perform FetchFail FetchSuccess (Http.get decodeGifUrl url)
    Task.perform RequestFail RequestResult (Http.send settings req)

getJsonSpec : String -> Cmd Msg
getJsonSpec url =
  Task.perform FetchSpecFail FetchSpecOk (Http.get decodeSpec url)

  -- Http.get decodeSpec url
  --   |> Task.toMaybe
  --   |> Task.map RenderSpec
  --   |> Cmd.task


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
