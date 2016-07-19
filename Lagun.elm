module Lagun exposing (..)

import Http
import Json.Decode as Json exposing ((:=), decodeString)
import Task
import Dict exposing (Dict)
import Set exposing (Set)
import String


-- MODEL


type alias Model =
  { specUrl : String,
  spec : Maybe Spec,
  expanded : Set String,
  paramValues : ParameterValues,
  requestResults : RequestResults }


init : { specUrl : String } -> ( Model, Cmd Msg )
init flags =
  ( Model flags.specUrl Maybe.Nothing Set.empty Dict.empty Dict.empty, getJsonSpec flags.specUrl)



-- UPDATE


type Msg
  = FetchSpec (Maybe String)
  | FetchSpecFail Http.Error
  | FetchSpecOk Spec
  | TryRequest (String, String) Http.Request
  | ExpansionToggled (Set String)
  | RequestFail Http.RawError
  | RequestResult (String, String) Http.Response
  | ParameterInput ParameterValues


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
  case action of
    FetchSpec maybeUrl ->
      let
        url =
          (Maybe.withDefault model.specUrl maybeUrl)
      in
        ( Model url model.spec model.expanded model.paramValues model.requestResults
        , getJsonSpec url
        )

    FetchSpecOk spec ->
      ( Model model.specUrl (Maybe.Just spec) model.expanded model.paramValues model.requestResults
      , Cmd.none
      )

    FetchSpecFail (Http.UnexpectedPayload error) ->
      (Model model.specUrl Maybe.Nothing model.expanded model.paramValues model.requestResults
      , debugCmd (debugOutput "Spec parse failure" error)) -- TODO: Actually show the error message to the user

    FetchSpecFail Http.Timeout ->
      (Model model.specUrl Maybe.Nothing model.expanded model.paramValues model.requestResults
      , debugCmd (debugOutput "Spec fetch timed out" ""))

    FetchSpecFail Http.NetworkError ->
      (Model model.specUrl Maybe.Nothing model.expanded model.paramValues model.requestResults
      , debugCmd (debugOutput "Spec fetch failed due to a network error" ""))

    FetchSpecFail (Http.BadResponse code msg) ->
      (Model model.specUrl Maybe.Nothing model.expanded model.paramValues model.requestResults
      , debugCmd (debugOutput "Spec fetch failed due to a http error" msg))

    ExpansionToggled expanded ->
      ( Model model.specUrl model.spec expanded model.paramValues model.requestResults
      , Cmd.none
      )

    TryRequest (path', verb) request ->
      ( model, tryRequest path' verb request )

    RequestResult key result ->
      ( Model model.specUrl model.spec model.expanded model.paramValues (Dict.insert key result model.requestResults),
      Cmd.none )

    RequestFail errorMsg ->
      (model, Cmd.none) -- TODO Actually show the error message

    ParameterInput paramValues ->
      ( Model model.specUrl model.spec model.expanded paramValues model.requestResults
      , Cmd.none
      )


debugCmd : String -> Cmd Msg
debugCmd error =
  Cmd.none

debugOutput : String -> String -> String
debugOutput location msg =
  Debug.log location msg

tryRequest : String -> String -> Http.Request -> Cmd Msg
tryRequest path' verb req =
  let
    settings =
      Http.defaultSettings
  in
    Task.perform RequestFail (\r -> RequestResult (path', verb) r) (Http.send settings req)


getJsonSpec : String -> Cmd Msg
getJsonSpec url =
  Task.perform FetchSpecFail FetchSpecOk (Http.get (decodeSpec (extractHost url)) url)


type alias RequestResults =
  Dict (String, String) Http.Response


type alias ParameterKey =
  ( String, String, String, String )


type alias ParameterValues =
  Dict ParameterKey String

-- Used for JSON decoding

type alias Spec =
  { info : Info, paths : Paths, swagger : String, host : String, basePath: String }


type alias Info =
  { title : String, description : String, version : String }


type alias Paths =
  Dict String Operations


type alias Parameter =
  { in' : String, name : String, description : String, type': String }


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

typeInfoVal : String -> Json.Decoder Parameter
typeInfoVal in' =
  Json.object4
    Parameter
    (Json.succeed in')
    ("name" := Json.string)
    (optionalField "description")
    (Json.oneOf ["type" := Json.string, Json.succeed "schema"]) -- TODO check, must be one of string, number, integer, boolean, array, file

typeInfo : String -> Json.Decoder Parameter
typeInfo in' =
  case in' of
    "body" ->
      typeInfoVal in'
    "query" ->
      typeInfoVal in'
    "header" ->
      typeInfoVal in'
    "path" ->
      typeInfoVal in'
    "formData" ->
      typeInfoVal in'
    _ ->
      Json.fail (in' ++ " is not a recognized parameter location")


decodeParameter : Json.Decoder Parameter
decodeParameter =
  ("in" := Json.string) `Json.andThen` typeInfo


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
    (Json.oneOf [ Json.at [ "parameters" ] <| Json.list decodeParameter, Json.succeed [] ])
    (Json.oneOf [ Json.at [ "responses" ] <| Json.dict decodeResponse, Json.succeed Dict.empty])


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
  optionalFieldWithDefault field ""

optionalFieldWithDefault : String -> String -> Json.Decoder String
optionalFieldWithDefault field default =
  Json.oneOf [ field := Json.string, Json.succeed default ]

extractHost : String -> String
extractHost url =
  let
    parts = String.split "/" url
  in
    Maybe.withDefault "localhost" (List.drop 2 parts |> List.head)

decodeSpec : String -> Json.Decoder Spec
decodeSpec defaultHost =
  Json.object5
    Spec
    decodeInfo
    decodePaths
    ("swagger" := Json.string)
    (optionalFieldWithDefault "host" defaultHost)
    (optionalField "basePath")
