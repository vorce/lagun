module Lagun (..) where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (placeholder, value, class, type', src, alt, href, name)
import Html.Events exposing (onClick, on, targetValue)
import Http
import Json.Decode as Json exposing ((:=), decodeString)
import Task
import Dict exposing (Dict)
import Markdown
import Set exposing (Set)
import Regex


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



-- VIEW


view : Signal.Address Action -> Model -> Html
view address { specUrl, spec, expanded, paramValues } =
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
            , p [] [ text ("API Version: " ++ spec.info.version) ]
            , hr [] []
            , pathList address paramValues spec.paths expanded
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


operationEntry : Signal.Address Action -> ParameterValues -> String -> ( String, Operation ) -> Html
operationEntry address paramValues path' ( opName, op ) =
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
                , parametersTable (parametersTableBody address paramValues path' opName op.parameters)
                , h6 [] [ text "Responses" ]
                , responsesTable op.responses
                , requestButton address (requestBuilder opName path' paramValues)
                ]
            ]
        ]
    ]


pathWithVariables : String -> Dict String String -> String
pathWithVariables path' variables =
  let
    re =
      Regex.regex "\\{(.*?)\\}"
  in
    Regex.replace Regex.All re (\{ match } -> (Maybe.withDefault "" (Dict.get match variables))) path'


requestBuilder : String -> String -> ParameterValues -> Http.Request
requestBuilder verb path' paramValues =
  let
    relevantParamValues =
      Dict.filter (\( p, v, in', n ) val -> p == path' && v == verb && in' == "path") paramValues

    relevantPathParams =
      -- yay :(
      Dict.filter (\( p, v, in', n ) val -> in' == "path") relevantParamValues
        |> Dict.toList
        |> List.map (\( ( p, v, in', name ), val ) -> ( "{" ++ name ++ "}", val ))
        |> Dict.fromList
  in
    { verb = verb
    , headers = []
    , url = ("http://petstore.swagger.io" ++ (pathWithVariables path' relevantPathParams))
    , body = Http.empty
    }


requestButton : Signal.Address Action -> Http.Request -> Html
requestButton address req =
  button
    [ class "button", onClick address (TryRequest req) ]
    [ text "Send request" ]


parametersTableBody : Signal.Address Action -> ParameterValues -> String -> String -> List Parameter -> Html
parametersTableBody address paramValues path' opName ps =
  tbody
    []
    (List.map (parameterEntry address paramValues path' opName) ps)


parametersTable : Html -> Html
parametersTable tableBody =
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
    , tableBody
    ]


parameterEntryInput : Signal.Address Action -> ParameterValues -> ParameterKey -> Html
parameterEntryInput address currentValues paramKey =
  input
    [ type' "text"
    , value (Maybe.withDefault "" (Dict.get paramKey currentValues))
    , on
        "input"
        targetValue
        (\val -> Signal.message address (ParameterInput (Dict.insert paramKey val currentValues)))
    ]
    []


parameterEntry : Signal.Address Action -> ParameterValues -> String -> String -> Parameter -> Html
parameterEntry address currentValues path' opName param =
  tr
    []
    [ td
        []
        [ text param.name ]
    , td
        []
        [ parameterEntryInput address currentValues (parameterKey path' opName param)
        ]
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


parameterKey : String -> String -> Parameter -> ParameterKey
parameterKey path' opName param =
  ( path', opName, param.in', param.name )


operationList : Signal.Address Action -> ParameterValues -> String -> Operations -> Html
operationList address paramValues path' ops =
  dl [] (List.map (operationEntry address paramValues path') (Dict.toList ops))


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


renderPath : Signal.Address Action -> ParameterValues -> Set String -> ( String, Operations ) -> Html
renderPath address paramValues expanded ( pathName, ops ) =
  case (Set.member pathName expanded) of
    False ->
      div
        []
        [ h5
            []
            [ a
                [ onClick address (ExpansionToggled (Set.insert pathName expanded))
                , href ("#" ++ pathName)
                , name pathName
                ]
                [ fontAwesome "plus-square-o" ]
            , text (" " ++ pathName)
            ]
        ]

    True ->
      div
        []
        [ h5
            []
            [ a
                [ onClick address (ExpansionToggled (Set.remove pathName expanded))
                , href ("#" ++ pathName)
                , name pathName
                ]
                [ fontAwesome "minus-square-o" ]
            , text (" " ++ pathName)
            ]
        , operationList address paramValues pathName ops
        ]


pathEntry : Signal.Address Action -> ParameterValues -> Set String -> ( String, Operations ) -> Html
pathEntry address paramValues expanded ( p, ops ) =
  dt
    []
    [ (renderPath address paramValues expanded ( p, ops ))
    ]


pathList : Signal.Address Action -> ParameterValues -> Paths -> Set String -> Html
pathList address paramValues paths expanded =
  div
    []
    [ dl [] (List.map (pathEntry address paramValues expanded) (Dict.toList paths)) ]


fontAwesome : String -> Html
fontAwesome name =
  span [ class ("fa fa-" ++ name) ] []



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



--title description version
