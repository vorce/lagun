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


-- MODEL


type alias Model =
  { specUrl : String, spec : Maybe Spec, expanded : Set String }


init : String -> ( Model, Effects Action )
init url =
  ( Model url Maybe.Nothing Set.empty, getJsonSpec url )



-- UPDATE


type Action
  = FetchSpec (Maybe String)
  | RenderSpec (Maybe Spec)
  | TryRequest Http.Request
  | ExpansionToggled (Set String)
  | RequestResult (Result Http.RawError Http.Response)


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    FetchSpec maybeUrl ->
      let
        url =
          (Maybe.withDefault model.specUrl maybeUrl)
      in
        ( Model url model.spec model.expanded
        , getJsonSpec url
        )

    RenderSpec maybeSpec ->
      ( Model model.specUrl maybeSpec model.expanded
      , Effects.none
      )

    ExpansionToggled expanded ->
      ( Model model.specUrl model.spec expanded
      , Effects.none
      )

    TryRequest request ->
      ( model, tryRequest request )

    RequestResult result ->
      ( model, Effects.none )



-- VIEW


view : Signal.Address Action -> Model -> Html
view address { specUrl, spec, expanded } =
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
            , pathList address spec.paths expanded
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


operationEntry : Signal.Address Action -> String -> ( String, Operation ) -> Html
operationEntry address path' ( opName, op ) =
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
                , requestButton address opName path' []
                ]
            ]
        ]
    ]


requestButton : Signal.Address Action -> String -> String -> List ( Parameter, String ) -> Html
requestButton address opName path' params =
  let
    req =
      { verb = opName, headers = [], url = "", body = Http.empty }
  in
    button
      [ class "button", onClick address (TryRequest req) ]
      [ text "Send request" ]


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


operationList : Signal.Address Action -> String -> Operations -> Html
operationList address path' ops =
  dl [] (List.map (operationEntry address path') (Dict.toList ops))


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


renderPath : Signal.Address Action -> Set String -> ( String, Operations ) -> Html
renderPath address expanded ( pathName, ops ) =
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
        , operationList address pathName ops
        ]


pathEntry : Signal.Address Action -> Set String -> ( String, Operations ) -> Html
pathEntry address expanded ( p, ops ) =
  dt
    []
    [ (renderPath address expanded ( p, ops ))
    ]


pathList : Signal.Address Action -> Paths -> Set String -> Html
pathList address paths expanded =
  div
    []
    [ dl [] (List.map (pathEntry address expanded) (Dict.toList paths)) ]


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
