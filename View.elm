module View (..) where

import Html exposing (..)
import Html.Attributes exposing (placeholder, value, class, type', src, alt, href, name)
import Html.Events exposing (onClick, on, targetValue)
import Markdown
import Regex
import Lagun exposing (Action, Model, Parameter, Operations, Paths, Response, Operation)
import Dict exposing (Dict)
import Set exposing (Set)
import Http


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
      [ button [ onClick address (Lagun.FetchSpec (Maybe.Just specUrl)) ] [ text "Lagun it!" ]
      ]
  ]


specUrlInput : Signal.Address Action -> String -> Html
specUrlInput address specUrl =
  input
    [ type' "text"
    , placeholder specUrl
    , value specUrl
    , on "input" targetValue (\newurl -> Signal.message address (Lagun.FetchSpec (Just newurl)))
    ]
    []


operationEntry : Signal.Address Action -> Lagun.ParameterValues -> String -> ( String, Operation ) -> Html
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


requestBuilder : String -> String -> Lagun.ParameterValues -> Http.Request
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
    [ class "button", onClick address (Lagun.TryRequest req) ]
    [ text "Send request" ]


parametersTableBody : Signal.Address Action -> Lagun.ParameterValues -> String -> String -> List Parameter -> Html
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


parameterEntryInput : Signal.Address Action -> Lagun.ParameterValues -> Lagun.ParameterKey -> Html
parameterEntryInput address currentValues paramKey =
  input
    [ type' "text"
    , value (Maybe.withDefault "" (Dict.get paramKey currentValues))
    , on
        "input"
        targetValue
        (\val -> Signal.message address (Lagun.ParameterInput (Dict.insert paramKey val currentValues)))
    ]
    []


parameterEntry : Signal.Address Action -> Lagun.ParameterValues -> String -> String -> Parameter -> Html
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


parameterKey : String -> String -> Parameter -> Lagun.ParameterKey
parameterKey path' opName param =
  ( path', opName, param.in', param.name )


operationList : Signal.Address Action -> Lagun.ParameterValues -> String -> Operations -> Html
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


renderPath : Signal.Address Action -> Lagun.ParameterValues -> Set String -> ( String, Operations ) -> Html
renderPath address paramValues expanded ( pathName, ops ) =
  case (Set.member pathName expanded) of
    False ->
      div
        []
        [ h5
            []
            [ a
                [ onClick address (Lagun.ExpansionToggled (Set.insert pathName expanded))
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
                [ onClick address (Lagun.ExpansionToggled (Set.remove pathName expanded))
                , href ("#" ++ pathName)
                , name pathName
                ]
                [ fontAwesome "minus-square-o" ]
            , text (" " ++ pathName)
            ]
        , operationList address paramValues pathName ops
        ]


pathEntry : Signal.Address Action -> Lagun.ParameterValues -> Set String -> ( String, Operations ) -> Html
pathEntry address paramValues expanded ( p, ops ) =
  dt
    []
    [ (renderPath address paramValues expanded ( p, ops ))
    ]


pathList : Signal.Address Action -> Lagun.ParameterValues -> Paths -> Set String -> Html
pathList address paramValues paths expanded =
  div
    []
    [ dl [] (List.map (pathEntry address paramValues expanded) (Dict.toList paths)) ]


fontAwesome : String -> Html
fontAwesome name =
  span [ class ("fa fa-" ++ name) ] []
