module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (placeholder, value, class, type', src, alt, href, name)
import Html.Events exposing (onClick, onInput, targetValue)
import Markdown
import Regex
import Lagun exposing (Msg, Model, Parameter, ParameterKey, ParameterValues, Operations, Paths, Response, Operation)
import Dict exposing (Dict)
import Set exposing (Set)
import Http


view : Model -> Html Msg
view { specUrl, spec, expanded, paramValues } =
  case spec of
    Maybe.Just spec ->
      div
        [ class "container" ]
        [ div
            [ class "row" ]
            (headerHtml specUrl)
        , div
            []
            [ h1 [] [ text spec.info.title ]
            , Markdown.toHtml [class "div"] spec.info.description
            , p [] [ text ("API Version: " ++ spec.info.version) ]
            , hr [] []
            , pathList paramValues spec.paths expanded
            ]
        ]

    Maybe.Nothing ->
      div
        [ class "container" ]
        [ p [] [ text "No API specification found, or something went wrong while parsing it. U_U" ]
        , div
            [ class "row" ]
            (headerHtml specUrl)
        ]


headerHtml : String -> List (Html Msg)
headerHtml specUrl =
  [ div
      [ class "column column-80" ]
      [ specUrlInput specUrl ]
  , div
      [ class "column column-20" ]
      [ button [ onClick (Lagun.FetchSpec (Maybe.Just specUrl)) ] [ text "Lagun it!" ]
      ]
  ]


specUrlInput : String -> Html Msg
specUrlInput specUrl =
  input
    [ type' "text"
    , placeholder specUrl
    , value specUrl
    , onInput (\i -> Lagun.FetchSpec (Maybe.Just i))
    ]
    []


operationEntry : ParameterValues -> String -> ( String, Operation ) -> Html Msg
operationEntry paramValues path' ( opName, op ) =
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
                , parametersTable (parametersTableBody paramValues path' opName op.parameters)
                , h6 [] [ text "Responses" ]
                , responsesTable op.responses
                , requestButton (requestBuilder opName path' paramValues)
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
    , url = ("http://petstore.swagger.io/v2" ++ (pathWithVariables path' relevantPathParams))
    , body = Http.empty
    }


requestButton : Http.Request -> Html Msg
requestButton req =
  button
    [ class "button", onClick (Lagun.TryRequest req) ]
    [ text "Send request" ]


parametersTableBody : ParameterValues -> String -> String -> List Parameter -> Html Msg
parametersTableBody paramValues path' opName ps =
  tbody
    []
    (List.map (parameterEntry paramValues path' opName) ps)


parametersTable : Html msg -> Html msg
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


parameterEntryInput : ParameterValues -> ParameterKey -> Html Msg
parameterEntryInput currentValues paramKey =
  input
    [ type' "text"
    , value (Maybe.withDefault "" (Dict.get paramKey currentValues))
    , onInput (\val -> (Lagun.ParameterInput (Dict.insert paramKey val currentValues)))
    ]
    []


parameterEntry : ParameterValues -> String -> String -> Parameter -> Html Msg
parameterEntry currentValues path' opName param =
  tr
    []
    [ td
        []
        [ text param.name ]
    , td
        []
        [ parameterEntryInput currentValues (parameterKey path' opName param)
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


operationList : ParameterValues -> String -> Operations -> Html Msg
operationList paramValues path' ops =
  dl [] (List.map (operationEntry paramValues path') (Dict.toList ops))


responseEntry : ( String, Response ) -> Html msg
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


responsesTable : Dict String Response -> Html msg
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


renderPath : ParameterValues -> Set String -> ( String, Operations ) -> Html Msg
renderPath paramValues expanded ( pathName, ops ) =
  case (Set.member pathName expanded) of
    False ->
      div
        []
        [ h5
            []
            [ a
                [ onClick (Lagun.ExpansionToggled (Set.insert pathName expanded))
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
                [ onClick (Lagun.ExpansionToggled (Set.remove pathName expanded))
                , href ("#" ++ pathName)
                , name pathName
                ]
                [ fontAwesome "minus-square-o" ]
            , text (" " ++ pathName)
            ]
        , operationList paramValues pathName ops
        ]


pathEntry : ParameterValues -> Set String -> ( String, Operations ) -> Html Msg
pathEntry paramValues expanded ( p, ops ) =
  dt
    []
    [ (renderPath paramValues expanded ( p, ops ))
    ]


pathList : ParameterValues -> Paths -> Set String -> Html Msg
pathList paramValues paths expanded =
  div
    []
    [ dl [] (List.map (pathEntry paramValues expanded) (Dict.toList paths)) ]


fontAwesome : String -> Html msg
fontAwesome name =
  span [ class ("fa fa-" ++ name) ] []
