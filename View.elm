module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (placeholder, value, class, type', src, alt, href, name)
import Html.Events exposing (onClick, onInput, targetValue)
import Markdown
import Regex
import Lagun exposing (Msg, Model, Parameter, ParameterKey, ParameterValues, Operations, Paths, Response, Operation, RequestResults, Spec)
import Dict exposing (Dict)
import Set exposing (Set)
import Http
import String


view : Model -> Html Msg
view { specUrl, spec, expanded, paramValues, requestResults } =
    case spec of
        Maybe.Just spec ->
            div [ class "container" ]
                [ div [ class "row" ]
                    (headerHtml specUrl)
                , div []
                    [ h1 [] [ text spec.info.title ]
                    , Markdown.toHtml [ class "div" ] spec.info.description
                    , p [] [ text ("API Version: " ++ spec.info.version) ]
                    , hr [] []
                    , pathList spec paramValues expanded requestResults
                    ]
                ]

        Maybe.Nothing ->
            div [ class "container" ]
                [ p [] [ text "No API specification found, or something went wrong while parsing it. U_U" ]
                , div [ class "row" ]
                    (headerHtml specUrl)
                ]


headerHtml : String -> List (Html Msg)
headerHtml specUrl =
    [ div [ class "column column-80" ]
        [ specUrlInput specUrl ]
    , div [ class "column column-20" ]
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


operationEntry : String -> ParameterValues -> String -> RequestResults -> ( String, Operation ) -> Html Msg
operationEntry url paramValues path' results ( opName, op ) =
    dt []
        [ div [ class "row" ]
            [ div [ class ("column column-10 method-" ++ opName) ]
                [ text opName ]
            , div [ class ("column column-90 method-summary-" ++ opName) ]
                [ text op.summary ]
            ]
        , div [ class "row" ]
            [ div [ class ("column column-100 params-and-request method-summary-" ++ opName) ]
                [ div []
                    [ h6 [] [ text "Parameters" ]
                    , parametersTable (parametersTableBody paramValues path' opName op.parameters)
                    , h6 [] [ text "Responses" ]
                    , responsesTable op.responses
                    , requestButton path' opName (requestBuilder url opName path' paramValues)
                    , requestResult ( path', opName ) results
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


parameterValuesIn : ParameterValues -> String -> List ( ( String, String, String, String ), String )
parameterValuesIn paramValues in' =
    Dict.filter (\( _, _, i, _ ) _ -> i == in') paramValues
        |> Dict.toList


requestBuilder : String -> String -> String -> ParameterValues -> Http.Request
requestBuilder url verb path' paramValues =
    -- TODO Add support for formData
    let
        relevantParamValues =
            Dict.filter (\( p, v, _, _ ) _ -> p == path' && v == verb) paramValues

        pathParams =
            -- yay :(
            parameterValuesIn relevantParamValues "path"
                |> List.map (\( ( _, _, _, name ), val ) -> ( "{" ++ name ++ "}", val ))
                |> Dict.fromList

        queryParams =
            parameterValuesIn relevantParamValues "query"
                |> List.map (\( ( _, _, _, name ), val ) -> ( name, val ))

        headerParams =
            parameterValuesIn relevantParamValues "header"
                |> List.map (\( ( _, _, _, name ), val ) -> ( name, val ))

        bodyParam =
            parameterValuesIn relevantParamValues "body"
                |> List.map (\( ( _, _, _, _ ), val ) -> Http.string val)
                |> List.head

        otherHeaders =
            Maybe.map (\_ -> [ ( "Content-Type", "application/json" ) ]) bodyParam
    in
        { verb = verb
        , headers =
            [ ( "Accept", "application/json" ) ] ++ headerParams ++ (Maybe.withDefault [] otherHeaders)
            -- application/xml, TODO these reside in paths.<path>.<method>.produces[]
        , url = Http.url (url ++ (pathWithVariables path' pathParams)) queryParams
        , body = Maybe.withDefault Http.empty bodyParam
        }


requestButton : String -> String -> Http.Request -> Html Msg
requestButton path' verb req =
    button [ class "button", onClick (Lagun.TryRequest ( path', verb ) req) ]
        [ text ("Request " ++ verb) ]


requestResult : ( String, String ) -> RequestResults -> Html msg
requestResult key results =
    case (Dict.member key results) of
        True ->
            div []
                [ h6 []
                    [ text "Request result" ]
                , div [ class "http-response" ] [ showHttpResponse (Dict.get key results) ]
                ]

        False ->
            span [] []


showHttpCode : Int -> String -> List (Html msg)
showHttpCode code statusText =
    let
        base =
            [ text "Response code: ", strong [] [ text (toString code) ], text (" - " ++ statusText ++ " ") ]
    in
        if code >= 200 && code < 300 then
            List.append base [ fontAwesome "check-square-o" ]
        else if code >= 400 && code < 500 then
            List.append base [ fontAwesome "exclamation-circle", fontAwesome "keyboard-o" ]
        else if code >= 500 && code < 600 then
            List.append base [ fontAwesome "exclamation-circle", fontAwesome "server" ]
        else
            base


showHttpResponse : Maybe Http.Response -> Html msg
showHttpResponse mr =
    case mr of
        Maybe.Just { status, statusText, value, headers } ->
            case value of
                Http.Text str ->
                    dt []
                        [ dl []
                            (showHttpCode status statusText)
                        , dl []
                            [ text ("Response body:\n"), div [ class "code-box" ] [ code [ class "code-text" ] [ text str ] ] ]
                        , dl []
                            [ text ("Response headers:\n")
                            , div [ class "code-box" ]
                                [ code [ class "code-text" ]
                                    [ text (String.join "\n" (List.map (\( k, v ) -> (k ++ ": " ++ v)) (Dict.toList headers))) ]
                                ]
                            ]
                        ]

                _ ->
                    dt []
                        [ dl []
                            (showHttpCode status statusText)
                        , dl []
                            [ text ("Response body:\n"), div [ class "code-box" ] [ code [ class "code-text" ] [ text "Unknown (non-string) data" ] ] ]
                        , dl []
                            [ text ("Response headers:\n")
                            , div [ class "code-box" ]
                                [ code [ class "code-text" ] [ text (String.join "<br />\n" (List.map (\( k, v ) -> (k ++ ": " ++ v)) (Dict.toList headers))) ] ]
                            ]
                        ]

        Maybe.Nothing ->
            span [] []


parametersTableBody : ParameterValues -> String -> String -> List Parameter -> Html Msg
parametersTableBody paramValues path' opName ps =
    tbody []
        (List.map (parameterEntry paramValues path' opName) ps)


parametersTable : Html msg -> Html msg
parametersTable tableBody =
    table []
        [ thead []
            [ tr []
                [ th []
                    [ text "Parameter" ]
                , th []
                    [ text "Value" ]
                , th []
                    [ text "Description" ]
                , th []
                    [ text "Parameter Type" ]
                , th []
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
    tr []
        [ td []
            [ text param.name ]
        , td []
            [ parameterEntryInput currentValues (parameterKey path' opName param)
            ]
        , td []
            [ text param.description ]
        , td []
            [ text param.in' ]
        , td []
            [ text param.type' ]
        ]


parameterKey : String -> String -> Parameter -> ParameterKey
parameterKey path' opName param =
    ( path', opName, param.in', param.name )


operationList : String -> ParameterValues -> String -> Operations -> RequestResults -> Html Msg
operationList url paramValues path' ops results =
    dl [] (List.map (operationEntry url paramValues path' results) (Dict.toList ops))


responseEntry : ( String, Response ) -> Html msg
responseEntry ( httpCode, r ) =
    tr []
        [ td []
            [ text httpCode ]
        , td []
            [ text r.description ]
          -- Markdown.toHtml
        , td []
            [ text "" ]
          -- TODO response model
        , td []
            [ text "" ]
          -- TODO headers
        ]


responsesTable : Dict String Response -> Html msg
responsesTable rs =
    table []
        [ thead []
            [ tr []
                [ th []
                    [ text "HTTP Code" ]
                , th []
                    [ text "Reason" ]
                , th []
                    [ text "Response model" ]
                , th []
                    [ text "Headers" ]
                ]
            ]
        , tbody []
            (List.map responseEntry (Dict.toList rs))
        ]


renderHiddenPath : String -> Set String -> Html Msg
renderHiddenPath pathName expanded =
    div []
        [ h5 []
            [ a
                [ onClick (Lagun.ExpansionToggled (Set.insert pathName expanded))
                , href ("#" ++ pathName)
                , name pathName
                ]
                [ fontAwesome "plus-square-o" ]
            , text (" " ++ pathName)
            ]
        ]


renderExpandedPath : String -> Set String -> Html Msg -> Html Msg
renderExpandedPath pathName expanded opsList =
    div []
        [ h5 []
            [ a
                [ onClick (Lagun.ExpansionToggled (Set.remove pathName expanded))
                , href ("#" ++ pathName)
                , name pathName
                ]
                [ fontAwesome "minus-square-o" ]
            , text (" " ++ pathName)
            ]
        , opsList
        ]


renderPath : String -> Set String -> Html Msg -> Html Msg
renderPath pathName expanded opsList =
    case (Set.member pathName expanded) of
        False ->
            renderHiddenPath pathName expanded

        True ->
            renderExpandedPath pathName expanded opsList


pathEntry : String -> Set String -> Html Msg -> Html Msg
pathEntry pathName expanded opsList =
    dt []
        [ (renderPath pathName expanded opsList)
        ]


pathList : Spec -> ParameterValues -> Set String -> RequestResults -> Html Msg
pathList spec paramValues expanded results =
    div []
        [ dl []
            (List.map
                (\( pathName, ops ) ->
                    pathEntry pathName expanded (operationList ("http://" ++ spec.host ++ spec.basePath) paramValues pathName ops results)
                )
                (Dict.toList spec.paths)
            )
        ]


fontAwesome : String -> Html msg
fontAwesome name =
    span [ class ("fa fa-" ++ name) ] []
