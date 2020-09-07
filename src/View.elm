module View exposing (view)

import Html exposing (..)
import Html.Attributes as Attributes exposing (attribute, class, id, name, placeholder, size, type_, value)
import Html.Events exposing (..)
import Model exposing (Challenge, FactorPool, Model, Msg(..), challengeResultCorrect, count)



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container-sm" ]
        [ h1 [] [ text "1x1-Trainer" ]
        , showConfiguration model
        , showControl model
        , showMaybeChallenge model
        , showResults model
        ]


showConfiguration : Model -> Html Msg
showConfiguration model =
    if model.config.show then
        div [ id "configuration" ]
            [ h2 [] [ text "Konfiguration" ]
            , showFactorConfig model.config.poolA "A" ChangesPoolA AddToPoolA RemoveFromPoolA
            , showFactorConfig model.config.poolB "B" ChangesPoolB AddToPoolB RemoveFromPoolB
            , div [ class "input-group", class "input-group-sm", class "mb-3" ]
                [ input
                    [ type_ "number"
                    , class "form-control"
                    , name "timeout"
                    , Attributes.min "2"
                    , Attributes.max "30"
                    , size 2
                    , attribute "aria-label" "Definiere den Timeout."
                    , onInput ChangeTimeout
                    , value (String.fromInt model.config.timeoutInSeconds)
                    ]
                    []
                , div [ class "input-group-append" ]
                    [ span [ class "input-group-text" ] [ text " s Zeit " ]
                    ]
                , div [ class "input-group-text" ]
                    [ input
                        [ type_ "checkbox"
                        , attribute "aria-label" "Umgekehrt"
                        , Attributes.readonly (not (model.currentChallenge == Nothing))
                        , Attributes.checked model.config.reverseChallenges
                        , onClick (ChangeReverse (not model.config.reverseChallenges))
                        ]
                        []
                    ]
                , div [ class "input-group-append" ]
                    [ span [ class "input-group-text" ] [ text " Umgekehrt" ]
                    ]
                ]
            , hr [] []
            ]

    else
        text ""


showFactorConfig : FactorPool -> String -> (String -> Msg) -> Msg -> Msg -> Html Msg
showFactorConfig pool configName changeMsg addMsg rmMsg =
    div [ id ("factor" ++ configName ++ "Config") ]
        [ p []
            [ strong [] [ text ("Faktor " ++ configName ++ ": ") ]
            , text (String.join ", " (List.map String.fromInt (List.sort (pool.firstElement :: pool.furtherElements))))
            ]
        , div [ class "input-group", class "input-group-sm", class "mb-3" ]
            [ input
                [ type_ "tel"
                , attribute "inputmode" "decimal"
                , class "form-control"
                , size 10
                , placeholder "3-5, 8"
                , Attributes.name ("range_" ++ configName)
                , attribute "aria-label" ("Faktoren für " ++ configName)
                , onInput changeMsg
                , value pool.changes
                ]
                []
            , div [ class "input-group-append" ]
                [ button [ type_ "button", class "btn", class "btn-success", name ("add_" ++ configName), onClick addMsg ] [ text "Hinzufügen" ]
                , button [ type_ "button", class "btn", class "btn-warning", name ("remove_" ++ configName), onClick rmMsg ] [ text "Entfernen" ]
                ]
            ]
        ]


showControl : Model -> Html Msg
showControl model =
    div [ id "control" ]
        [ if model.config.show then
            button [ type_ "button", class "btn", class "btn-primary", name "hide", onClick (HideConfig model.config) ] [ text "Verdecke und Speichere Config" ]

          else
            button [ type_ "button", class "btn", class "btn-primary", name "show", onClick (ShowConfig model.config) ] [ text "Zeige Config" ]
        , text " "
        , case model.currentChallenge of
            Nothing ->
                button [ type_ "button", class "btn", class "btn-primary", name "start", onClick StartChallenges ] [ text "Start" ]

            Just _ ->
                button [ type_ "button", class "btn", class "btn-primary", name "start", onClick StopChallenges ] [ text "Stop" ]
        ]


showMaybeChallenge : Model -> Html Msg
showMaybeChallenge model =
    case model.currentChallenge of
        Nothing ->
            text ""

        Just challenge ->
            showCurrentChallenge challenge model.remainingTime model.config.reverseChallenges


showCurrentChallenge : Challenge -> Int -> Bool -> Html Msg
showCurrentChallenge challenge remainingTime reverse =
    div [ id "challenge" ]
        [ h2 [] [ text "Aufgabe" ]
        , p [] [ text ("Noch " ++ String.fromInt remainingTime ++ " Sekunden") ]
        , if reverse then
            showReverseChallenge challenge

          else
            showNormalChallenge challenge
        ]


showNormalChallenge : Challenge -> Html Msg
showNormalChallenge challenge =
    div [ class "input-group", class "input-group-sm", class "mb-3" ]
        [ div [ class "input-group-prepend" ]
            [ span [ class "input-group-text" ] [ text (maybeIntToString challenge.faktorA ++ " x " ++ maybeIntToString challenge.faktorB ++ " = ") ]
            ]
        , numberInput "result" "Ergebnis" challenge.result (Result challenge)
        , div [ class "input-group-append" ]
            [ button [ type_ "button", class "btn", class "btn-success", name "next", onClick (Solved challenge) ] [ text "Abgeben" ]
            ]
        ]


showReverseChallenge : Challenge -> Html Msg
showReverseChallenge challenge =
    div [ class "input-group", class "input-group-sm", class "mb-3" ]
        [ numberInput "faktorA" "FaktorA" challenge.faktorA (FaktorA challenge)
        , div [ class "input-group-append" ]
            [ span [ class "input-group-text" ] [ text " x " ] ]
        , numberInput "faktorB" "faktorB" challenge.faktorB (FaktorB challenge)
        , div [ class "input-group-append" ]
            [ span [ class "input-group-text" ] [ text (" = " ++ maybeIntToString challenge.result) ] ]
        , div [ class "input-group-append" ]
            [ button [ type_ "button", class "btn", class "btn-success", name "next", onClick (Solved challenge) ] [ text "Abgeben" ]
            ]
        ]


numberInput : String -> String -> Maybe Int -> (String -> Msg) -> Html Msg
numberInput attributName label startValue msg =
    input [ type_ "number", class "form-control", Attributes.min "2", Attributes.max "900", size 3, name attributName, attribute "aria-label" label, value (maybeIntToString startValue), onInput msg ]
        []


showResults : Model -> Html Msg
showResults model =
    if List.isEmpty model.solvedChallenges then
        text ""

    else
        div [ id "results" ]
            ([ hr [] []
             , h2 [] [ text "Ergebnisse" ]
             , showSuccessRate model.solvedChallenges
             ]
                ++ showSolvedChallenges model.solvedChallenges
            )


showSuccessRate : List Challenge -> Html Msg
showSuccessRate challenges =
    p [] [ text (String.fromInt (count challengeResultCorrect challenges) ++ " von " ++ String.fromInt (List.length challenges) ++ " Richtig.") ]


showSolvedChallenges : List Challenge -> List (Html Msg)
showSolvedChallenges challenges =
    List.map showSolvedChallenge challenges


showSolvedChallenge : Challenge -> Html Msg
showSolvedChallenge challenge =
    if challengeResultCorrect challenge then
        p [ class "alert", class "alert-success", attribute "role" "alert" ]
            [ text ("Richtig " ++ challengeToString challenge)
            ]

    else
        p [ class "alert", class "alert-warning", attribute "role" "alert" ]
            [ text ("Nicht ganz " ++ challengeToString challenge ++ " und nicht " ++ maybeIntToString challenge.result)
            ]


challengeToString : Challenge -> String
challengeToString challenge =
    maybeIntToString challenge.faktorA ++ " x " ++ maybeIntToString challenge.faktorB ++ " = " ++ maybeIntToString (calcResult challenge)


calcResult : Challenge -> Maybe Int
calcResult challenge =
    case ( challenge.faktorA, challenge.faktorB ) of
        ( Nothing, Nothing ) ->
            Nothing

        ( Nothing, Just _ ) ->
            Nothing

        ( Just _, Nothing ) ->
            Nothing

        ( Just a, Just b ) ->
            Just (a * b)


maybeIntToString : Maybe Int -> String
maybeIntToString maybeInt =
    Maybe.withDefault "" (Maybe.map String.fromInt maybeInt)
