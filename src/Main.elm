module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes as Attributes exposing (attribute, class, id, name, placeholder, size, type_, value)
import Html.Events exposing (..)
import List exposing (range)
import Random
import Time



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL
-- Maybe because we wan't to allow each at input, which could be empty.


type alias Challenge =
    { faktorA : Maybe Int
    , faktorB : Maybe Int
    , result : Maybe Int
    }


type alias FactorPool =
    { firstElement : Int
    , furtherElements : List Int
    , changes : String
    }


setChanges : String -> FactorPool -> FactorPool
setChanges newChanges pool =
    { pool | changes = newChanges }


addChanges : FactorPool -> FactorPool
addChanges pool =
    { pool | furtherElements = List.append pool.furtherElements (convertToList pool.changes), changes = "" }


removeChanges : FactorPool -> FactorPool
removeChanges pool =
    List.foldl removeElement { pool | changes = "" } (convertToList pool.changes)


{-| Remove the search element from pool only once.
-}
removeElement : Int -> FactorPool -> FactorPool
removeElement search pool =
    case pool.furtherElements of
        [] ->
            pool

        first :: others ->
            if pool.firstElement == search then
                { pool | firstElement = first, furtherElements = others }

            else
                { pool | furtherElements = removeFromList search pool.furtherElements }


{-| Remove the first occurrence of a value from a list.
-}
removeFromList : a -> List a -> List a
removeFromList search list =
    case list of
        [] ->
            []

        first :: other ->
            if first == search then
                other

            else
                first :: removeFromList search other


setPoolA : FactorPool -> Config -> Config
setPoolA pool config =
    { config | poolA = pool }


setPoolB : FactorPool -> Config -> Config
setPoolB pool config =
    { config | poolB = pool }


setConfig : Config -> Model -> Model
setConfig newConfig model =
    { model | config = newConfig }



-- TODO niels 08.08.2020: Config must become persistent.


type alias Config =
    { poolA : FactorPool
    , poolB : FactorPool
    , timeoutInSeconds : Int
    , -- for reverse Challenges we only show the result and the user must guess the factors.
      reverseChallenges : Bool
    , show : Bool
    }


setTimeoutInSeconds : Config -> Maybe Int -> Config
setTimeoutInSeconds config newTime =
    case newTime of
        Nothing ->
            config

        Just time ->
            { config | timeoutInSeconds = time }


setReverse : Config -> Bool -> Config
setReverse config newReverse =
    { config | reverseChallenges = newReverse }


{-| Convert a list definition to a list of int.
convertToList "1, 3-5" == [1,3,4,5]
-}
convertToList : String -> List Int
convertToList definition =
    List.sort (List.foldl List.append [] (List.map convertElementToList (List.map String.trim (String.split "," definition))))


{-| Convert 1 to [1] or 1-2 to range 1 2
convertElementToList "1-3" == [1,2,3]
-}
convertElementToList : String -> List Int
convertElementToList definition =
    if String.contains "-" definition then
        convertToRange (Array.fromList (List.filterMap String.toInt (List.map String.trim (String.split "-" definition))))

    else
        case String.toInt definition of
            Nothing ->
                []

            Just i ->
                [ i ]


convertToRange : Array Int -> List Int
convertToRange array =
    let
        a =
            Array.get 0 array

        b =
            Array.get 1 array
    in
    case ( a, b ) of
        ( Nothing, Nothing ) ->
            []

        ( Nothing, Just end ) ->
            [ end ]

        ( Just start, Nothing ) ->
            [ start ]

        ( Just start, Just end ) ->
            List.range start end


type alias Model =
    { config : Config
    , currentChallenge : Maybe Challenge
    , remainingTime : Int
    , solvedChallenges : List Challenge
    }


getChallenge : Int -> Int -> Challenge
getChallenge a b =
    Challenge (Just a) (Just b) Nothing


randomFactor : FactorPool -> Random.Generator Int
randomFactor factorPool =
    Random.uniform factorPool.firstElement factorPool.furtherElements


challengeGen : FactorPool -> FactorPool -> Random.Generator Challenge
challengeGen factorPoolA factorPoolB =
    Random.map2
        (\a b -> getChallenge a b)
        (randomFactor factorPoolA)
        (randomFactor factorPoolB)


init : () -> ( Model, Cmd Msg )
init _ =
    -- TODO niels 08.08.2020: Local-Storage must be added.
    -- https://elmprogramming.com/saving-app-state.html
    -- https://package.elm-lang.org/packages/billstclair/elm-localstorage/latest/
    let
        listA =
            range 3 14

        listB =
            range 10 14
    in
    ( Model (Config (FactorPool 2 listA "") (FactorPool 1 listB "") 20 False False) Maybe.Nothing 0 []
    , Cmd.none
    )



-- UPDATE


type Msg
    = StartChallenges
    | StopChallenges
    | NewChallenge Challenge
    | Tick Challenge Time.Posix
    | Solved Challenge
    | Result Challenge String
    | ShowConfig Config
    | HideConfig Config
    | ChangeTimeout String
    | ChangeReverse Bool
    | AddToPoolA
    | RemoveFromPoolA
    | ChangesPoolA String
    | AddToPoolB
    | RemoveFromPoolB
    | ChangesPoolB String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeTimeout newTime ->
            ( { model | config = setTimeoutInSeconds model.config (String.toInt newTime) }, Cmd.none )

        ChangeReverse newReverse ->
            ( { model | config = setReverse model.config newReverse }, Cmd.none )

        ChangesPoolA input ->
            ( model |> setConfig (model.config |> setPoolA (model.config.poolA |> setChanges input)), Cmd.none )

        ChangesPoolB input ->
            ( model |> setConfig (model.config |> setPoolB (model.config.poolB |> setChanges input)), Cmd.none )

        AddToPoolA ->
            ( model |> setConfig (model.config |> setPoolA (model.config.poolA |> addChanges)), Cmd.none )

        RemoveFromPoolA ->
            ( model |> setConfig (model.config |> setPoolA (model.config.poolA |> removeChanges)), Cmd.none )

        AddToPoolB ->
            ( model |> setConfig (model.config |> setPoolB (model.config.poolB |> addChanges)), Cmd.none )

        RemoveFromPoolB ->
            ( model |> setConfig (model.config |> setPoolB (model.config.poolB |> removeChanges)), Cmd.none )

        ShowConfig config ->
            let
                newConfig =
                    { config | show = True }
            in
            ( { model | config = newConfig }, Cmd.none )

        HideConfig config ->
            let
                newConfig =
                    { config | show = False }
            in
            ( { model | config = newConfig }, Cmd.none )

        StartChallenges ->
            ( { model | solvedChallenges = [] }
            , Random.generate NewChallenge (challengeGen model.config.poolA model.config.poolB)
            )

        StopChallenges ->
            ( { model | currentChallenge = Nothing }, Cmd.none )

        Result challenge result ->
            let
                newChallenge =
                    Just { challenge | result = String.toInt result }
            in
            ( { model | currentChallenge = newChallenge }, Cmd.none )

        Solved challenge ->
            ( { model | currentChallenge = Nothing, solvedChallenges = challenge :: model.solvedChallenges }
            , if numberOfWrongChallenges (challenge :: model.solvedChallenges) < 3 then
                Random.generate NewChallenge (challengeGen model.config.poolA model.config.poolB)

              else
                Cmd.none
            )

        NewChallenge newChallenge ->
            ( { model | currentChallenge = Just newChallenge, remainingTime = model.config.timeoutInSeconds }
            , Cmd.none
            )

        Tick challenge _ ->
            { model | remainingTime = model.remainingTime - 1 }
                |> (if model.remainingTime <= 1 then
                        update (Solved challenge)

                    else
                        \m -> ( m, Cmd.none )
                   )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.currentChallenge of
        Nothing ->
            Sub.none

        Just c ->
            Time.every 1000 (Tick c)



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
                    [ input [ type_ "checkbox", attribute "aria-label" "Umgekehrt", Attributes.checked model.config.reverseChallenges, onClick (ChangeReverse (not model.config.reverseChallenges)) ] []
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
            button [ type_ "button", class "btn", class "btn-primary", name "hide", onClick (HideConfig model.config) ] [ text "Verdecke Config" ]

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

        Just c ->
            showCurrentChallenge c model.remainingTime



-- TODO niels 17.08.2020: Reverse must be implemented.


showCurrentChallenge : Challenge -> Int -> Html Msg
showCurrentChallenge challenge remainingTime =
    div [ id "challenge" ]
        [ h2 [] [ text "Aufgabe" ]
        , p [] [ text ("Noch " ++ String.fromInt remainingTime ++ " Sekunden") ]
        , div [ class "input-group", class "input-group-sm", class "mb-3" ]
            [ div [ class "input-group-prepend" ]
                [ span [ class "input-group-text" ] [ text (maybeIntToString challenge.faktorA ++ " x " ++ maybeIntToString challenge.faktorB ++ " = ") ]
                ]
            , input
                [ type_ "number"
                , class "form-control"
                , Attributes.min "2"
                , Attributes.max "900"
                , size 3
                , name "result"
                , attribute "aria-label" "Ergebnis"
                , value (maybeIntToString challenge.result)
                , onInput (Result challenge)
                ]
                []
            , div [ class "input-group-append" ]
                [ button [ type_ "button", class "btn", class "btn-success", name "next", onClick (Solved challenge) ] [ text "Abgeben" ]
                ]
            ]
        ]


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
    p [] [ text (String.fromInt (numberOfCorrectChallenges challenges) ++ " von " ++ String.fromInt (List.length challenges) ++ " Richtig.") ]


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


numberOfWrongChallenges : List Challenge -> Int
numberOfWrongChallenges challenges =
    List.length (List.filter challengeResultWrong challenges)


numberOfCorrectChallenges : List Challenge -> Int
numberOfCorrectChallenges challenges =
    List.length (List.filter challengeResultCorrect challenges)


challengeResultCorrect : Challenge -> Bool
challengeResultCorrect challenge =
    Maybe.withDefault 0 challenge.faktorA * Maybe.withDefault 0 challenge.faktorB == Maybe.withDefault -1 challenge.result


challengeResultWrong : Challenge -> Bool
challengeResultWrong challenge =
    not (challengeResultCorrect challenge)


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
