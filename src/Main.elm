module Main exposing (..)

import Browser
import Json exposing (fromJson)
import Json.Encode as Encode
import List exposing (range)
import Model exposing (..)
import Ports
import Random
import Time
import View exposing (view)



-- MAIN


main : Program (Maybe String) Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


getChallenge : Bool -> Int -> Int -> Challenge
getChallenge reverse a b =
    if reverse then
        Challenge Nothing Nothing (Just (a * b))

    else
        Challenge (Just a) (Just b) Nothing


randomFactor : FactorPool -> Random.Generator Int
randomFactor factorPool =
    Random.uniform factorPool.firstElement factorPool.furtherElements


challengeGen : Bool -> FactorPool -> FactorPool -> Random.Generator Challenge
challengeGen reverse factorPoolA factorPoolB =
    Random.map2
        (\a b -> getChallenge reverse a b)
        (randomFactor factorPoolA)
        (randomFactor factorPoolB)


init : Maybe String -> ( Model, Cmd Msg )
init flags =
    let
        config =
            Maybe.withDefault
                (Config (FactorPool 2 (range 3 14) "") (FactorPool 1 (range 10 14) "") 20 False False)
                (fromJson flags)
    in
    ( Model config Maybe.Nothing 0 []
    , Cmd.none
    )



-- UPDATE


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
            ( { model | config = newConfig }, saveConfig newConfig )

        StartChallenges ->
            ( { model | solvedChallenges = [] }
            , Random.generate NewChallenge (challengeGen model.config.reverseChallenges model.config.poolA model.config.poolB)
            )

        StopChallenges ->
            ( { model | currentChallenge = Nothing }, Cmd.none )

        Result challenge result ->
            let
                newChallenge =
                    Just { challenge | result = String.toInt result }
            in
            ( { model | currentChallenge = newChallenge }, Cmd.none )

        FaktorA challenge faktor ->
            let
                newChallenge =
                    Just { challenge | faktorA = String.toInt faktor }
            in
            ( { model | currentChallenge = newChallenge }, Cmd.none )

        FaktorB challenge faktor ->
            let
                newChallenge =
                    Just { challenge | faktorB = String.toInt faktor }
            in
            ( { model | currentChallenge = newChallenge }, Cmd.none )

        Solved challenge ->
            ( { model | currentChallenge = Nothing, solvedChallenges = challenge :: model.solvedChallenges }
            , if count challengeResultWrong (challenge :: model.solvedChallenges) < 3 then
                Random.generate NewChallenge (challengeGen model.config.reverseChallenges model.config.poolA model.config.poolB)

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



-- Ports


saveConfig : Config -> Cmd msg
saveConfig config =
    Ports.storeConfig (Encode.encode 0 (Json.toJson config))
