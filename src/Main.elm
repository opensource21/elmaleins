module Main exposing (..)
-- Press a button to generate a random number between 1 and 6.
--
-- Read how it works:
--   https://guide.elm-lang.org/effects/random.html
--
-- https://medium.com/elm-shorts/updating-nested-records-in-elm-15d162e80480


import Browser
import Html exposing (..)
import Html.Events exposing (..)
import List exposing (drop, head, range)
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
type alias Challenge =
    {
        faktorA: Int,
        faktorB: Int,
        result: Int,
        solution: Maybe Int
    }

type alias Config =
    {
        listA: List Int,
        listB: List Int,
        timeoutInSeconds: Int,
        show: Bool
    }

type alias Model =
    {
        -- TODO niels 06.08.2020: Change to ListWithFirstElement or error-mechanism
        config: Config,
        currentChallenge: Challenge,
        remainingTime: Int,
        solvedChallenges: List Challenge
  }

getChallenge: Int -> Int -> Challenge
getChallenge a b =
    Challenge a b (a*b) Nothing

randomFactor: List Int -> Random.Generator Int
randomFactor listOfFactors =
    Random.uniform (Maybe.withDefault 0 (head listOfFactors)) (drop 1 listOfFactors)

challengeGen :  List  Int -> List Int -> Random.Generator Challenge
challengeGen  listOfAFactors  listOfBFactors =
    Random.map2
        (\a b ->  getChallenge a b)
        (randomFactor listOfAFactors)
        (randomFactor listOfBFactors)

init : () -> (Model, Cmd Msg)
init _ =
    let
        listA = range 1 15
        listB = range 1 15
    in
    ( Model (Config listA listB 20 False) (getChallenge 1 1) 0 []
    , Random.generate NewChallenge (challengeGen listA listB)
    )

-- UPDATE
type Msg
  = Next
  | NewChallenge Challenge
  | Tick Time.Posix
  | Solved


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Next ->
      ( model
      , Random.generate NewChallenge (challengeGen model.config.listA model.config.listB)
      )

    Solved ->
        -- TODO niels 07.08.2020: Move current-challenge to list
        (model, Cmd.none)

    NewChallenge newChallenge ->
        ( { model  | currentChallenge = newChallenge, remainingTime = model.config.timeoutInSeconds },
           Cmd.none
        )

    Tick _ ->
        ( { model | remainingTime = model.remainingTime - 1 } |>
             if model.remainingTime < 1 then
                update Solved
             else
                (\m -> (m, Cmd.none))
        )


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick

-- VIEW
view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text ((String.fromInt model.currentChallenge.faktorA) ++ " x " ++ (String.fromInt model.currentChallenge.faktorB) ++ " = " ++ (String.fromInt model.currentChallenge.result)) ]
    , button [ onClick Next ] [ text "Roll" ]
    ]

