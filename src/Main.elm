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
import List exposing (range)
import Random



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
        result: Int
    }

type alias SolvedChallenge =
    {
        challenge: Challenge,
        solution: Int
    }

type alias Model =
  {
        configA: List Int,
        configB: List Int,
        currentChallenge: Challenge,
        solvedChallenges: List Challenge
  }

getChallenge: Int -> Int -> Challenge
getChallenge a b =
    Challenge a b (a*b)

challengeGen :  List  Int -> List Int -> Random.Generator Challenge
challengeGen  listOfAFactors  listOfBFactors =
    Random.map2
        (\a b ->  getChallenge a b)  (randomFactor listOfAFactors) (randomFactor listOfBFactors)

randomFactor: List Int -> Random.Generator Int
randomFactor listOfFactors =
    Random.uniform (first listOfFactors) (List.drop 1 listOfFactors)

first: List Int -> Int
first  list =
    case List.head list of
        Nothing -> 0
        Just elem -> elem

init : () -> (Model, Cmd Msg)
init _ =
    let
        listA = range 1 15
        listB = range 1 15
    in
    ( Model listA listB (getChallenge 1 1) []
    , Random.generate NewChallenge (challengeGen listA listB)
    )



-- UPDATE


type Msg
  = Roll
  | NewChallenge Challenge


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      ( model
      , Cmd.none
      )

    NewChallenge newChallenge ->
        ( { model  | currentChallenge = newChallenge}
            , Cmd.none
        )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text ((String.fromInt model.currentChallenge.faktorA) ++ " x " ++ (String.fromInt model.currentChallenge.faktorB) ++ " = " ++ (String.fromInt model.currentChallenge.result)) ]
    , button [ onClick Roll ] [ text "Roll" ]
    ]

