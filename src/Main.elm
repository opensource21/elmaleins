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

-- TODO niels 08.08.2020: Config must become configurable.
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
        currentChallenge: Maybe Challenge,
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
    -- TODO niels 08.08.2020: Local-Storage must be added.
    -- https://elmprogramming.com/saving-app-state.html
    -- https://package.elm-lang.org/packages/billstclair/elm-localstorage/latest/
    let
        listA = range 1 15
        listB = range 1 15
    in
    ( Model (Config listA listB 20 False) Maybe.Nothing 0 []
    , Cmd.none
    )

-- UPDATE
type Msg
  = StartChallenges
  | NewChallenge Challenge
  | Tick Challenge Time.Posix
  | Solved Challenge
  -- TODO niels 08.08.2020: Update solution must be added.


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    StartChallenges ->
      ( model
      , Random.generate NewChallenge (challengeGen model.config.listA model.config.listB)
      )

    Solved challenge ->
        ({model|currentChallenge = Nothing, solvedChallenges = challenge :: model.solvedChallenges},
        -- TODO niels 08.08.2020: If more than 3 errors stop otherwise StartChallenges
        Cmd.none)

    NewChallenge newChallenge ->
        ( { model  | currentChallenge = Just newChallenge, remainingTime = model.config.timeoutInSeconds },
           Cmd.none
        )

    Tick challenge _ ->
        ( { model | remainingTime = model.remainingTime - 1 } |>
             if model.remainingTime <= 1 then
                update (Solved challenge)
             else
                (\m -> (m, Cmd.none))
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
-- TODO niels 08.08.2020: The complete View part must be written!
view : Model -> Html Msg
view model =
      div []
        [ h1 [] [ text ((showMaybeChallenge model.currentChallenge) ++ " (" ++ String.fromInt model.remainingTime ++ ")") ]
        , button [ onClick StartChallenges ] [ text "Roll" ]
        ]

showMaybeChallenge: Maybe Challenge -> String
showMaybeChallenge challenge =
    case challenge of
        Nothing ->
            "no current challenge"
        Just c ->
            showChallenge c

showChallenge: Challenge -> String
showChallenge challenge =
    (String.fromInt challenge.faktorA) ++ " x " ++ (String.fromInt challenge.faktorB) ++ " = " ++ (String.fromInt challenge.result)
