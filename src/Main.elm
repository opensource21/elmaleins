module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes as Attributes exposing (attribute, class, id, name, placeholder, size, type_)
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
    {
        faktorA: Maybe Int,
        faktorB: Maybe Int,
        result: Maybe Int
    }

type alias FactorPool =
    {
        firstElement: Int,
        furtherElements: List Int,
        changes: String
    }

-- TODO niels 08.08.2020: Config must become configurable.
type alias Config =
    {
        poolA: FactorPool,
        poolB: FactorPool,
        timeoutInSeconds: Int,
        -- for reverse Challenges we only show the result and the user must guess the factors.
        reverseChallenges: Bool,
        show: Bool
    }

type alias Model =
    {
        config: Config,
        currentChallenge: Maybe Challenge,
        remainingTime: Int,
        solvedChallenges: List Challenge
  }

getChallenge: Int -> Int -> Challenge
getChallenge a b =
    Challenge (Just a) (Just b) Nothing

randomFactor: FactorPool -> Random.Generator Int
randomFactor factorPool =
    Random.uniform factorPool.firstElement factorPool.furtherElements

challengeGen : FactorPool -> FactorPool -> Random.Generator Challenge
challengeGen  factorPoolA  factorPoolB =
    Random.map2
        (\a b ->  getChallenge a b)
        (randomFactor factorPoolA)
        (randomFactor factorPoolB)

init : () -> (Model, Cmd Msg)
init _ =
    -- TODO niels 08.08.2020: Local-Storage must be added.
    -- https://elmprogramming.com/saving-app-state.html
    -- https://package.elm-lang.org/packages/billstclair/elm-localstorage/latest/
    let
        listA = range 2 15
        listB = range 10 15
    in
    ( Model (Config (FactorPool 1 listA "") (FactorPool 1 listB "") 20 False True) Maybe.Nothing 0 []
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
      , Random.generate NewChallenge (challengeGen model.config.poolA model.config.poolB)
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
      div [class "container-sm"]
        [ h1 [][text "1x1-Trainer"],
         showConfiguration model,
         p [][ text ((showMaybeChallenge model.currentChallenge) ++ " (" ++ String.fromInt model.remainingTime ++ ")") ]
        , button [ onClick StartChallenges ] [ text "Roll" ]
        ]

showConfiguration : Model -> Html Msg
showConfiguration model =
    if model.config.show then
       div [id "configuration"] [
          h2 [] [text "Konfiguration"],
          showFactorConfig model.config.poolA "A",
          showFactorConfig model.config.poolB "B",
          div [class "input-group", class "input-group-sm", class "mb-3"][
              input [type_ "number", class "form-control", name "timeout", Attributes.min "2",  Attributes.max "30", size 2,
                     attribute "aria-label" "Definiere den Timeout."] [],
              div [class "input-group-append"] [
                  span [class "input-group-text"][text " s Zeit "]
              ],
              div [class "input-group-text" ][
                  input [type_ "checkbox", attribute "aria-label" "Umgekehrt"] []
              ],
              div [class "input-group-append"][
                  span [class "input-group-text"][text " Umgekehrt"]
              ]
          ],
          hr[][]
       ]
    else
       text "No Config"

showFactorConfig: FactorPool -> String -> Html Msg
showFactorConfig config configName=
    div[id ("factor" ++ configName ++ "Config")][
        p[][
        strong [] [text ("Faktor " ++ configName ++ ": ")],
        text (String.join ", " (List.map String.fromInt (config.firstElement :: config.furtherElements)))
        ],
        div [class "input-group", class "input-group-sm", class "mb-3"] [
             input [type_ "text", class "form-control", size 10, placeholder "3-5, 8", Attributes.name ("range_" ++  configName),
                          attribute "aria-label" ("Faktoren für " ++ configName)][],
             div [class "input-group-append"] [
                button [type_ "button", class "btn", class "btn-success", name ("add_" ++  configName)][text "Hinzufügen"],
                button [type_ "button", class "btn", class "btn-warning", name ("remove_" ++  configName)][text "Entfernen"]
            ]
        ]
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
    (maybeIntToString challenge.faktorA) ++ " x " ++ (maybeIntToString challenge.faktorB) ++ " = " ++ (maybeIntToString challenge.result)

maybeIntToString: Maybe Int -> String
maybeIntToString maybeInt =
    Maybe.withDefault "" (Maybe.map String.fromInt maybeInt)