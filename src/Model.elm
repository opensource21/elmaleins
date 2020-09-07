module Model exposing
    ( Challenge
    , Config
    , FactorPool
    , Model
    , Msg(..)
    , addChanges
    , challengeResultCorrect
    , challengeResultWrong
    , count
    , removeChanges
    , setChanges
    , setConfig
    , setPoolA
    , setPoolB
    , setReverse
    , setTimeoutInSeconds
    )

import Array exposing (Array)
import Time



-- MODEL
-- Maybe because we want to allow each at input, which could be empty.


type Msg
    = StartChallenges
    | StopChallenges
    | NewChallenge Challenge
    | Tick Challenge Time.Posix
    | Solved Challenge
    | Result Challenge String
    | FaktorA Challenge String
    | FaktorB Challenge String
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


count : (a -> Bool) -> List a -> Int
count predicate challenges =
    List.length (List.filter predicate challenges)


challengeResultCorrect : Challenge -> Bool
challengeResultCorrect challenge =
    Maybe.withDefault 0 challenge.faktorA * Maybe.withDefault 0 challenge.faktorB == Maybe.withDefault -1 challenge.result


challengeResultWrong : Challenge -> Bool
challengeResultWrong challenge =
    not (challengeResultCorrect challenge)
