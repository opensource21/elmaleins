module Json exposing (fromJson, toJson)

import Json.Decode as Decode exposing (Decoder, bool, field, int, map3, map5)
import Json.Encode as Encode
import Model exposing (Config, FactorPool)



-- Json


firstElement : Int -> ( String, Encode.Value )
firstElement value =
    ( "firstElement", Encode.int value )


furtherElements : List Int -> ( String, Encode.Value )
furtherElements values =
    ( "furtherElements", Encode.list Encode.int values )


toJson : Config -> Encode.Value
toJson config =
    Encode.object
        [ ( "poolA", factorPoolToJson config.poolA )
        , ( "poolB", factorPoolToJson config.poolB )
        , ( "timeoutInSeconds", Encode.int config.timeoutInSeconds )
        , ( "reverseChallenges", Encode.bool config.reverseChallenges )
        , ( "show", Encode.bool config.show )
        ]


factorPoolToJson : FactorPool -> Encode.Value
factorPoolToJson pool =
    Encode.object
        [ firstElement pool.firstElement
        , furtherElements pool.furtherElements
        , ( "changes", Encode.string pool.changes )
        ]


fromJson : Maybe String -> Maybe Config
fromJson configJsonMB =
    case configJsonMB of
        Just configJson ->
            case Decode.decodeString configDecoder configJson of
                Ok config ->
                    Just config

                Err _ ->
                    Nothing

        Nothing ->
            Nothing


configDecoder : Decoder Config
configDecoder =
    map5 Config
        (field "poolA" factorPoolDecoder)
        (field "poolB" factorPoolDecoder)
        timeoutInSecondsDecoder
        reverseChallengesDecoder
        showDecoder


factorPoolDecoder : Decoder FactorPool
factorPoolDecoder =
    map3 FactorPool
        (field "firstElement" Decode.int)
        (field "furtherElements" (Decode.list Decode.int))
        (field "changes" Decode.string)


showDecoder : Decoder Bool
showDecoder =
    field "show" bool


reverseChallengesDecoder : Decoder Bool
reverseChallengesDecoder =
    field "reverseChallenges" bool


timeoutInSecondsDecoder : Decoder Int
timeoutInSecondsDecoder =
    field "timeoutInSeconds" int
