module Main exposing (..)

import Browser
import Debug
import Html exposing (..)
import Html.Attributes exposing (class, for, href, id, name, step, type_, value)
import Html.Events exposing (onInput)
import Json.Decode exposing (Decoder, float, succeed)
import Json.Decode.Pipeline exposing (required)
import List



---- Initial RPE values decoder code ----


decodeRPETable : Decoder RPETable
decodeRPETable =
    succeed RPETable
        |> required "rpe6" decodeReps
        |> required "rpe7" decodeReps
        |> required "rpe8" decodeReps
        |> required "rpe9" decodeReps
        |> required "rpe10" decodeReps


decodeReps : Decoder RPEReps
decodeReps =
    succeed RPEReps
        |> required "reps1" float
        |> required "reps2" float
        |> required "reps3" float
        |> required "reps4" float
        |> required "reps5" float
        |> required "reps6" float
        |> required "reps7" float
        |> required "reps8" float
        |> required "reps9" float
        |> required "reps10" float
        |> required "reps11" float
        |> required "reps12" float



---- Custom Types and Aliases ----


type alias RPETable =
    { rpe6 : RPEReps
    , rpe7 : RPEReps
    , rpe8 : RPEReps
    , rpe9 : RPEReps
    , rpe10 : RPEReps
    }


type alias RPEReps =
    { reps1 : Float
    , reps2 : Float
    , reps3 : Float
    , reps4 : Float
    , reps5 : Float
    , reps6 : Float
    , reps7 : Float
    , reps8 : Float
    , reps9 : Float
    , reps10 : Float
    , reps11 : Float
    , reps12 : Float
    }


type Field
    = GivenWeight
    | GivenReps
    | GivenRPE
    | TargetReps
    | TargetRPE


type Error
    = Missing Field
    | Bad Field


type Msg
    = UpdateField Field String



---- MODEL ----


type alias Model =
    { rpeTable : RPETable
    , givenWeight : String
    , givenReps : String
    , givenRPE : String
    , targetReps : String
    , targetRPE : String
    , targetWeight : String
    , estimated1RM : String
    , errors : List Error
    }


initialModel : RPETable -> Model
initialModel rpeTable =
    { rpeTable = rpeTable
    , givenWeight = ""
    , givenReps = ""
    , givenRPE = ""
    , targetReps = ""
    , targetRPE = ""
    , targetWeight = ""
    , estimated1RM = ""
    , errors =
        [ Missing GivenWeight
        , Missing GivenReps
        , Missing GivenRPE
        , Missing TargetReps
        , Missing TargetRPE
        ]
    }



---- UPDATE ----


addUniqueItemToList : a -> List a -> List a
addUniqueItemToList item list =
    item :: filterAllItemFromList item list


filterAllItemFromList : a -> List a -> List a
filterAllItemFromList item list =
    List.filter (\i -> i /= item) list


getNewIntErrors : List Error -> String -> Field -> List Error
getNewIntErrors errors stringInt fieldType =
    let
        parsedInt =
            String.toInt stringInt

        cleanErrors =
            filterAllItemFromList (Missing fieldType) errors
    in
    if parsedInt == Nothing then
        addUniqueItemToList (Bad fieldType) cleanErrors

    else
        filterAllItemFromList (Bad fieldType) cleanErrors


getNewFloatErrors : List Error -> String -> Field -> List Error
getNewFloatErrors errors stringFloat fieldType =
    let
        parsedFloat =
            String.toInt stringFloat

        cleanErrors =
            filterAllItemFromList (Missing fieldType) errors
    in
    if parsedFloat == Nothing then
        addUniqueItemToList (Bad fieldType) cleanErrors

    else
        filterAllItemFromList (Bad fieldType) cleanErrors


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateField field value ->
            let
                newErrors =
                    case field of
                        GivenWeight ->
                            getNewIntErrors model.errors value field

                        _ ->
                            getNewIntErrors model.errors value field
            in
            case field of
                GivenWeight ->
                    ( { model | givenWeight = value, errors = newErrors }, Cmd.none )

                GivenReps ->
                    ( { model | givenReps = value, errors = newErrors }, Cmd.none )

                GivenRPE ->
                    ( { model | givenRPE = value, errors = newErrors }, Cmd.none )

                TargetReps ->
                    ( { model | targetReps = value, errors = newErrors }, Cmd.none )

                TargetRPE ->
                    ( { model | targetRPE = value, errors = newErrors }, Cmd.none )



---- VIEW ----


showTargetFloat : Float -> String
showTargetFloat number =
    if number == 0 then
        "..."

    else
        String.fromFloat number


getTargetWeight : List Error -> String -> String
getTargetWeight errors weight =
    if List.length errors > 0 then
        "..."

    else
        "number"


getEstimated1RM : List Error -> String -> String
getEstimated1RM errors e1rm =
    if List.length errors > 0 then
        "..."

    else
        "number"


view : Model -> Html Msg
view model =
    div [ id "content" ]
        [ div [ class "header" ]
            [ h1 []
                [ text "RPE Calculator " ]
            ]
        , div [ class "subheader" ]
            [ h3 []
                [ text "Starting Numbers " ]
            ]
        , div [ class "input-row given-weight" ]
            [ div [ class "error" ] []
            , div [ class "input-container" ]
                [ label [ for "given-weight" ]
                    [ text "Weight " ]
                , input
                    [ class "weight"
                    , id "given-weight"
                    , onInput (UpdateField GivenWeight)
                    , type_ "number"
                    , value model.givenWeight
                    ]
                    []
                ]
            ]
        , div [ class "input-row given-reps" ]
            [ div [ class "error" ] []
            , div [ class "input-container" ]
                [ label [ for "given-reps" ]
                    [ text "Reps " ]
                , input
                    [ class "reps"
                    , id "given-reps"
                    , onInput (UpdateField GivenReps)
                    , type_ "number"
                    , value model.givenReps
                    ]
                    []
                ]
            ]
        , div [ class "input-row given-rpe bottom-border" ]
            [ div [ class "error" ] []
            , div [ class "input-container" ]
                [ label [ for "given-rpe" ]
                    [ text "RPE " ]
                , input
                    [ class "rpe"
                    , id "given-rpe"
                    , onInput (UpdateField GivenRPE)
                    , type_ "number"
                    , value model.givenRPE
                    ]
                    []
                ]
            ]
        , div [ class "subheader" ]
            [ h3 []
                [ text "Target Numbers " ]
            ]
        , div [ class "input-row desired-reps" ]
            [ div [ class "error" ] []
            , div [ class "input-container" ]
                [ label [ for "desired-reps" ]
                    [ text "Reps " ]
                , input
                    [ class "reps"
                    , id "desired-reps"
                    , onInput (UpdateField TargetReps)
                    , type_ "number"
                    , value model.targetReps
                    ]
                    []
                ]
            ]
        , div [ class "input-row desired-rpe bottom-border" ]
            [ div [ class "error" ] []
            , div [ class "input-container" ]
                [ label [ for "desired-rpe" ]
                    [ text "RPE " ]
                , input
                    [ class "rpe"
                    , id "desired-rpe"
                    , type_ "number"
                    , value model.targetRPE
                    , onInput (UpdateField TargetRPE)
                    ]
                    []
                ]
            ]
        , div [ class "results" ]
            [ h3 []
                [ text "Target weight: "
                , span [ id "target-weight" ]
                    [ text (getTargetWeight model.errors model.targetWeight) ]
                ]
            , h3 []
                [ text "Estimated 1RM: "
                , span [ id "e1RM" ]
                    [ text (getEstimated1RM model.errors model.estimated1RM) ]
                ]
            ]
        , div [ class "options" ]
            [ label [ class "rounding", for "rounding" ]
                [ text "Rounding: " ]
            , select [ class "rounding", id "rounding", name "rounding" ]
                [ option [ value "5" ]
                    [ text "5.0" ]
                , option [ value "2.5" ]
                    [ text "2.5" ]
                , option [ value "1" ]
                    [ text "1.0" ]
                , option [ value "0.01" ]
                    [ text "0.01" ]
                ]
            ]
        , div [ class "footer" ]
            [ span []
                [ text "© 2020 Zack Youngren" ]
            , br [] []
            , text "Code on "
            , a [ href "https://www.github.com/zack/rpe-elm" ]
                [ text "GitHub" ]
            ]
        ]



---- PROGRAM ----


main : Program RPETable Model Msg
main =
    Browser.element
        { view = view
        , init = \rpeTable -> ( initialModel rpeTable, Cmd.none )
        , update = update
        , subscriptions = always Sub.none
        }
