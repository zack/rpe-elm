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


getFieldFromError : Error -> Field
getFieldFromError error =
    case error of
        Missing field ->
            field

        Bad field ->
            field


addErrorWithUniqueFieldToErrors : Error -> List Error -> List Error
addErrorWithUniqueFieldToErrors error errors =
    case error of
        Missing field ->
            error :: filterAllFieldFromErrors field errors

        Bad field ->
            error :: filterAllFieldFromErrors field errors


filterAllFieldFromErrors : Field -> List Error -> List Error
filterAllFieldFromErrors field list =
    List.filter (\err -> getFieldFromError err /= field) list



-- getNewIntErrors : List Error -> String -> Field -> List Error
-- getNewIntErrors errors stringInt field =
--     let
--         parsedInt =
--             String.toInt stringInt
--
--         cleanErrors =
--             filterAllFieldFromErrors field errors
--     in
--     if parsedInt == Nothing then
--         addErrorWithUniqueFieldToErrors (Bad field) cleanErrors
--
--     else
--         cleanErrors
-- getNewFloatErrors : List Error -> String -> Field -> List Error
-- getNewFloatErrors errors stringFloat field =
--     let
--         parsedFloat =
--             String.toFloat stringFloat
--
--         cleanErrors =
--             filterAllFieldFromErrors field errors
--     in
--     if parsedFloat == Nothing then
--         addErrorWithUniqueFieldToErrors (Bad field) cleanErrors
--
--     else
--         filterAllFieldFromErrors field cleanErrors


validateWeight : List Error -> String -> Field -> List Error
validateWeight errors weight field =
    let
        parsedFloat =
            String.toFloat weight

        cleanErrors =
            filterAllFieldFromErrors field errors
    in
    case parsedFloat of
        Nothing ->
            addErrorWithUniqueFieldToErrors (Bad field) cleanErrors

        Just float ->
            if float <= 0 then
                addErrorWithUniqueFieldToErrors (Bad field) cleanErrors

            else
                cleanErrors


validateReps : List Error -> String -> Field -> List Error
validateReps errors reps field =
    let
        parsedInt =
            String.toInt reps

        cleanErrors =
            filterAllFieldFromErrors field errors
    in
    case parsedInt of
        Nothing ->
            addErrorWithUniqueFieldToErrors (Bad field) cleanErrors

        Just int ->
            if int < 1 || int > 12 then
                addErrorWithUniqueFieldToErrors (Bad field) cleanErrors

            else
                cleanErrors


validateRPE : List Error -> String -> Field -> List Error
validateRPE errors reps field =
    let
        parsedInt =
            String.toInt reps

        cleanErrors =
            filterAllFieldFromErrors field errors
    in
    case parsedInt of
        Nothing ->
            addErrorWithUniqueFieldToErrors (Bad field) cleanErrors

        Just int ->
            if int < 6 || int > 10 then
                addErrorWithUniqueFieldToErrors (Bad field) cleanErrors

            else
                cleanErrors


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateField GivenWeight weight ->
            let
                newErrors =
                    validateWeight model.errors weight GivenWeight

                new1rm =
                    getEstimated1RM model.givenReps weight model.givenRPE model.rpeTable
            in
            ( { model | givenWeight = weight, estimated1RM = new1rm, errors = newErrors }, Cmd.none )

        UpdateField GivenReps reps ->
            let
                newErrors =
                    validateReps model.errors reps GivenReps

                new1rm =
                    getEstimated1RM reps model.givenWeight model.givenRPE model.rpeTable
            in
            ( { model | givenReps = reps, estimated1RM = new1rm, errors = newErrors }, Cmd.none )

        UpdateField GivenRPE rpe ->
            let
                newErrors =
                    validateRPE model.errors rpe GivenRPE

                new1rm =
                    getEstimated1RM model.givenReps model.givenWeight rpe model.rpeTable
            in
            ( { model | givenRPE = rpe, estimated1RM = new1rm, errors = newErrors }, Cmd.none )

        UpdateField TargetReps value ->
            let
                newErrors =
                    validateReps model.errors value TargetReps
            in
            ( { model | targetReps = value, errors = newErrors }, Cmd.none )

        UpdateField TargetRPE value ->
            let
                newErrors =
                    validateRPE model.errors value TargetRPE
            in
            ( { model | targetRPE = value, errors = newErrors }, Cmd.none )



---- VIEW ----


showTargetFloat : Float -> String
showTargetFloat number =
    if number == 0 then
        "..."

    else
        String.fromFloat number


getTargetWeight : Model -> String
getTargetWeight model =
    if List.length model.errors > 0 then
        "..."

    else
        "number"


getEstimated1RM : String -> String -> String -> RPETable -> String
getEstimated1RM givenReps givenWeight givenRPE rpeTable =
    let
        givenRPEDecimal =
            getValueForRepCount
                (String.toInt givenReps)
                (getRepsForRPE (String.toInt givenRPE) rpeTable)

        floatWeight =
            String.toFloat givenWeight
    in
    case ( givenRPEDecimal, floatWeight ) of
        ( Just rpe, Just weight ) ->
            String.fromFloat (rpe * weight)

        _ ->
            "..."


getRepsForRPE : Maybe Int -> RPETable -> Maybe RPEReps
getRepsForRPE rpe rpeTable =
    case rpe of
        Just 6 ->
            Just rpeTable.rpe6

        Just 7 ->
            Just rpeTable.rpe7

        Just 8 ->
            Just rpeTable.rpe8

        Just 9 ->
            Just rpeTable.rpe9

        Just 10 ->
            Just rpeTable.rpe10

        _ ->
            Nothing


getValueForRepCount : Maybe Int -> Maybe RPEReps -> Maybe Float
getValueForRepCount repCount rpeReps =
    case ( repCount, rpeReps ) of
        ( Just 1, Just reps ) ->
            Just reps.reps1

        ( Just 2, Just reps ) ->
            Just reps.reps2

        ( Just 3, Just reps ) ->
            Just reps.reps3

        ( Just 4, Just reps ) ->
            Just reps.reps4

        ( Just 5, Just reps ) ->
            Just reps.reps5

        ( Just 6, Just reps ) ->
            Just reps.reps7

        ( Just 8, Just reps ) ->
            Just reps.reps8

        ( Just 9, Just reps ) ->
            Just reps.reps9

        ( Just 10, Just reps ) ->
            Just reps.reps10

        ( Just 11, Just reps ) ->
            Just reps.reps11

        ( Just 12, Just reps ) ->
            Just reps.reps12

        _ ->
            Nothing


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
                    [ text (getTargetWeight model) ]
                ]
            , h3 []
                [ text "Estimated 1RM: "
                , span [ id "e1RM" ]
                    [ text model.estimated1RM ]
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
