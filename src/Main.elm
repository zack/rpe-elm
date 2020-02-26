module Main exposing (..)

import Browser
import Debug
import Html exposing (..)
import Html.Attributes exposing (class, classList, for, href, id, name, step, type_, value)
import Html.Events exposing (on, onInput)
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


type Rounding
    = Five
    | TwoPointFive
    | One
    | PointOhOne


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
    | UpdateRounding String



---- MODEL ----


type alias Model =
    { errors : List Error
    , estimated1RM : String
    , givenRPE : String
    , givenReps : String
    , givenWeight : String
    , rounding : Rounding
    , rpeTable : RPETable
    , targetRPE : String
    , targetReps : String
    , targetWeight : String
    }


initialErrors : List Error
initialErrors =
    [ Missing GivenWeight
    , Missing GivenReps
    , Missing GivenRPE
    , Missing TargetReps
    , Missing TargetRPE
    ]


initialModel : RPETable -> Model
initialModel rpeTable =
    { errors = initialErrors
    , estimated1RM = "..."
    , givenRPE = ""
    , givenReps = ""
    , givenWeight = ""
    , rounding = Five
    , rpeTable = rpeTable
    , targetRPE = ""
    , targetReps = ""
    , targetWeight = "..."
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
            if weight == "" then
                addErrorWithUniqueFieldToErrors (Missing field) cleanErrors

            else
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
            if reps == "" then
                addErrorWithUniqueFieldToErrors (Missing field) cleanErrors

            else
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
            if reps == "" then
                addErrorWithUniqueFieldToErrors (Missing field) cleanErrors

            else
                addErrorWithUniqueFieldToErrors (Bad field) cleanErrors

        Just int ->
            if int < 6 || int > 10 then
                addErrorWithUniqueFieldToErrors (Bad field) cleanErrors

            else
                cleanErrors


roundingFromString : String -> Rounding
roundingFromString roundingString =
    case roundingString of
        "5" ->
            Five

        "2.5" ->
            TwoPointFive

        "1" ->
            One

        _ ->
            PointOhOne


getRoundAsFloat : Rounding -> Float
getRoundAsFloat rounder =
    case rounder of
        Five ->
            5

        TwoPointFive ->
            2.5

        One ->
            1

        PointOhOne ->
            0.01


round_ : Rounding -> Float -> Float
round_ rounder val =
    let
        rounderFloat =
            getRoundAsFloat rounder
    in
    toFloat (round (val / rounderFloat)) * rounderFloat


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateRounding roundingString ->
            let
                rounding =
                    roundingFromString roundingString
            in
            ( { model
                | rounding = rounding
                , estimated1RM = getEstimated1RM { model | rounding = rounding }
                , targetWeight = getTargetWeight { model | rounding = rounding }
              }
            , Cmd.none
            )

        UpdateField GivenWeight weight ->
            ( { model
                | givenWeight = weight
                , estimated1RM = getEstimated1RM { model | givenWeight = weight }
                , targetWeight = getTargetWeight { model | givenWeight = weight }
                , errors = validateWeight model.errors weight GivenWeight
              }
            , Cmd.none
            )

        UpdateField GivenReps reps ->
            ( { model
                | givenReps = reps
                , estimated1RM = getEstimated1RM { model | givenReps = reps }
                , targetWeight = getTargetWeight { model | givenReps = reps }
                , errors = validateReps model.errors reps GivenReps
              }
            , Cmd.none
            )

        UpdateField GivenRPE rpe ->
            ( { model
                | givenRPE = rpe
                , estimated1RM = getEstimated1RM { model | givenRPE = rpe }
                , targetWeight = getTargetWeight { model | givenRPE = rpe }
                , errors = validateRPE model.errors rpe GivenRPE
              }
            , Cmd.none
            )

        UpdateField TargetReps reps ->
            ( { model
                | targetReps = reps
                , targetWeight = getTargetWeight { model | targetReps = reps }
                , errors = validateReps model.errors reps TargetReps
              }
            , Cmd.none
            )

        UpdateField TargetRPE rpe ->
            ( { model
                | targetRPE = rpe
                , targetWeight = getTargetWeight { model | targetRPE = rpe }
                , errors = validateRPE model.errors rpe TargetRPE
              }
            , Cmd.none
            )



---- VIEW ----


showTargetFloat : Float -> String
showTargetFloat number =
    if number == 0 then
        "..."

    else
        String.fromFloat number


getEstimated1RM : Model -> String
getEstimated1RM model =
    let
        givenRPEDecimal =
            getValueForRepCount
                (String.toInt model.givenReps)
                (getRepsForRPE (String.toInt model.givenRPE) model.rpeTable)

        floatWeight =
            String.toFloat model.givenWeight
    in
    case ( givenRPEDecimal, floatWeight, model.errors ) of
        ( Just rpe, Just weight, [] ) ->
            String.fromFloat (round_ model.rounding (weight / rpe))

        _ ->
            "..."


{-| Calculate our own e1rm here instead of passing it in because we want to make
sure we are always using an unrounded e1rm to calculate our target weight
-}
getTargetWeight : Model -> String
getTargetWeight model =
    let
        targetRPEDecimal =
            getValueForRepCount
                (String.toInt model.targetReps)
                (getRepsForRPE (String.toInt model.targetRPE) model.rpeTable)

        estimated1RM =
            String.toFloat
                (getEstimated1RM { model | rounding = PointOhOne })
    in
    case ( targetRPEDecimal, estimated1RM, model.errors ) of
        ( Just rpe, Just e1rm, [] ) ->
            String.fromFloat (round_ model.rounding (rpe * e1rm))

        _ ->
            "..."


getErrorText : List Error -> Field -> String
getErrorText errors field =
    List.foldr
        (\error memo ->
            case error of
                Bad f ->
                    if field == f then
                        if field == GivenWeight then
                            "Weight must be above 0"

                        else if field == GivenReps then
                            "Reps must be between 1 and 10"

                        else if field == GivenRPE then
                            "RPE must be between 6 and 10"

                        else if field == TargetReps then
                            "Reps must be between 1 and 10"

                        else if field == TargetRPE then
                            "RPE must be between 6 and 10"

                        else
                            memo

                    else
                        memo

                Missing _ ->
                    memo
        )
        ""
        errors


getErrorTextForBadField : Field -> String
getErrorTextForBadField field =
    case field of
        GivenWeight ->
            "Weight must be above 0"

        GivenReps ->
            "Reps must be between 1 and 10"

        GivenRPE ->
            "RPE must be between 6 and 10"

        TargetReps ->
            "Reps must be between 1 and 10"

        TargetRPE ->
            "RPE must be between 6 and 10"


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
            Just reps.reps6

        ( Just 7, Just reps ) ->
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
    let
        getErrorTextFunc =
            getErrorText model.errors

        givenWeightErrorText =
            getErrorTextFunc GivenWeight

        hasGivenWeightError =
            givenWeightErrorText /= ""

        givenRepsErrorText =
            getErrorTextFunc GivenReps

        hasGivenRepsError =
            givenRepsErrorText /= ""

        givenRPEErrorText =
            getErrorTextFunc GivenRPE

        hasGivenRPEError =
            givenRPEErrorText /= ""

        targetRepsErrorText =
            getErrorTextFunc TargetReps

        hasTargetRepsError =
            targetRepsErrorText /= ""

        targetRPEErrorText =
            getErrorTextFunc TargetRPE

        hasTargetRPEError =
            targetRPEErrorText /= ""
    in
    div [ id "content" ]
        [ div [ class "header" ]
            [ h1 []
                [ text "RPE Calculator " ]
            ]
        , div [ class "subheader" ]
            [ h3 []
                [ text "Starting Numbers " ]
            ]
        , div
            [ class "input-row given-weight"
            , classList
                [ ( "error"
                  , hasGivenWeightError
                  )
                ]
            ]
            [ div [ class "error" ] [ text givenWeightErrorText ]
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
        , div
            [ class "input-row given-reps"
            , classList
                [ ( "error"
                  , hasGivenRepsError
                  )
                ]
            ]
            [ div [ class "error" ] [ text givenRepsErrorText ]
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
        , div
            [ class "input-row given-rpe bottom-border"
            , classList
                [ ( "error"
                  , hasGivenRPEError
                  )
                ]
            ]
            [ div [ class "error" ] [ text givenRPEErrorText ]
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
        , div
            [ class "input-row desired-reps"
            , classList
                [ ( "error"
                  , hasTargetRepsError
                  )
                ]
            ]
            [ div [ class "error" ] [ text targetRepsErrorText ]
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
        , div
            [ class "input-row desired-rpe bottom-border"
            , classList
                [ ( "error"
                  , hasTargetRPEError
                  )
                ]
            ]
            [ div [ class "error" ] [ text targetRPEErrorText ]
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
                    [ text model.targetWeight ]
                ]
            , h3 []
                [ text "Estimated 1RM: "
                , span [ id "e1RM" ]
                    [ text model.estimated1RM ]
                ]
            ]
        , div [ class "options" ]
            [ label [ class "rounding", for "rounding" ]
                [ text "Rounding:\u{00A0}" ]
            , select [ class "rounding", id "rounding", name "rounding", onInput UpdateRounding ]
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
