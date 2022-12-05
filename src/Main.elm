module Main exposing (..)

import Browser
-- import Debug
import Html exposing (..)
import Html.Attributes
    exposing
        ( class
        , classList
        , for
        , href
        , id
        , maxlength
        , name
        , step
        , type_
        , value
        )
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
    | E1RMMultiplier


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
    , e1RMMultiplier : String
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
    , e1RMMultiplier = "100"
    , givenRPE = ""
    , givenReps = ""
    , givenWeight = ""
    , rounding = TwoPointFive
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
validateRPE errors rpe field =
    let
        parsedFloat =
            String.toFloat rpe

        cleanErrors =
            filterAllFieldFromErrors field errors
    in
    case parsedFloat of
        Nothing ->
            if rpe == "" then
                addErrorWithUniqueFieldToErrors (Missing field) cleanErrors

            else
                addErrorWithUniqueFieldToErrors (Bad field) cleanErrors

        Just float ->
            if float < 6 || float > 10 then
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


round_ : Rounding -> String -> String
round_ rounder val =
    let
        rounderFloat =
            getRoundAsFloat rounder

        floatVal =
            String.toFloat val
    in
    case floatVal of
        Just v ->
            String.fromFloat (toFloat (round (v / rounderFloat)) * rounderFloat)

        _ ->
            "..."


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateRounding roundingString ->
            ( { model | rounding = roundingFromString roundingString }, Cmd.none )

        UpdateField GivenWeight weight ->
            let
                errors =
                    validateWeight model.errors weight GivenWeight

                newModel =
                    { model | givenWeight = weight, errors = errors }

                estimated1RM =
                    getEstimated1RM newModel

                targetWeight =
                    getTargetWeight { newModel | estimated1RM = estimated1RM }
            in
            ( { model
                | givenWeight = weight
                , estimated1RM = estimated1RM
                , targetWeight = targetWeight
                , errors = errors
              }
            , Cmd.none
            )

        UpdateField GivenReps reps ->
            let
                errors =
                    validateReps model.errors reps GivenReps

                newModel =
                    { model | givenReps = reps, errors = errors }

                estimated1RM =
                    getEstimated1RM newModel

                targetWeight =
                    getTargetWeight { newModel | estimated1RM = estimated1RM }

            in
            ( { model
                | givenReps = reps
                , estimated1RM = estimated1RM
                , targetWeight = targetWeight
                , errors = errors
              }
            , Cmd.none
            )

        UpdateField GivenRPE rpe ->
            let
                errors =
                    validateRPE model.errors rpe GivenRPE

                newModel =
                    { model | givenRPE = rpe, errors = errors }

                estimated1RM =
                    getEstimated1RM newModel

                targetWeight =
                    getTargetWeight { newModel | estimated1RM = estimated1RM }
            in
            ( { model
                | givenRPE = rpe
                , estimated1RM = estimated1RM
                , targetWeight = targetWeight
                , errors = errors
              }
            , Cmd.none
            )

        UpdateField TargetReps reps ->
            let
                errors =
                    validateReps model.errors reps TargetReps

                newModel =
                    { model | targetReps = reps, errors = errors }
            in
            ( { model
                | targetReps = reps
                , targetWeight = getTargetWeight newModel
                , errors = errors
              }
            , Cmd.none
            )

        UpdateField TargetRPE rpe ->
            let
                errors =
                    validateRPE model.errors rpe TargetRPE

                newModel =
                    { model | targetRPE = rpe, errors = errors }
            in
            ( { model
                | targetRPE = rpe
                , targetWeight = getTargetWeight newModel
                , errors = errors
              }
            , Cmd.none
            )

        UpdateField E1RMMultiplier multiplier ->
            ( { model | e1RMMultiplier = multiplier }, Cmd.none )



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
        givenRPE = String.toFloat model.givenRPE
        givenRPEFloor =  Maybe.map floor givenRPE
        givenRPECeiling =  Maybe.map ceiling givenRPE

        givenRPEFloorDecimal =
            getValueForRepCount
                (String.toInt model.givenReps)
                (getRepsForRPE givenRPEFloor model.rpeTable)

        givenRPECeilingDecimal =
            getValueForRepCount
                (String.toInt model.givenReps)
                (getRepsForRPE givenRPECeiling model.rpeTable)

        givenRPEDecimal =
            case ( givenRPE ) of
                (Just grpe) ->
                    case ( givenRPECeilingDecimal, givenRPEFloorDecimal, givenRPEFloor ) of
                        ( Just ceilDec, Just floorDec, Just floor) ->
                            Just (floorDec + (( ceilDec - floorDec) * (grpe - (toFloat floor))))
                        _ ->
                            Nothing
                _ ->
                    Nothing

        floatWeight =
            String.toFloat model.givenWeight

        cannotCalculate =
            errorsPreventE1RM model.errors

    in
    case ( givenRPEDecimal, floatWeight, cannotCalculate ) of
        ( Just rpe, Just weight, False ) ->
            String.fromFloat (weight / rpe)

        _ ->
            "..."


errorsPreventE1RM : List Error -> Bool
errorsPreventE1RM errors =
    let
        foldFunc =
            \e memo ->
                case e of
                    Missing field ->
                        case field of
                            GivenWeight ->
                                True

                            GivenReps ->
                                True

                            GivenRPE ->
                                True

                            _ ->
                                memo

                    Bad field ->
                        case field of
                            GivenWeight ->
                                True

                            GivenReps ->
                                True

                            GivenRPE ->
                                True

                            _ ->
                                memo
    in
    List.foldr foldFunc False errors


getTargetWeight : Model -> String
getTargetWeight model =
    let
        targetRPE = String.toFloat model.targetRPE
        targetRPEFloor =  Maybe.map floor targetRPE
        targetRPECeiling =  Maybe.map ceiling targetRPE

        targetRPEFloorDecimal =
            getValueForRepCount
                (String.toInt model.targetReps)
                (getRepsForRPE targetRPEFloor model.rpeTable)

        targetRPECeilingDecimal =
            getValueForRepCount
                (String.toInt model.targetReps)
                (getRepsForRPE targetRPECeiling model.rpeTable)

        targetRPEDecimal =
            case ( targetRPE ) of
                (Just trpe) ->
                    case ( targetRPECeilingDecimal, targetRPEFloorDecimal, targetRPEFloor ) of
                        ( Just ceilDec, Just floorDec, Just floor) ->
                            Just (floorDec + (( ceilDec - floorDec) * (trpe - (toFloat floor))))
                        _ ->
                            Nothing
                _ ->
                    Nothing

        estimated1RM =
            String.toFloat model.estimated1RM

    in
    case ( targetRPEDecimal, estimated1RM, model.errors ) of
        ( Just rpe, Just e1rm, [] ) ->
            String.fromFloat (rpe * e1rm)

        _ ->
            "..."


getE1RMMultiplied : String -> String -> String
getE1RMMultiplied e1rm multiplier =
    let
        e2 =
            String.toFloat e1rm

        m2 =
            String.toFloat multiplier
    in
    case ( e2, m2 ) of
        ( Just e, Just m ) ->
            String.fromFloat (e * m / 100)

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

        E1RMMultiplier ->
            ""


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
                    [ text (round_ model.rounding model.targetWeight) ]
                ]
            , h3 []
                [ text "E1RM: "
                , span [ id "e1RM" ]
                    [ text (round_ model.rounding model.estimated1RM) ]
                , span [] [ text " × " ]
                , input
                    [ class "e1rm-multiplier"
                    , maxlength 3
                    , value model.e1RMMultiplier
                    , onInput (UpdateField E1RMMultiplier)
                    ]
                    []
                , span [] [ text "% = " ]
                , span []
                    [ text
                        (round_ model.rounding
                            (getE1RMMultiplied
                                model.estimated1RM
                                model.e1RMMultiplier
                            )
                        )
                    ]
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
                [ text "© 2021 Zack Youngren" ]
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
