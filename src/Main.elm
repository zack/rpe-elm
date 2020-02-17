module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, for, href, id, name, step, type_, value)
import Json.Decode exposing (Decoder, float, succeed)
import Json.Decode.Pipeline exposing (required)



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



---- MODEL ----


type alias Model =
    { rpeTable : RPETable
    , givenWeight : Float
    , givenReps : Int
    , givenRPE : Int
    , targetReps : Int
    , targetRPE : Int
    , targetWeight : Float
    , estimated1RM : Float
    }


initialModel : RPETable -> Model
initialModel rpeTable =
    { rpeTable = rpeTable
    , givenWeight = 0
    , givenReps = 0
    , givenRPE = 0
    , targetReps = 0
    , targetRPE = 0
    , targetWeight = 0
    , estimated1RM = 0
    }



---- UPDATE ----


type Msg
    = UpdateGivenWeight Float
    | UpdateGivenReps Int
    | UpdateGivenRPE Int
    | UpdateTargetReps Int
    | UpdateTargetRPE Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


showTargetFloat : Float -> String
showTargetFloat number =
    if number == 0 then
        "..."

    else
        String.fromFloat number


view : Model -> Html Msg
view model =
    div [ id "content" ]
        [ div [ class "header" ]
            [ h1 []
                [ text "RPE Calculator " ]
            ]
        , div [ class "subheader" ]
            [ h3 []
                [ text "Given Numbers " ]
            ]
        , div [ class "input-row given-weight" ]
            [ div [ class "error" ] []
            , div [ class "input-container" ]
                [ label [ for "given-weight" ]
                    [ text "Weight " ]
                , input
                    [ class "weight"
                    , id "given-weight"
                    , step "0.01"
                    , type_
                        "number"
                    , value (String.fromFloat model.givenWeight)
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
                    , type_ "number"
                    , value
                        (String.fromInt model.givenReps)
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
                    , type_ "number"
                    , value
                        (String.fromInt model.givenRPE)
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
                    , id
                        "desired-reps"
                    , type_ "number"
                    , value
                        (String.fromInt
                            model.targetReps
                        )
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
                    , id
                        "desired-rpe"
                    , type_ "number"
                    , value
                        (String.fromInt
                            model.targetRPE
                        )
                    ]
                    []
                ]
            ]
        , div [ class "results" ]
            [ h3 []
                [ text "Target weight: "
                , span [ id "target-weight" ]
                    [ text (showTargetFloat model.targetWeight) ]
                ]
            , h3 []
                [ text "Estimated 1RM: "
                , span [ id "e1RM" ]
                    [ text (showTargetFloat model.estimated1RM) ]
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
