module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (attribute, class, for, href, id, name, step, type_, value)
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
    { rpeTable : RPETable }


initialModel : RPETable -> Model
initialModel rpeTable =
    { rpeTable = rpeTable }



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ id "content" ]
        [ div [ class "header" ]
            [ h1 []
                [ text "RPE Calculator " ]
            ]
        , div [ class "subheader" ]
            [ h3 []
                [ text "Basis Numbers " ]
            ]
        , div [ class "input-row basis-weight" ]
            [ div [ class "error" ] []
            , div [ class "input-container" ]
                [ label [ for "basis-weight" ]
                    [ text "Weight " ]
                , input [ class "weight", id "basis-weight", step "0.01", type_ "number" ] []
                ]
            ]
        , div [ class "input-row basis-reps" ]
            [ div [ class "error" ] []
            , div [ class "input-container" ]
                [ label [ for "basis-reps" ]
                    [ text "Reps " ]
                , input [ class "reps", id "basis-reps", type_ "number" ] []
                ]
            ]
        , div [ class "input-row basis-rpe bottom-border" ]
            [ div [ class "error" ] []
            , div [ class "input-container" ]
                [ label [ for "basis-rpe" ]
                    [ text "RPE " ]
                , input [ class "rpe", id "basis-rpe", type_ "number" ] []
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
                , input [ class "reps", attribute "disabled" "", id "desired-reps", type_ "number" ] []
                ]
            ]
        , div [ class "input-row desired-rpe bottom-border" ]
            [ div [ class "error" ] []
            , div [ class "input-container" ]
                [ label [ for "desired-rpe" ]
                    [ text "RPE " ]
                , input [ class "rpe", attribute "disabled" "", id "desired-rpe", type_ "number" ] []
                ]
            ]
        , div [ class "results" ]
            [ h3 []
                [ text "Target weight: "
                , span [ id "solved-weight" ]
                    [ text "... " ]
                ]
            , h3 []
                [ text "Estimated 1RM: "
                , span [ id "e1RM" ]
                    [ text "... " ]
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
                [ text "© 2018 Zack Youngren" ]
            , text "Code on "
            , a [ href "https://www.github.com/zack/rpe" ]
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
