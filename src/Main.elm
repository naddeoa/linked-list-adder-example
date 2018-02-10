module Main exposing (..)

import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src)
import Html.Events
import List exposing (all, isEmpty, head, tail)
import Maybe exposing (withDefault)


calculateCarry : Int -> ( Int, Int )
calculateCarry n =
    if n > 9 then
        ( 1, n % 10 )
    else
        ( 0, n )


addLists : List Int -> List Int -> List Int
addLists =
    addListsRec 0 []


headOrZero : List Int -> Int
headOrZero =
    List.head >> withDefault 0


tailOrEmpty : List Int -> List Int
tailOrEmpty =
    List.tail >> withDefault []


addListsRec : Int -> List Int -> List Int -> List Int -> List Int
addListsRec carry sum l1 l2 =
    if all isEmpty [ l1, l2 ] && carry == 0 then
        sum
    else
        let
            ( newCarry, newN ) =
                calculateCarry <| (headOrZero l1) + (headOrZero l2) + carry
        in
            addListsRec newCarry (sum ++ [ newN ]) (tailOrEmpty l1) (tailOrEmpty l2)



---- MODEL ----


type alias Model =
    { l1 : List Int
    , l2 : List Int
    , n1 : Int
    , n2 : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { l1 = [ 1, 8, 8 ], l2 = [ 9, 8, 7 ], n1 = 881, n2 = 789 }, Cmd.none )



---- UPDATE ----


type Msg
    = N1Changed String
    | N2Changed String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        N1Changed n ->
            case String.toInt n of
                Ok result ->
                    ( { model | n1 = result }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        N2Changed n ->
            case String.toInt n of
                Ok result ->
                    ( { model | n2 = result }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


listToNumber : List Int -> String
listToNumber list =
    List.reverse list
        |> List.map toString
        |> String.join ""


numberToList : Int -> List Int
numberToList i =
    i
        |> toString
        |> String.split ""
        |> List.reverse
        |> List.map String.toInt
        |> List.map (Result.withDefault 0)



---- VIEW ----


viewAddition : List Int -> List Int -> Html Msg
viewAddition l1 l2 =
    div []
        [ div [ Html.Attributes.style [ ( "min-width", "100px" ) ] ]
            [ Html.div [] [ Html.text <| toString l1 ]
            , Html.div [] [ Html.text <| toString l2 ]
            , Html.div [] [ Html.text <| toString <| addLists l1 l2 ]
            ]
        , div [ Html.Attributes.style [ ( "height", "40px" ) ] ] []
        , div [ Html.Attributes.style [ ( "min-width", "100px" ) ] ]
            [ Html.div [] [ Html.text <| listToNumber l1 ]
            , Html.div [ Html.Attributes.style [ ( "display", "flex" ) ] ]
                [ Html.span [] [ Html.text "+" ]
                , Html.span [ Html.Attributes.style [ ( "flex", "1" ) ] ] [ Html.text <| listToNumber l2 ]
                ]
            , Html.hr [] []
            , Html.div [ Html.Attributes.style [ ( "font-weight", "bold" ) ] ] [ Html.text <| listToNumber <| addLists l1 l2 ]
            ]
        ]


view : Model -> Html Msg
view { l1, l2, n1, n2 } =
    div []
        [ Html.h3 [] [ text "Example " ]
        , div [ Html.Attributes.style [ ( "text-align", "right" ), ( "display", "flex" ), ( "flex-direction", "column" ), ( "align-items", "center" ) ] ]
            [ Html.h3 [ Html.Attributes.style [ ( "height", "40px" ) ] ] [ Html.text "Update numbers below" ]
            , div []
                [ Html.div [] [ Html.label [] [ Html.text "n1 " ], Html.input [ Html.Events.onInput N1Changed, Html.Attributes.type_ "number", Html.Attributes.value <| toString n1 ] [] ]
                , Html.div [] [ Html.label [] [ Html.text "n2 " ], Html.input [ Html.Events.onInput N2Changed, Html.Attributes.type_ "number", Html.Attributes.value <| toString n2 ] [] ]
                ]
            , Html.h3 [ Html.Attributes.style [ ( "height", "40px" ) ] ] [ Html.text "Result" ]
            , viewAddition (numberToList n1) (numberToList n2)
            ]
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
