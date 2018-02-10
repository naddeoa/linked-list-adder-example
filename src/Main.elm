module Main exposing (..)

import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src)
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
    }


init : ( Model, Cmd Msg )
init =
    ( { l1 = [ 1, 8, 8 ], l2 = [ 9, 8, 7 ] }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


listToNumber : List Int -> String
listToNumber list =
    List.reverse list
        |> List.map toString
        |> String.join ""



---- VIEW ----


view : Model -> Html Msg
view { l1, l2 } =
    div []
        [ h1 [] [ text "adder" ]
        , div [ Html.Attributes.style [ ( "text-align", "right" ), ( "display", "flex" ), ( "flex-direction", "column" ), ( "align-items", "center" ) ] ]
            [ div [ Html.Attributes.style [ ( "min-width", "100px" ) ] ]
                [ Html.div [] [ Html.text <| toString l1 ]
                , Html.div [] [ Html.text <| toString l2 ]
                , Html.div [] [ Html.text <| toString <| addLists l1 l2 ]
                ]
            , div [ Html.Attributes.style [ ( "height", "40px" ) ] ] []
            , div [ Html.Attributes.style [ ( "min-width", "100px" ) ] ]
                [ Html.div [] [ Html.text <| listToNumber l1 ]
                , Html.div [] [ Html.text <| listToNumber l2 ]
                , Html.div [] [ Html.text <| listToNumber <| addLists l1 l2 ]
                ]
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
