module Main exposing (..)

import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src)


tail : List a -> List a
tail l =
    Maybe.withDefault [] <| List.tail l


addLists : List Int -> List Int -> List Int
addLists =
    addListsRec 0 []


addListsRec : Int -> List Int -> List Int -> List Int -> List Int
addListsRec carry sum l1 l2 =
    let
        head1 =
            List.head l1

        head2 =
            List.head l2

        combine n1 n2 =
            let
                added =
                    carry + n1 + n2

                newCarry =
                    if added > 9 then
                        1
                    else
                        0
            in
                addListsRec newCarry (List.append sum [ added % 10 ]) (tail l1) (tail l2)
    in
        case head1 of
            Nothing ->
                case head2 of
                    Nothing ->
                        if carry == 0 then
                            sum
                        else
                            combine 0 0

                    Just n2 ->
                        combine 0 n2

            Just n1 ->
                case head2 of
                    Nothing ->
                        combine n1 0

                    Just n2 ->
                        combine n1 n2



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
