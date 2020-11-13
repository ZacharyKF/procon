module Card.CardList exposing (..)

import Array exposing (Array)
import Card.Card as Card exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D exposing (..)
import Json.Encode as E exposing (..)


type alias CardListModel =
    { cards : Array CardModel
    , id : Int
    , text : String
    }


cardListDecoder : Decoder CardListModel
cardListDecoder =
    D.map3 CardListModel
        (field "cards" (D.array cardDecoder))
        (field "id" D.int)
        (field "text" D.string)


cardListEncoder : CardListModel -> E.Value
cardListEncoder model =
    E.object
        [ ( "cards", E.array cardEncoder model.cards )
        , ( "id", E.int model.id )
        , ( "text", E.string model.text )
        ]


init : Int -> String -> CardListModel
init id label =
    { cards = Array.empty
    , id = id
    , text = label
    }


type CardListMsg
    = ToCard Int CardMsg
    | MoveCardUp Int
    | MoveCardDown Int
    | DeleteCard Int
    | AddCard


update : CardListModel -> CardListMsg -> ( CardListModel, Cmd CardListMsg )
update model msg =
    case msg of
        ToCard id card_msg ->
            let
                oldcard =
                    Array.get id model.cards
            in
            case oldcard of
                Nothing ->
                    ( model, Cmd.none )

                Just acard ->
                    let
                        ( newcard, sub_cmd ) =
                            Card.update acard card_msg
                    in
                    ( { model | cards = Array.set id newcard model.cards }, Cmd.map (ToCard id) sub_cmd )

        MoveCardUp id ->
            let
                ( carda, cardb ) =
                    ( Array.get id model.cards, Array.get (id - 1) model.cards )
            in
            case ( carda, cardb ) of
                ( Just acard, Just bcard ) ->
                    ( { model
                        | cards =
                            Array.set (id - 1) (shift_card_up acard) <|
                                Array.set id (shift_card_down bcard) model.cards
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        MoveCardDown id ->
            let
                ( carda, cardb ) =
                    ( Array.get id model.cards, Array.get (id + 1) model.cards )
            in
            case ( carda, cardb ) of
                ( Just acard, Just bcard ) ->
                    ( { model
                        | cards =
                            Array.set (id + 1) (shift_card_down acard) <|
                                Array.set id (shift_card_up bcard) model.cards
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        DeleteCard id ->
            let
                remove_ind index arr =
                    Array.fromList <|
                        List.filterMap (ind_transform index) (Array.toList arr)
            in
            ( { model | cards = remove_ind id model.cards }, Cmd.none )

        AddCard ->
            let
                newcard =
                    Card.init (Array.length model.cards)
            in
            ( { model | cards = Array.push newcard model.cards }, Cmd.none )


shift_card_up : CardModel -> CardModel
shift_card_up card =
    { card | id = card.id - 1 }


shift_card_down : CardModel -> CardModel
shift_card_down card =
    { card | id = card.id + 1 }


ind_transform : Int -> CardModel -> Maybe CardModel
ind_transform id model =
    if model.id == id then
        Nothing

    else if model.id < id then
        Just model

    else
        Just { model | id = model.id - 1 }


view : CardListModel -> (Int -> CardListMsg -> msg) -> Html msg
view model lift =
    let
        get_card_view card =
            Html.map (lift model.id) (Card.view card lift_card_msg)
    in
    div
        [ style "flex" "1"
        , style "outline" "solid"
        , style "flex-direction" "column"
        , style "display" "flex"
        , style "margin" "0.25em"
        , style "padding" "0.25em"
        , style "justify-content" "flex-start"
        ]
        [ div
            [ style "height" "1.5em"
            , style "justify-content" "center"
            ]
            [ text model.text
            ]
        , button
            [ onClick <| lift model.id AddCard, style "margin" "0.1em" ]
            [ text "Add Card" ]
        , div
            [ style "display" "flex"
            , style "flex-direction" "column"
            ]
          <|
            List.map get_card_view <|
                Array.toList model.cards
        ]


lift_card_msg : Int -> CardMsg -> CardListMsg
lift_card_msg id card_msg =
    case card_msg of
        MoveUp ->
            MoveCardUp id

        MoveDown ->
            MoveCardDown id

        Delete ->
            DeleteCard id

        _ ->
            ToCard id card_msg
