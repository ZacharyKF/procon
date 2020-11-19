module Card.CardList exposing (..)

import Array exposing (..)
import Card.Card as Card exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Json.Decode as D exposing (..)
import Json.Encode as E exposing (..)
import Styling exposing (..)
import WithId exposing (..)


type alias CardListModel =
    WithId (CardListData {})


type alias CardListData a =
    { a | cards : HasIdArray CardModel, text : String }


cardListDataCons : String -> HasIdArray CardModel -> CardListData a -> CardListData a
cardListDataCons label cards other =
    { other | cards = cards, text = label }


cons : Int -> String -> HasIdArray CardModel -> CardListModel
cons id label cards =
    (withIdCons id << cardListDataCons label cards) { id = 0, text = "", cards = Array.empty }


init : Int -> String -> CardListModel
init id label =
    cons id label Array.empty


cardListDecoder : Decoder CardListModel
cardListDecoder =
    D.map3 cons
        (field "id" D.int)
        (field "text" D.string)
        (field "cards" (D.array cardDecoder))


cardListEncoder : CardListModel -> E.Value
cardListEncoder model =
    E.object
        [ ( "cards", E.array cardEncoder model.cards )
        , ( "id", E.int model.id )
        , ( "text", E.string model.text )
        ]


type CardListMsg
    = ToCard Int CardMsg
    | ActOnCards WithIdAction


update : CardListModel -> CardListMsg -> ( CardListModel, Cmd CardListMsg )
update model msg =
    case msg of
        ActOnCards action ->
            let
                newArr =
                    updateHasIdArray model.cards action Card.init
            in
            ( { model | cards = newArr }, Cmd.none )

        ToCard id card_msg ->
            let
                ( newarr, nmsg ) =
                    updateId model.cards id card_msg Card.update ToCard Cmd.none
            in
            ( { model | cards = newarr }, nmsg )


view : CardListModel -> (Int -> CardListMsg -> msg) -> Html msg
view model lift =
    let
        get_card_view card =
            Html.Styled.map (lift model.id) (Card.view card lift_card_msg)

        liftCardAction act =
            lift model.id <| ActOnCards act
    in
    cardListBody
        []
        [ cardListTitle [] [ text model.text ]
        , cardListCardContainer [] <|
            (List.map get_card_view <| Array.toList model.cards)
                ++ [ getButton pbtn False Add liftCardAction ]
        ]


lift_card_msg : Int -> CardMsg -> CardListMsg
lift_card_msg id card_msg =
    case card_msg of
        CardAction action ->
            ActOnCards action

        _ ->
            ToCard id card_msg
