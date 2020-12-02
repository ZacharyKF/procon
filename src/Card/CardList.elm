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


cons : Int -> String -> HasIdArray CardModel -> CardListModel
cons id label cards =
    { id = id, text = label, cards = cards }


init : Int -> String -> CardListModel
init id label =
    cons id label Array.empty


cardListDecoder : Decoder CardListModel
cardListDecoder =
    D.map3 cons
        (field "id" D.int)
        (field "text" D.string)
        (D.array cardDecoder |> field "cards")


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
    | ConfirmFirst String CardListMsg


update : CardListModel -> CardListMsg -> ( CardListModel, Cmd CardListMsg )
update model msg =
    case msg of
        ActOnCards action ->
            let
                newArr =
                    updateHasIdArray model.cards action Card.init
            in
            ( { model | cards = newArr }, Cmd.none )

        ToCard id cardMsg ->
            let
                ( newarr, nmsg ) =
                    updateId model.cards id cardMsg Card.update ToCard Cmd.none
            in
            ( { model | cards = newarr }, nmsg )

        ConfirmFirst _ _ ->
            ( model, Cmd.none )


view : CardListModel -> (Int -> CardListMsg -> msg) -> Html msg
view model lift =
    let
        getCardView card =
            Html.Styled.map (lift model.id) (Card.view card True liftCardMsg)
    in
    cardListBody
        []
        [ cardListTitle [] [ text model.text ]
        , (Array.toList model.cards |> List.map getCardView)
            ++ [ ActOnCards
                    >> lift model.id
                    |> getButton sbtn False Add
               ]
            |> cardListCardContainer []
        ]


liftCardMsg : Int -> CardMsg -> CardListMsg
liftCardMsg id cardMsg =
    case cardMsg of
        CardAction action ->
            ActOnCards action

        Card.ConfirmFirst str msg ->
            ConfirmFirst str (liftCardMsg id msg)

        _ ->
            ToCard id cardMsg
