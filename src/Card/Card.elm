module Card.Card exposing (..)

import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Json.Decode as D exposing (Decoder, bool, field, int, map4, maybe, string)
import Json.Encode as E exposing (..)
import Styling exposing (..)
import WithId exposing (..)
import WithText exposing (..)


type alias CardModel =
    WithText (WithId { rank : Int })


init : Int -> CardModel
init id =
    Just -1 |> cons id "Add information here..." False


cons : Int -> String -> Bool -> Maybe Int -> CardModel
cons id str edit mrank =
    let
        rank =
            case mrank of
                Nothing ->
                    1000000

                Just rnk ->
                    rnk
    in
    { id = id, text = str, edit = edit, rank = rank }


cardDecoder : Decoder CardModel
cardDecoder =
    map4 cons
        (field "id" D.int)
        (field "text" D.string)
        (field "edit" D.bool)
        (maybe (field "rank" D.int))


cardEncoder : CardModel -> E.Value
cardEncoder model =
    E.object
        [ ( "id", E.int model.id )
        , ( "text", E.string model.text )
        , ( "edit", E.bool model.edit )
        , ( "rank", E.int model.rank )
        ]


type CardMsg
    = TextAction WithTextAction
    | CardAction WithIdAction
    | ConfirmFirst String CardMsg


update : CardModel -> CardMsg -> ( CardModel, Cmd CardMsg )
update card msg =
    case msg of
        TextAction action ->
            ( updateWithText card action, Cmd.none )

        _ ->
            ( card, Cmd.none )


view : CardModel -> Bool -> (Int -> CardMsg -> msg) -> Html msg
view model enableDelete lift =
    let
        clift =
            lift model.id
    in
    card
        []
        [ TextAction
            >> clift
            |> getClickableTextArea cardStatic cardEdit model
        , cardButtonContainer []
            [ CardAction
                >> clift
                |> getButton sbtn False (Move model.id Up)
            , CardAction
                >> clift
                |> getButton sbtn False (Move model.id Down)
            , if enableDelete then
                CardAction
                    >> ConfirmFirst "Are you sure you want to delete this Card?"
                    >> clift
                    |> getButton sbtn False (Delete model.id)

              else
                div [] []
            ]
        ]
