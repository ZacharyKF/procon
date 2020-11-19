module Card.Card exposing (..)

import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Json.Decode as D exposing (Decoder, bool, field, int, map3, string)
import Json.Encode as E exposing (..)
import Styling exposing (..)
import WithId exposing (..)
import WithText exposing (..)


type alias CardModel =
    WithText (WithId {})


init : Int -> CardModel
init id =
    cons id "Add information here..." False


cons : Int -> String -> Bool -> CardModel
cons id str edit =
    { id = 0, text = "", edit = False }
        |> withTextCons str edit
        >> withIdCons id


cardDecoder : Decoder CardModel
cardDecoder =
    map3 cons
        (field "id" D.int)
        (field "text" D.string)
        (field "edit" D.bool)


cardEncoder : CardModel -> E.Value
cardEncoder model =
    E.object
        [ ( "id", E.int model.id )
        , ( "text", E.string model.text )
        , ( "edit", E.bool model.edit )
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


view : CardModel -> (Int -> CardMsg -> msg) -> Html msg
view model lift =
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
            , CardAction
                >> ConfirmFirst "Are you sure you want to delete this Card?"
                >> clift
                |> getButton sbtn False (Delete model.id)
            ]
        ]
