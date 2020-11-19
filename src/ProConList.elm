module ProConList exposing (..)

import Card.CardList as CardList exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Json.Decode as D exposing (Decoder, bool, field, int, map5, string)
import Json.Encode as E exposing (..)
import Styling exposing (..)
import WithId exposing (..)
import WithText exposing (..)


type alias ProConListModel =
    WithId (WithText (ProConListData {}))


type alias ProConListData a =
    { a | pro_list : CardListModel, con_list : CardListModel }


proConDataCons : CardListModel -> CardListModel -> ProConListData a -> ProConListData a
proConDataCons proList conList other =
    { other | pro_list = proList, con_list = conList }


cons : CardListModel -> CardListModel -> Int -> String -> Bool -> ProConListModel
cons proList conList id text edit =
    { pro_list = proList, con_list = conList, id = id, text = text, edit = edit }


init : Int -> ProConListModel
init id =
    cons (CardList.init 0 "👍") (CardList.init 1 "👎") id "Label Here..." False


proConListDecoder : Decoder ProConListModel
proConListDecoder =
    map5 cons
        (field "pro_list" cardListDecoder)
        (field "con_list" cardListDecoder)
        (field "id" D.int)
        (field "text" D.string)
        (field "edit" D.bool)


proConListEncoder : ProConListModel -> E.Value
proConListEncoder model =
    E.object
        [ ( "pro_list", cardListEncoder model.pro_list )
        , ( "con_list", cardListEncoder model.con_list )
        , ( "id", E.int model.id )
        , ( "edit", E.bool model.edit )
        , ( "text", E.string model.text )
        ]


type ProConListMsg
    = ToList Int CardListMsg
    | TextAction WithTextAction
    | ProConAction WithIdAction


update : ProConListModel -> ProConListMsg -> ( ProConListModel, Cmd ProConListMsg )
update model msg =
    case msg of
        ToList id list_msg ->
            case id of
                0 ->
                    let
                        ( newlist, sub_cmd ) =
                            CardList.update model.pro_list list_msg
                    in
                    ( { model | pro_list = newlist }, Cmd.map (ToList 0) sub_cmd )

                _ ->
                    let
                        ( newlist, sub_cmd ) =
                            CardList.update model.con_list list_msg
                    in
                    ( { model | con_list = newlist }, Cmd.map (ToList 1) sub_cmd )

        TextAction action ->
            ( updateWithText model action, Cmd.none )

        ProConAction _ ->
            ( model, Cmd.none )


view : ProConListModel -> (Int -> ProConListMsg -> msg) -> Html msg
view model lift =
    let
        lift_list_view list =
            Html.Styled.map (lift model.id) (CardList.view list lift_list_msg)

        pclTextLift act =
            lift model.id (TextAction act)

        pclIdLift act =
            lift model.id (ProConAction act)
    in
    proConListBody
        []
        [ proConListTitleText []
            [ getClickableTextArea proConListStatic proConListEdit model pclTextLift
            , getButton tbtn True (Move model.id Up) pclIdLift
            , getButton tbtn True (Move model.id Down) pclIdLift
            , getButton tbtn True (Delete model.id) pclIdLift
            ]
        , div [ style "display" "flex" ]
            [ lift_list_view model.pro_list, lift_list_view model.con_list ]
        ]


lift_list_msg : Int -> CardListMsg -> ProConListMsg
lift_list_msg id list_msg =
    case list_msg of
        _ ->
            ToList id list_msg
