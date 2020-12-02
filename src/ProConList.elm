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
    { a | proList : CardListModel, conList : CardListModel }


cons : CardListModel -> CardListModel -> Int -> String -> Bool -> ProConListModel
cons proList conList id text edit =
    { proList = proList, conList = conList, id = id, text = text, edit = edit }


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
        [ ( "pro_list", cardListEncoder model.proList )
        , ( "con_list", cardListEncoder model.conList )
        , ( "id", E.int model.id )
        , ( "edit", E.bool model.edit )
        , ( "text", E.string model.text )
        ]


type ProConListMsg
    = ToList Int CardListMsg
    | TextAction WithTextAction
    | ProConAction WithIdAction
    | ConfirmFirst String ProConListMsg


update : ProConListModel -> ProConListMsg -> ( ProConListModel, Cmd ProConListMsg )
update model msg =
    case msg of
        ToList id listMsg ->
            case id of
                0 ->
                    let
                        ( newlist, subCmd ) =
                            CardList.update model.proList listMsg
                    in
                    ( { model | proList = newlist }, Cmd.map (ToList 0) subCmd )

                _ ->
                    let
                        ( newlist, subCmd ) =
                            CardList.update model.conList listMsg
                    in
                    ( { model | conList = newlist }, Cmd.map (ToList 1) subCmd )

        TextAction action ->
            ( updateWithText model action, Cmd.none )

        ProConAction _ ->
            ( model, Cmd.none )

        ConfirmFirst _ _ ->
            ( model, Cmd.none )


view : ProConListModel -> (Int -> ProConListMsg -> msg) -> Html msg
view model lift =
    let
        liftListView list =
            Html.Styled.map (lift model.id) (CardList.view list liftListMsg)

        pclLift =
            lift model.id
    in
    proConListBody
        []
        [ proConListTitleText []
            [ TextAction
                >> pclLift
                |> getClickableTextArea proConListStatic proConListEdit model
            , ProConAction
                >> pclLift
                |> getButton pbtn True (Move model.id Up)
            , ProConAction
                >> pclLift
                |> getButton pbtn True (Move model.id Down)
            , ProConAction
                >> ConfirmFirst "Are you sure you want to delete this ProConList?"
                >> pclLift
                |> getButton pbtn True (Delete model.id)
            ]
        , simpleScrollableFlexDiv [] [ liftListView model.proList, liftListView model.conList ]
        ]


liftListMsg : Int -> CardListMsg -> ProConListMsg
liftListMsg id listMsg =
    case listMsg of
        CardList.ConfirmFirst str msg ->
            ConfirmFirst str (liftListMsg id msg)

        _ ->
            ToList id listMsg
