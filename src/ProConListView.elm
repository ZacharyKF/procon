module ProConListView exposing (..)

import Array exposing (Array)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Json.Decode as D exposing (Decoder, bool, field, int, map4, string)
import Json.Encode as E exposing (..)
import ProConList exposing (..)
import Styling exposing (..)
import WithId exposing (..)
import WithText exposing (..)


type alias ProConListViewModel =
    WithId (WithText (ProConListViewData {}))


type alias ProConListViewData a =
    { a | pro_con_lists : Array ProConListModel }


proConListViewDataCons : Array ProConListModel -> ProConListViewData a -> ProConListViewData a
proConListViewDataCons lists other =
    { other | pro_con_lists = lists }


cons : Int -> String -> Bool -> Array ProConListModel -> ProConListViewModel
cons id text edit lists =
    { id = id, text = text, edit = edit, pro_con_lists = lists }


init : Int -> ProConListViewModel
init id =
    cons id "Title Here..." False Array.empty


proConListViewDecoder : Decoder ProConListViewModel
proConListViewDecoder =
    map4 cons
        (field "id" D.int)
        (field "title" D.string)
        (field "edit" D.bool)
        (field "pro_con_lists" (D.array proConListDecoder))


proConListViewEncoder : ProConListViewModel -> E.Value
proConListViewEncoder model =
    E.object
        [ ( "edit", E.bool model.edit )
        , ( "id", E.int model.id )
        , ( "pro_con_lists", E.array proConListEncoder model.pro_con_lists )
        , ( "title", E.string model.text )
        ]


type ProConListViewMsg
    = ToProConList Int ProConListMsg
    | TextAction WithTextAction
    | WithIdAction WithIdAction
    | ActOnLists WithIdAction


update : ProConListViewModel -> ProConListViewMsg -> ( ProConListViewModel, Cmd ProConListViewMsg )
update model msg =
    case msg of
        ToProConList id pcmsg ->
            let
                ( newArr, nmsg ) =
                    updateId model.pro_con_lists id pcmsg ProConList.update ToProConList Cmd.none
            in
            ( { model | pro_con_lists = newArr }, nmsg )

        ActOnLists action ->
            let
                newArr =
                    updateHasIdArray model.pro_con_lists action ProConList.init
            in
            ( { model | pro_con_lists = newArr }, Cmd.none )

        TextAction action ->
            ( updateWithText model action, Cmd.none )

        WithIdAction _ ->
            ( model, Cmd.none )


view : ProConListViewModel -> (Int -> ProConListViewMsg -> msg) -> Html msg
view model lift =
    let
        build_list_view list =
            Html.Styled.map (lift model.id) (ProConList.view list lift_pro_con_msg)

        pclvTextLift act =
            lift model.id (TextAction act)

        pclvListLift act =
            lift model.id (ActOnLists act)

        pclvIdLift act =
            lift model.id (WithIdAction act)
    in
    proConListViewBody
        []
        [ proConListViewTitleContainer
            []
            [ getClickableTextArea proConListViewStatic proConListViewEdit model pclvTextLift
            , getButton sbtn False (Move model.id Up) pclvIdLift
            , getButton sbtn False (Move model.id Down) pclvIdLift
            , getButton sbtn False (Delete model.id) pclvIdLift
            ]
        , proConListViewContent [] <|
            List.map build_list_view (Array.toList model.pro_con_lists)
                ++ [ getButton sbtn False Add pclvListLift ]
        ]


lift_pro_con_msg : Int -> ProConListMsg -> ProConListViewMsg
lift_pro_con_msg id listmsg =
    case listmsg of
        ProConAction action ->
            ActOnLists action

        _ ->
            ToProConList id listmsg
