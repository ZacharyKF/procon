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
    { a | proConLists : Array ProConListModel }


proConListViewDataCons : Array ProConListModel -> ProConListViewData a -> ProConListViewData a
proConListViewDataCons lists other =
    { other | proConLists = lists }


cons : Int -> String -> Bool -> Array ProConListModel -> ProConListViewModel
cons id text edit lists =
    { id = id, text = text, edit = edit, proConLists = lists }


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
        , ( "pro_con_lists", E.array proConListEncoder model.proConLists )
        , ( "title", E.string model.text )
        ]


type ProConListViewMsg
    = ToProConList Int ProConListMsg
    | TextAction WithTextAction
    | WithIdAction WithIdAction
    | ActOnLists WithIdAction
    | ConfirmFirst String ProConListViewMsg


update : ProConListViewModel -> ProConListViewMsg -> ( ProConListViewModel, Cmd ProConListViewMsg )
update model msg =
    case msg of
        ToProConList id pcmsg ->
            let
                ( newArr, nmsg ) =
                    updateId model.proConLists id pcmsg ProConList.update ToProConList Cmd.none
            in
            ( { model | proConLists = newArr }, nmsg )

        ActOnLists action ->
            let
                newArr =
                    updateHasIdArray model.proConLists action ProConList.init
            in
            ( { model | proConLists = newArr }, Cmd.none )

        TextAction action ->
            ( updateWithText model action, Cmd.none )

        WithIdAction _ ->
            ( model, Cmd.none )

        ConfirmFirst _ _ ->
            ( model, Cmd.none )


view : ProConListViewModel -> (Int -> ProConListViewMsg -> msg) -> Html msg
view model lift =
    let
        buildListView list =
            Html.Styled.map (lift model.id) (ProConList.view list liftProConMsg)

        pclvLift =
            lift model.id
    in
    proConListViewBody
        []
        [ proConListViewTitleContainer
            []
            [ TextAction
                >> pclvLift
                |> getClickableTextArea proConListViewStatic proConListViewEdit model
            , WithIdAction
                >> pclvLift
                |> getButton sbtn False (Move model.id Up)
            , WithIdAction
                >> pclvLift
                |> getButton sbtn False (Move model.id Down)
            , WithIdAction
                >> ConfirmFirst "Are you sure you want to delete this ProConListView?"
                >> pclvLift
                |> getButton sbtn False (Delete model.id)
            ]
        , (Array.toList model.proConLists |> List.map buildListView)
            ++ [ ActOnLists
                    >> pclvLift
                    |> getButton pbtn False Add
               ]
            |> proConListViewContent []
        ]


liftProConMsg : Int -> ProConListMsg -> ProConListViewMsg
liftProConMsg id listmsg =
    case listmsg of
        ProConAction action ->
            ActOnLists action

        ProConList.ConfirmFirst str msg ->
            ConfirmFirst str (liftProConMsg id msg)

        _ ->
            ToProConList id listmsg
