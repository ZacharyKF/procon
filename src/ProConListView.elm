module ProConListView exposing (..)

import Array exposing (Array)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Json.Decode as D exposing (Decoder, bool, field, int, map4, string)
import Json.Encode as E exposing (..)
import ProConList exposing (..)
import Styling exposing (..)


type alias ProConListViewModel =
    { id : Int
    , title : String
    , edit : Bool
    , pro_con_lists : Array ProConListModel
    }


init : Int -> ProConListViewModel
init id =
    { id = id
    , title = "Placeholder"
    , edit = False
    , pro_con_lists = Array.empty
    }


proConListViewDecoder : Decoder ProConListViewModel
proConListViewDecoder =
    map4 ProConListViewModel
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
        , ( "title", E.string model.title )
        ]


type ProConListViewMsg
    = ToProConList Int ProConListMsg
    | AddList
    | Edit Bool
    | Change String


update : ProConListViewModel -> ProConListViewMsg -> ( ProConListViewModel, Cmd ProConListViewMsg )
update model msg =
    case msg of
        ToProConList id pcmsg ->
            let
                old_pcl =
                    Array.get id model.pro_con_lists
            in
            case old_pcl of
                Nothing ->
                    ( model, Cmd.none )

                Just apcl ->
                    let
                        ( new_pcl, sub_cmd ) =
                            ProConList.update apcl pcmsg
                    in
                    ( { model | pro_con_lists = Array.set id new_pcl model.pro_con_lists }, Cmd.map (ToProConList id) sub_cmd )

        AddList ->
            let
                newlist =
                    ProConList.init (Array.length model.pro_con_lists)
            in
            ( { model | pro_con_lists = Array.push newlist model.pro_con_lists }, Cmd.none )

        Change str ->
            ( { model | title = str }, Cmd.none )

        Edit bool ->
            ( { model | edit = bool }, Cmd.none )


view : ProConListViewModel -> (Int -> ProConListViewMsg -> msg) -> Html msg
view model lift =
    let
        build_list_view list =
            Html.Styled.map (lift model.id) (ProConList.view list lift_pro_con_msg)
    in
    pclistview
        []
        [ pctitle
            []
            [ get_title model lift
            , sbtn [ onClick (lift model.id AddList) ] [ text "Add Option" ]
            ]
        , pccontent
            []
          <|
            List.map build_list_view (Array.toList model.pro_con_lists)
        ]


get_title : ProConListViewModel -> (Int -> ProConListViewMsg -> msg) -> Html msg
get_title model lift =
    let
        strToOut str =
            lift model.id (Change str)
    in
    if model.edit then
        pctitletext []
            [ textarea
                [ onDoubleClick <| lift model.id (Edit <| not model.edit)
                , placeholder ("Placeholder" ++ String.fromInt model.id)
                , onInput strToOut
                , value model.title
                ]
                []
            ]

    else
        pctitletext
            [ onDoubleClick <| lift model.id (Edit <| not model.edit)
            ]
            [ text model.title
            ]


lift_pro_con_msg : Int -> ProConListMsg -> ProConListViewMsg
lift_pro_con_msg id listmsg =
    case listmsg of
        _ ->
            ToProConList id listmsg
