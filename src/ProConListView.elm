module ProConListView exposing (..)

import Array exposing (Array)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D exposing (Decoder, bool, field, int, map4, string)
import Json.Encode as E exposing (..)
import ProConList exposing (..)


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
            Html.map (lift model.id) (ProConList.view list lift_pro_con_msg)
    in
    div
        [ style "flex-direction" "column"
        , style "display" "flex"
        , style "width" "100%"
        , style "max-width" "95vw"
        , style "justify-content" "flex-start"
        , style "flex-grow" "true"
        ]
        [ div
            [ style "display" "flex"
            , style "justify-content" "space-between"
            , style "outline" "solid"
            ]
            [ get_title model lift
            , button [ onClick (lift model.id AddList) ] [ text "Add Option" ]
            ]
        , div
            [ style "flex" "1"
            , style "display" "flex"
            , style "flex-direction" "row"
            , style "overflow" "auto"
            ]
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
        textarea
            [ onDoubleClick <| lift model.id (Edit <| not model.edit)
            , placeholder ("Placeholder" ++ String.fromInt model.id)
            , onInput strToOut
            , value model.title
            , style "height" "1.2em"
            , style "resize" "none"
            ]
            []

    else
        div
            [ onDoubleClick <| lift model.id (Edit <| not model.edit)
            , style "height" "1.1em"
            ]
            [ text model.title
            ]


lift_pro_con_msg : Int -> ProConListMsg -> ProConListViewMsg
lift_pro_con_msg id listmsg =
    case listmsg of
        _ ->
            ToProConList id listmsg
