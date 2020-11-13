port module Main exposing (Model, Msg, init, subscriptions, update, view)

import Array exposing (Array)
import Browser
import Browser.Navigation as Nav exposing (load)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D exposing (Decoder, array, decodeString, field, int, map2)
import Json.Encode as E exposing (..)
import Platform.Cmd exposing (Cmd)
import ProConListView exposing (..)
import Url



-- TODO: ADD PER CONTAINER PRO/CON SORT PAGE (MORE DETAILS TO COME)
-- TODO: ADD PER CONTAINER RESULT DISPLAY (MORE DETAILS TO COME)


port save : E.Value -> Cmd msg


port load : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    load Load


init : flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { url = url, key = key, in_view = 0, card_lists = Array.empty }, Cmd.none )


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }


type alias Model =
    Navi Data


type alias Navi a =
    { a | url : Url.Url, key : Nav.Key, in_view : Int }


type alias Data =
    { card_lists : Array ProConListViewModel }


dataDecoder : Decoder Data
dataDecoder =
    D.map Data
        (field "card_lists" (D.array proConListViewDecoder))


dataEncoder : Data -> E.Value
dataEncoder data =
    E.object
        [ ( "card_lists", E.array proConListViewEncoder data.card_lists )
        ]


type Msg
    = ToProConListView Int ProConListViewMsg
    | AddList
    | SetView Int
    | Load String
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update container_msg model =
    case container_msg of
        AddList ->
            ( { model | card_lists = Array.push (ProConListView.init (Array.length model.card_lists)) model.card_lists }, and_save { card_lists = model.card_lists } Cmd.none )

        ToProConListView id child_msg ->
            let
                old_list =
                    Array.get id model.card_lists
            in
            case old_list of
                Nothing ->
                    ( model, and_save { card_lists = model.card_lists } Cmd.none )

                Just alist ->
                    let
                        ( newlist, sub_cmd ) =
                            ProConListView.update alist child_msg
                    in
                    ( { model | card_lists = Array.set id newlist model.card_lists }, and_save { card_lists = model.card_lists } (Cmd.map (ToProConListView id) sub_cmd) )

        Load value ->
            case decodeString dataDecoder value of
                Err _ ->
                    ( model, Cmd.none )

                Ok val ->
                    ( { model | card_lists = val.card_lists }, Cmd.none )

        SetView id ->
            ( { model | in_view = id }, and_save { card_lists = model.card_lists } Cmd.none )

        _ ->
            ( model, and_save { card_lists = model.card_lists } Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Application Title"
    , body =
        [ div
            [ style "display" "flex"
            , style "overflow" "auto"
            ]
            [ div
                [ style "display" "flex"
                , style "flex-direction" "column"
                , style "max-width" "10vw"
                , style "margin-right" "0.25em"
                , style "outline" "solid"
                ]
                (button
                    [ onClick AddList ]
                    [ text "Add ProCon" ]
                    :: List.map pro_con_to_button (Array.toList model.card_lists)
                )
            , get_view_or_help model
            ]
        ]
    }


get_view_or_help : Model -> Html Msg
get_view_or_help model =
    let
        maybe_list =
            Array.get model.in_view model.card_lists
    in
    case maybe_list of
        Nothing ->
            text "Press *Add ProCon* to start!"

        Just pclv ->
            ProConListView.view pclv lift_card_list_msg


pro_con_to_button : ProConListViewModel -> Html Msg
pro_con_to_button pclv =
    button [ onClick (SetView pclv.id) ] [ text pclv.title ]


and_save : Data -> Cmd msg -> Cmd msg
and_save data cmd =
    Cmd.batch [ cmd, save (dataEncoder data) ]


lift_card_list_msg : Int -> ProConListViewMsg -> Msg
lift_card_list_msg id card_msg =
    case card_msg of
        _ ->
            ToProConListView id card_msg
