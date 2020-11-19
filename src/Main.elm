port module Main exposing (Model, Msg, init, subscriptions, update, view)

import Array exposing (Array)
import Browser
import Browser.Navigation as Nav exposing (load)
import Dict exposing (Dict)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick)
import Json.Decode as D exposing (Decoder, array, decodeString, field, int, map2)
import Json.Encode as E exposing (..)
import Platform.Cmd exposing (Cmd)
import ProConListView exposing (..)
import Styling exposing (..)
import Url
import WithId exposing (..)



-- TODO: DELETE CONFIRM POPUP
-- TODO: CLICK AND DRAG TO RE-ORGANIZE, REMOVE BUTTONS FOR SORTING
-- TODO: DELETE HOVER OVER BOTTOM RIGHT
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
    | ActOnListViews WithIdAction
    | SetView Int
    | Load String
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update container_msg model =
    case container_msg of
        ActOnListViews action ->
            let
                newArr =
                    updateHasIdArray model.card_lists action ProConListView.init
            in
            case action of
                Move id dir ->
                    case dir of
                        Down ->
                            ( { model | card_lists = newArr, in_view = Basics.min (id + 1) (Array.length newArr - 1) }, and_save model Cmd.none )

                        Up ->
                            ( { model | card_lists = newArr, in_view = Basics.max (id - 1) 0 }, and_save model Cmd.none )

                Delete id ->
                    ( { model | card_lists = newArr, in_view = id - 1 }, and_save model Cmd.none )

                Add ->
                    ( { model | card_lists = newArr }, and_save model Cmd.none )

        ToProConListView id child_msg ->
            let
                ( newArr, nmsg ) =
                    updateId model.card_lists id child_msg ProConListView.update ToProConListView Cmd.none
            in
            ( { model | card_lists = newArr }, and_save model nmsg )

        Load value ->
            case decodeString dataDecoder value of
                Err _ ->
                    ( model, Cmd.none )

                Ok val ->
                    ( { model | card_lists = val.card_lists }, Cmd.none )

        SetView id ->
            ( { model | in_view = id }, and_save model Cmd.none )

        _ ->
            ( model, and_save model Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "ProCon Tool"
    , body =
        [ toUnstyled
            (maincontent
                []
                [ pdivcol
                    []
                    (List.map pro_con_to_button (Array.toList model.card_lists)
                        ++ [ getButton pbtn False Add ActOnListViews ]
                    )
                , get_view_or_help model
                ]
            )
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
            nocntntdiv [] [ text "Press *➕* to start!" ]

        Just pclv ->
            ProConListView.view pclv lift_card_list_msg


pro_con_to_button : ProConListViewModel -> Html Msg
pro_con_to_button pclv =
    pbtn [ onClick (SetView pclv.id) ] [ text pclv.text ]


and_save : Model -> Cmd msg -> Cmd msg
and_save data cmd =
    Cmd.batch [ cmd, save (dataEncoder { card_lists = data.card_lists }) ]


lift_card_list_msg : Int -> ProConListViewMsg -> Msg
lift_card_list_msg id card_msg =
    case card_msg of
        WithIdAction action ->
            ActOnListViews action

        _ ->
            ToProConListView id card_msg
