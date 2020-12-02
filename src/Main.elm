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
import ProConList exposing (ProConListMsg(..))
import ProConListView exposing (..)
import Styling exposing (..)
import Url
import WithId exposing (..)



-- TODO: ADD PER CONTAINER PRO/CON SORT PAGE (MORE DETAILS TO COME)
-- TODO: ADD PER CONTAINER RESULT DISPLAY (MORE DETAILS TO COME)
-- TODO: CLICK AND DRAG TO RE-ORGANIZE, REMOVE BUTTONS FOR SORTING
-- TODO: DELETE HOVER OVER BOTTOM RIGHT
-- TODO: CLEAN UP THE DECODER/ENCODER LOGIC


port save : E.Value -> Cmd msg


port load : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    load Load


init : flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { url = url, key = key, inView = 0, popup = Nothing, cardLists = Array.empty }, Cmd.none )


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
    Navi (Popup Data)


type alias Navi a =
    { a | url : Url.Url, key : Nav.Key, inView : Int }


type alias Popup a =
    { a | popup : Maybe PopupData }


type alias PopupData =
    { str : String, msg : Msg }


type alias Data =
    { cardLists : Array ProConListViewModel }


dataDecoder : Decoder Data
dataDecoder =
    D.map Data
        (field "card_lists" (D.array proConListViewDecoder))


dataEncoder : Data -> E.Value
dataEncoder data =
    E.object
        [ ( "card_lists", E.array proConListViewEncoder data.cardLists )
        ]


type Msg
    = ToProConListView Int ProConListViewMsg
    | ActOnListViews WithIdAction
    | SetView Int
    | Load String
    | ConfirmFirst String Msg
    | Cancel
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update containerMsg model =
    case containerMsg of
        ActOnListViews action ->
            let
                newArr =
                    updateHasIdArray model.cardLists action ProConListView.init
            in
            case action of
                Move id dir ->
                    case dir of
                        Down ->
                            ( { model
                                | cardLists = newArr
                                , inView = Basics.min (id + 1) (Array.length newArr - 1)
                                , popup = Nothing
                              }
                            , andSave model Cmd.none
                            )

                        Up ->
                            ( { model
                                | cardLists = newArr
                                , inView = Basics.max (id - 1) 0
                                , popup = Nothing
                              }
                            , andSave model Cmd.none
                            )

                Delete id ->
                    ( { model
                        | cardLists = newArr
                        , inView = id - 1
                        , popup = Nothing
                      }
                    , andSave model Cmd.none
                    )

                Add ->
                    ( { model
                        | cardLists = newArr
                        , popup = Nothing
                      }
                    , andSave model Cmd.none
                    )

        ToProConListView id childMsg ->
            let
                ( newArr, nmsg ) =
                    updateId model.cardLists id childMsg ProConListView.update ToProConListView Cmd.none
            in
            ( { model
                | cardLists = newArr
                , popup = Nothing
              }
            , andSave model nmsg
            )

        Load value ->
            case decodeString dataDecoder value of
                Err _ ->
                    ( model, Cmd.none )

                Ok val ->
                    ( { model
                        | cardLists = val.cardLists
                        , popup = Nothing
                      }
                    , Cmd.none
                    )

        SetView id ->
            ( { model
                | inView = id
                , popup = Nothing
              }
            , andSave model Cmd.none
            )

        ConfirmFirst str msg ->
            ( { model | popup = Just { str = str, msg = msg } }, Cmd.none )

        Cancel ->
            ( { model | popup = Nothing }, Cmd.none )

        _ ->
            ( model, andSave model Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "ProCon Tool"
    , body =
        [ [ (Array.toList model.cardLists |> List.map proConToButton)
                ++ [ getButton pbtn False Add ActOnListViews ]
                |> proConListViewButtons []
          , getViewOrHelp model
          , getConfirmPopup model
          ]
            |> mainContent []
            |> toUnstyled
        ]
    }


getViewOrHelp : Model -> Html Msg
getViewOrHelp model =
    let
        maybeList =
            Array.get model.inView model.cardLists
    in
    case maybeList of
        Nothing ->
            noContentDiv [] [ text "Press *➕* to start!" ]

        Just pclv ->
            ProConListView.view pclv liftCardMsg


getConfirmPopup : Model -> Html Msg
getConfirmPopup model =
    case model.popup of
        Nothing ->
            div [] []

        Just popupData ->
            [ popupBacking [] []
            , popupBody []
                [ popupText [] [ text popupData.str ]
                , popupButtonContainer []
                    [ buttonMarginWrapper [] [ sbtn [ onClick Cancel ] [ text "Cancel" ] ]
                    , buttonMarginWrapper [] [ pbtn [ onClick popupData.msg ] [ text "Confirm" ] ]
                    ]
                ]
            ]
                |> popupContainer []


proConToButton : ProConListViewModel -> Html Msg
proConToButton pclv =
    pbtn [ onClick (SetView pclv.id) ] [ text pclv.text ]


andSave : Model -> Cmd msg -> Cmd msg
andSave data cmd =
    Cmd.batch [ cmd, save (dataEncoder { cardLists = data.cardLists }) ]


liftCardMsg : Int -> ProConListViewMsg -> Msg
liftCardMsg id cardMsg =
    case cardMsg of
        WithIdAction action ->
            ActOnListViews action

        ProConListView.ConfirmFirst str msg ->
            liftCardMsg id msg
                |> ConfirmFirst str

        _ ->
            ToProConListView id cardMsg
