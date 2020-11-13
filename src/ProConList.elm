module ProConList exposing (..)

import Card.CardList as CardList exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Json.Decode as D exposing (Decoder, bool, field, int, map5, string)
import Json.Encode as E exposing (..)
import Styling exposing (..)


type alias ProConListModel =
    { pro_list : CardListModel
    , con_list : CardListModel
    , id : Int
    , text : String
    , edit : Bool
    }


proConListDecoder : Decoder ProConListModel
proConListDecoder =
    map5 ProConListModel
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


init : Int -> ProConListModel
init id =
    { pro_list = CardList.init 0 "👍"
    , con_list = CardList.init 1 "👎"
    , id = id
    , text = "🤷\u{200D}♀️❔🤷\u{200D}♂️"
    , edit = False
    }


type ProConListMsg
    = ToList Int CardListMsg
    | Change String
    | Edit Bool


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
                    ( { model | con_list = newlist }, Cmd.map (ToList 0) sub_cmd )

        Change str ->
            ( { model | text = str }, Cmd.none )

        Edit bool ->
            ( { model | edit = bool }, Cmd.none )


view : ProConListModel -> (Int -> ProConListMsg -> msg) -> Html msg
view model lift =
    let
        lift_list_view list =
            Html.Styled.map (lift model.id) (CardList.view list lift_list_msg)
    in
    pcl
        []
        [ get_body model lift
        , div [ style "display" "flex" ]
            [ lift_list_view model.pro_list, lift_list_view model.con_list ]
        ]


get_body : ProConListModel -> (Int -> ProConListMsg -> msg) -> Html msg
get_body model lift =
    let
        strToOut str =
            lift model.id (Change str)
    in
    if model.edit then
        pcltitletext []
            [ textarea
                [ onDoubleClick <| lift model.id (Edit <| not model.edit)
                , placeholder "🤷\u{200D}♀️❔🤷\u{200D}♂️"
                , onInput strToOut
                , value model.text
                , style "height" "3em"
                , style "resize" "none"
                ]
                []
            ]

    else
        pcltitletext
            [ onDoubleClick <| lift model.id (Edit <| not model.edit)
            , style "height" "3em"
            ]
            [ text model.text
            ]


lift_list_msg : Int -> CardListMsg -> ProConListMsg
lift_list_msg id list_msg =
    case list_msg of
        _ ->
            ToList id list_msg
