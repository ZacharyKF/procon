module Card.Card exposing (..)

import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Json.Decode as D exposing (Decoder, bool, field, int, map3, string)
import Json.Encode as E exposing (..)
import Styling exposing (..)


type alias CardModel =
    { id : Int
    , text : String
    , edit : Bool
    }


init : Int -> CardModel
init id =
    { text = "🤷\u{200D}♀️❔🤷\u{200D}♂️"
    , edit = False
    , id = id
    }


cardDecoder : Decoder CardModel
cardDecoder =
    map3 CardModel
        (field "id" D.int)
        (field "text" D.string)
        (field "edit" D.bool)


cardEncoder : CardModel -> E.Value
cardEncoder model =
    E.object
        [ ( "id", E.int model.id )
        , ( "text", E.string model.text )
        , ( "edit", E.bool model.edit )
        ]


type CardMsg
    = Change String
    | Edit Bool
    | MoveUp
    | MoveDown
    | Delete


update : CardModel -> CardMsg -> ( CardModel, Cmd CardMsg )
update card msg =
    case msg of
        Change str ->
            ( { card | text = str }, Cmd.none )

        Edit bool ->
            ( { card | edit = bool }, Cmd.none )

        _ ->
            ( card, Cmd.none )


view : CardModel -> (Int -> CardMsg -> msg) -> Html msg
view model lift =
    let
        ( del, up, down ) =
            get_updownbuttons model lift
    in
    card
        [ onDoubleClick <| lift model.id (Edit <| not model.edit) ]
        [ get_body model lift
        , div
            [ style "display" "flex"
            , style "flex-direction" "column"
            ]
            [ del
            , up
            , down
            ]
        ]


get_body : CardModel -> (Int -> CardMsg -> msg) -> Html msg
get_body model lift =
    let
        strToOut str =
            lift model.id (Change str)
    in
    if model.edit then
        textarea
            [ placeholder "🤷\u{200D}♀️❔🤷\u{200D}♂️"
            , onInput strToOut
            , value model.text
            , style "flex" "10"
            , style "flex-grow" "true"
            , style "resize" "none"
            ]
            []

    else
        div
            [ style "flex-grow" "true"
            , style "flex" "10"
            ]
            [ text model.text
            ]


get_updownbuttons : CardModel -> (Int -> CardMsg -> msg) -> ( Html msg, Html msg, Html msg )
get_updownbuttons model lift =
    ( tbtn
        [ onClick <| lift model.id Delete, style "margin" "0.05em", style "flex" "1", style "flex-grow" "true" ]
        [ text "☠" ]
    , tbtn
        [ onClick <| lift model.id MoveUp, style "margin" "0.05em", style "flex" "1", style "flex-grow" "true" ]
        [ text "☝" ]
    , tbtn
        [ onClick <| lift model.id MoveDown, style "margin" "0.05em", style "flex" "1", style "flex-grow" "true" ]
        [ text "👇" ]
    )
