module WithText exposing (..)

import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)


type alias WithText a =
    { a | text : String, edit : Bool }


type WithTextAction
    = Change String
    | Edit Bool


updateWithText : WithText a -> WithTextAction -> WithText a
updateWithText model action =
    case action of
        Change str ->
            { model | text = str }

        Edit bool ->
            { model | edit = bool }


getDoubleClick : WithText a -> (WithTextAction -> pmsg) -> Attribute pmsg
getDoubleClick model lift =
    not model.edit
        |> Edit
        >> lift
        |> onDoubleClick


getOnEdit : (WithTextAction -> pmsg) -> Attribute pmsg
getOnEdit lift =
    Change
        >> lift
        |> onInput


getClickableTextArea : (List (Attribute pmsg) -> List (Html pmsg) -> Html pmsg) -> (List (Attribute pmsg) -> List (Html pmsg) -> Html pmsg) -> WithText a -> (WithTextAction -> pmsg) -> Html pmsg
getClickableTextArea static edit model lift =
    if model.edit then
        edit
            [ getDoubleClick model lift
            , getOnEdit lift
            , value model.text
            ]
            []

    else
        static
            [ getDoubleClick model lift ]
            [ text model.text ]
