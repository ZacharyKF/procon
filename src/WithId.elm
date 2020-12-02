module WithId exposing (..)

import Array exposing (Array)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Styling exposing (webkitStrokeBtn)


type alias WithId a =
    { a | id : Int }


type alias HasId a =
    WithId a


type alias HasIdArray a =
    Array (HasId a)


type alias WithHasIdArray a b =
    { a | children : HasIdArray b }


type alias HasIdUpdate cmsg a =
    HasId a -> HasIdMsg cmsg -> ( HasId a, Cmd cmsg )


type alias HasIdMsg cmsg =
    cmsg


type alias LiftHasIdMsg cmsg msg =
    Int -> cmsg -> msg


type alias AltMsg msg =
    Cmd msg


type WithIdAction
    = Move Int WithIdDir
    | Add
    | Delete Int


type WithIdDir
    = Up
    | Down


updateHasIdArray : HasIdArray a -> WithIdAction -> (Int -> HasId a) -> HasIdArray a
updateHasIdArray arr act new =
    case act of
        Move id dir ->
            move arr id dir

        Delete id ->
            remove arr id

        Add ->
            Array.push (new (Array.length arr)) arr


updateId : HasIdArray a -> Int -> HasIdMsg cmsg -> HasIdUpdate cmsg a -> LiftHasIdMsg cmsg msg -> AltMsg msg -> ( HasIdArray a, Cmd msg )
updateId arr id cmsg update lift altCmd =
    let
        maybeHasId =
            Array.get id arr
    in
    case maybeHasId of
        Nothing ->
            ( arr, altCmd )

        Just hasId ->
            let
                ( newHasId, subCmd ) =
                    update hasId cmsg
            in
            ( Array.set id newHasId arr, Cmd.map (lift id) subCmd )


move : HasIdArray a -> Int -> WithIdDir -> HasIdArray a
move arr id dir =
    case dir of
        Up ->
            let
                ( item, upItem ) =
                    ( Array.get id arr, Array.get (id - 1) arr )
            in
            case ( item, upItem ) of
                ( Just itema, Just itemb ) ->
                    let
                        newa =
                            { itema | id = id - 1 }

                        newb =
                            { itemb | id = id }
                    in
                    Array.set id newb (Array.set (id - 1) newa arr)

                _ ->
                    arr

        Down ->
            let
                ( item, downItem ) =
                    ( Array.get id arr, Array.get (id + 1) arr )
            in
            case ( item, downItem ) of
                ( Just itema, Just itemb ) ->
                    let
                        newa =
                            { itema | id = id + 1 }

                        newb =
                            { itemb | id = id }
                    in
                    Array.set id newb (Array.set (id + 1) newa arr)

                _ ->
                    arr


remove : HasIdArray a -> Int -> HasIdArray a
remove arr id =
    let
        idFilter a =
            if a.id == id then
                Nothing

            else if a.id < id then
                Just a

            else
                Just { a | id = a.id - 1 }

        newarr =
            List.filterMap idFilter (Array.toList arr) |> Array.fromList
    in
    newarr


getButton : (List (Attribute pmsg) -> List (Html pmsg) -> Html pmsg) -> Bool -> WithIdAction -> (WithIdAction -> pmsg) -> Html pmsg
getButton btn horz action lift =
    case action of
        Move _ dir ->
            btn
                [ lift action |> onClick, webkitStrokeBtn ]
                [ getDirText dir horz |> text ]

        Add ->
            btn
                [ lift action |> onClick, webkitStrokeBtn ]
                [ text "➕" ]

        Delete _ ->
            btn
                [ lift action |> onClick, webkitStrokeBtn ]
                [ text "❌" ]


getDirText : WithIdDir -> Bool -> String
getDirText dir horz =
    case dir of
        Up ->
            if horz then
                "⬅"

            else
                "⬆"

        Down ->
            if horz then
                "➡"

            else
                "⬇"
