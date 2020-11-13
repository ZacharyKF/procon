module Positioning exposing (..)

import Browser.Dom as Dom
import Platform.Cmd as Cmd
import Task exposing (Task)


type alias Position =
    { x : Float
    , y : Float
    }


default_pos : () -> Position
default_pos _ =
    { x = 0, y = 0 }


type alias Bounds =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


default_bounds : () -> Bounds
default_bounds _ =
    { x = 0, y = 0, width = 0, height = 0 }


get_bounds : (Maybe Dom.Element -> msg) -> String -> Cmd msg
get_bounds conv id =
    Dom.getElement id
        |> convert_elem conv


convert_elem : (Maybe Dom.Element -> msg) -> Task Dom.Error Dom.Element -> Cmd msg
convert_elem conv task =
    Task.attempt (do_if_alright conv) task


do_if_alright : (Maybe Dom.Element -> msg) -> Result Dom.Error Dom.Element -> msg
do_if_alright conv res =
    case res of
        Ok val ->
            conv (Just val)

        Err err ->
            conv Nothing
