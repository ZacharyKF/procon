module ChartColumn exposing (..)

import Array exposing (..)
import Chart exposing (Datum)
import Hex exposing (..)
import Html.Styled exposing (..)
import List exposing (..)
import Styling exposing (cardListTitle, chartColumnBody, chartContainer, chartContentsList, coloredBox, labelRow, pieChart)


type alias ChartColumnData =
    { data : List ( String, Int ), title : String }


view : ChartColumnData -> Html msg
view model =
    let
        totalDatum =
            List.length model.data

        totalValue =
            model.data |> List.map Tuple.second |> List.sum

        colorStep =
            (255 * 3) / toFloat (List.length model.data + 1) |> floor

        chartDatum =
            model.data |> List.sort |> List.indexedMap (indexValueToChartDatum totalValue (indexToColor colorStep))
    in
    chartColumnBody []
        [ cardListTitle [] [ text model.title ]
        , chartContainer [] [ chartDatum |> List.map Tuple.second |> pieChart 250 ]
        , cardListTitle [] [ text "Legend" ]
        , chartDatum |> List.map Tuple.first |> chartContentsList []
        ]


indexToColor : Int -> Int -> String
indexToColor colorStep index =
    let
        colorString offset =
            let
                colorNum =
                    modBy ((colorStep * (index + 1)) + offset) 765
            in
            if colorNum > 510 then
                "00"

            else if colorNum > 255 then
                let
                    colorStr =
                        Hex.toString (510 - colorNum)
                in
                case String.length colorStr of
                    2 ->
                        colorStr

                    _ ->
                        "0" ++ colorStr

            else
                let
                    colorStr =
                        Hex.toString colorNum
                in
                case String.length colorStr of
                    2 ->
                        colorStr

                    _ ->
                        "0" ++ colorStr
    in
    "#" ++ colorString 0 ++ colorString 510 ++ colorString 255


indexValueToChartDatum : Int -> (Int -> String) -> Int -> ( String, Int ) -> ( Html msg, Datum )
indexValueToChartDatum totalValue colorPicker index ( title, value ) =
    let
        percString =
            (toFloat value / toFloat totalValue) * 100 |> String.fromFloat |> String.slice 0 4

        color =
            colorPicker index
    in
    ( labelRow []
        [ coloredBox color [] []
        , text (title ++ " " ++ percString ++ "%")
        ]
    , { color = color, value = toFloat value }
    )
