module ChartColumn exposing (..)

import Array exposing (..)
import Chart exposing (Datum)
import Html.Styled exposing (..)
import List exposing (..)
import Styling exposing (cardListTitle, chartColumnBody, chartContainer, chartContentsList, coloredBox, labelRow, pieChart)


type alias ChartColumnData =
    { data : List ( String, Int ), title : String, mode : ChartMode }


type ChartMode
    = Linear
    | Logarithmic
    | Exponential



-- Looooong list of colors


colors : Array String
colors =
    Array.fromList [ "#00FFFF", "#7FFFD4", "#FFE4C4", "#000000", "#FFEBCD", "#0000FF", "#8A2BE2", "#A52A2A", "#DEB887", "#5F9EA0", "#7FFF00", "#D2691E", "#FF7F50", "#6495ED", "#DC143C", "#00FFFF", "#00008B", "#008B8B", "#B8860B", "#A9A9A9", "#006400", "#A9A9A9", "#BDB76B", "#8B008B", "#556B2F", "#FF8C00", "#9932CC", "#8B0000", "#E9967A", "#8FBC8F", "#483D8B", "#2F4F4F", "#2F4F4F", "#00CED1", "#9400D3", "#FF1493", "#00BFFF", "#696969", "#1E90FF", "#B22222", "#228B22", "#FF00FF", "#FFD700", "#DAA520", "#808080", "#008000", "#ADFF2F", "#808080", "#FF69B4", "#CD5C5C", "#4B0082", "#F0E68C", "#7CFC00", "#FFFACD", "#ADD8E6", "#F08080", "#90EE90", "#FFB6C1", "#FFA07A", "#20B2AA", "#87CEFA", "#778899", "#778899", "#B0C4DE", "#00FF00", "#32CD32", "#FF00FF", "#800000", "#66CDAA", "#0000CD", "#BA55D3", "#9370DB", "#3CB371", "#7B68EE", "#00FA9A", "#48D1CC", "#C71585", "#191970", "#FFE4B5", "#FFDEAD", "#000080", "#808000", "#6B8E23", "#FFA500", "#FF4500", "#DA70D6", "#EEE8AA", "#98FB98", "#AFEEEE", "#DB7093", "#FFDAB9", "#CD853F", "#FFC0CB", "#DDA0DD", "#B0E0E6", "#800080", "#663399", "#FF0000", "#BC8F8F", "#4169E1", "#8B4513", "#FA8072", "#F4A460", "#2E8B57", "#A0522D", "#C0C0C0", "#87CEEB", "#6A5ACD", "#708090", "#708090", "#00FF7F", "#4682B4", "#D2B48C", "#008080", "#D8BFD8", "#FF6347", "#40E0D0", "#EE82EE", "#F5DEB3", "#FFFF00", "#9ACD32" ]


view : ChartColumnData -> Html msg
view model =
    let
        totalDatum =
            List.length model.data

        chartDatum =
            model.data |> List.sort |> List.indexedMap (indexValueToChartDatum totalDatum)
    in
    chartColumnBody []
        [ cardListTitle [] [ text model.title ]
        , chartContainer [] [ chartDatum |> List.map Tuple.second |> pieChart 250 ]
        , cardListTitle [] [ text "Legend" ]
        , chartDatum |> List.map Tuple.first |> chartContentsList []
        ]


indexValueToChartDatum : Int -> Int -> ( String, Int ) -> ( Html msg, Datum )
indexValueToChartDatum total index ( title, value ) =
    let
        colorIndex =
            floor ((toFloat index / toFloat total) * toFloat (Array.length colors))

        color =
            case Array.get colorIndex colors of
                Nothing ->
                    "#00FFFF"

                Just smthng ->
                    smthng
    in
    ( labelRow []
        [ coloredBox color [] []
        , text title
        ]
    , { color = color, value = toFloat value }
    )



-- pieChart 300 [ { color = "#0ff", value = 3 }, { color = "purple", value = 27 } ]
--             , pieChart 300 [ { color = "#0ff", value = 3 }, { color = "purple", value = 27 } ]
