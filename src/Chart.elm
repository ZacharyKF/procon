module Chart exposing
    ( pie
    , Dataset, Datum
    )

{-| An SVG chart library.


# Pie

@docs pie

taken from <https://github.com/camjc/elm-chart/blob/master/src/Chart.elm>

-}

import List
import Svg.Styled
import Svg.Styled.Attributes


type alias ArcOutput =
    { id : Int
    , x1 : Float
    , y1 : Float
    , x2 : Float
    , y2 : Float
    , largeArcFlag : Int
    , color : String
    }


type alias Datum =
    { color : String
    , value : Float
    }


type alias Dataset =
    List Datum


quarter : Float
quarter =
    pi / 2


half : Float
half =
    pi


round : Float
round =
    pi * 2


radius : Float
radius =
    0.5


donut : Bool
donut =
    False


getPointX : Float -> Float
getPointX angle =
    radius * cos angle


getPointY : Float -> Float
getPointY angle =
    radius * sin angle


getTotalOfDataset : Dataset -> Float
getTotalOfDataset dataset =
    List.sum (List.map .value dataset)


getArc : { dataset : Dataset, datum : Datum, index : Int, total : Float } -> ArcOutput
getArc { dataset, datum, index, total } =
    let
        drawnValue =
            getTotalOfDataset (List.take index dataset)

        startAngle =
            round * drawnValue / total

        angle =
            round * datum.value / total

        endAngle =
            startAngle + angle
    in
    { id = index
    , x1 = getPointX startAngle
    , y1 = getPointY startAngle
    , x2 = getPointX endAngle
    , y2 = getPointY endAngle
    , largeArcFlag =
        if angle > half then
            1

        else
            0
    , color = datum.color
    }


arcToPath : ArcOutput -> String
arcToPath { id, x1, y1, x2, y2, largeArcFlag, color } =
    "M0,0 L"
        ++ String.fromFloat x1
        ++ ","
        ++ String.fromFloat y1
        ++ " A0.5,0.5 0 "
        ++ String.fromInt largeArcFlag
        ++ ",1 "
        ++ String.fromFloat x2
        ++ ","
        ++ String.fromFloat y2
        ++ " z"


getArcs : Dataset -> List (Svg.Styled.Svg a)
getArcs dataset =
    List.indexedMap
        (\index datum ->
            let
                dAttribute =
                    { dataset = dataset
                    , datum = datum
                    , index = index
                    , total = getTotalOfDataset dataset
                    }
                        |> getArc
                        |> arcToPath
                        |> Svg.Styled.Attributes.d
            in
            Svg.Styled.path [ Svg.Styled.Attributes.fill datum.color, dAttribute ] []
        )
        dataset


{-| Draws a pie chart of given diameter of the dataset.
Chart.pie 300 [{color = "#0ff", value = 3}, {color = "purple", value = 27}]
-}
pie : Int -> Dataset -> Svg.Styled.Svg a
pie diameter dataset =
    let
        diameterString =
            String.fromInt diameter
    in
    Svg.Styled.svg
        [ Svg.Styled.Attributes.viewBox "-0.5 -0.5 1 1"
        , Svg.Styled.Attributes.width diameterString
        , Svg.Styled.Attributes.height diameterString
        , Svg.Styled.Attributes.style "transform: rotate(-90deg)"
        ]
        (getArcs dataset)
