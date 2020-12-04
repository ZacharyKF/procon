module Styling exposing (..)

import Chart exposing (Dataset, pie)
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (attribute, css, href, src, style)
import Html.Styled.Events exposing (onClick)
import Svg.Styled exposing (svg)


theme :
    { secondaryBack : Color
    , secondaryEmp : Color
    , secondary : Color
    , primaryBack : Color
    , primary : Color
    , primaryEmp : Color
    , textColor : Color
    , textShadow : Color
    , background : Color
    }
theme =
    { primaryBack = hex "C37E95"
    , primary = hex "c87e91"
    , primaryEmp = hex "C87E90"
    , secondaryBack = hex "90C383"
    , secondary = hex "91C87E"
    , secondaryEmp = hex "95C37E"
    , textColor = hex "f6f4f4"
    , textShadow = hex "090e07"
    , background = hex "fdecee"
    }


basicFont : Style
basicFont =
    Css.batch
        [ textShadow4 (Css.px 0) (Css.px 0) (Css.px 3) theme.textShadow
        , color theme.textColor
        , fontWeight (int 700)
        ]


inverseFont : Style
inverseFont =
    Css.batch
        [ textShadow4 (Css.px 0) (Css.px 0) (Css.px 2) theme.textColor
        , color theme.textShadow
        , fontWeight (int 700)
        ]


simpleScrollableFlexDiv : List (Attribute msg) -> List (Html msg) -> Html msg
simpleScrollableFlexDiv =
    styled div
        [ displayFlex
        , overflowY auto
        , margin (Css.em 0.25)
        ]


simplePrimaryTitle : List (Attribute msg) -> List (Html msg) -> Html msg
simplePrimaryTitle =
    styled div
        [ basicFont
        , minHeight (Css.em 1.5)
        , resize none
        , backgroundColor theme.primaryBack
        , displayFlex
        , alignItems center
        , justifyContent center
        ]


buttonMarginWrapper : List (Attribute msg) -> List (Html msg) -> Html msg
buttonMarginWrapper =
    styled div [ margin (Css.em 0.5) ]


webkitStrokeBtn : Html.Styled.Attribute msg
webkitStrokeBtn =
    style "-webkit-text-stroke" "1px black"


btnStyle : Style
btnStyle =
    Css.batch
        [ justifyContent center
        , alignItems center
        , paddingBottom (Css.em 0.2)
        , paddingTop (Css.em 0.2)
        , paddingRight (Css.em 0.5)
        , paddingLeft (Css.em 0.5)
        , minHeight (Css.em 1.5)
        , minWidth (Css.em 2)
        , zIndex (int 10)
        , displayFlex
        , hover
            [ fontWeight (int 900)
            , boxShadow4 (Css.em 0) (Css.em 0.3) (Css.em 0.4) theme.textShadow
            , zIndex (int 13)
            ]
        ]


pbtn : List (Attribute msg) -> List (Html msg) -> Html msg
pbtn =
    styled div
        [ backgroundColor theme.primary
        , basicFont
        , btnStyle
        , hover
            [ backgroundColor theme.primaryEmp
            ]
        ]


sbtn : List (Attribute msg) -> List (Html msg) -> Html msg
sbtn =
    styled div
        [ backgroundColor theme.secondary
        , basicFont
        , btnStyle
        , hover
            [ backgroundColor theme.secondaryEmp
            ]
        ]



-- Main --


mainContent : List (Attribute msg) -> List (Html msg) -> Html msg
mainContent =
    styled div
        [ displayFlex
        , maxHeight (Css.vh 100)
        , maxWidth (Css.vw 100)
        , height (Css.vh 100)
        , width (Css.vw 100)
        ]


proConListViewButtons : List (Attribute msg) -> List (Html msg) -> Html msg
proConListViewButtons =
    styled div
        [ displayFlex
        , flex (int 1)
        , maxWidth (Css.vw 10)
        , flexGrow (int 1)
        , flexDirection column
        , backgroundColor theme.primaryBack
        , padding (Css.em 0.2)
        , boxShadow4 (Css.em 0) (Css.em 0.4) (Css.em 0.5) theme.textShadow
        , zIndex (int 10)
        ]


noContentDiv : List (Attribute msg) -> List (Html msg) -> Html msg
noContentDiv =
    styled div
        [ displayFlex
        , flex (int 1)
        , alignItems center
        , justifyContent center
        , inverseFont
        ]


popupContainer : List (Attribute msg) -> List (Html msg) -> Html msg
popupContainer =
    styled div
        [ position absolute
        , height (Css.pct 100)
        , width (Css.pct 100)
        , displayFlex
        , justifyContent center
        , alignItems center
        ]


popupBacking : List (Attribute msg) -> List (Html msg) -> Html msg
popupBacking =
    styled div
        [ height (Css.pct 100)
        , width (Css.pct 100)
        , backgroundColor theme.textShadow
        , opacity (num 0.5)
        , zIndex (int 11)
        ]


popupBody : List (Attribute msg) -> List (Html msg) -> Html msg
popupBody =
    styled div
        [ position absolute
        , flex (int 1)
        , flexDirection column
        , zIndex (int 12)
        , backgroundColor theme.background
        , outline3 (Css.px 1) solid theme.textShadow
        ]


popupText : List (Attribute msg) -> List (Html msg) -> Html msg
popupText =
    styled div
        [ inverseFont
        , backgroundColor theme.background
        , margin (Css.em 0.5)
        , paddingBottom (Css.em 1)
        ]


popupButtonContainer : List (Attribute msg) -> List (Html msg) -> Html msg
popupButtonContainer =
    styled div
        [ displayFlex
        , justifyContent spaceBetween
        ]



-- ProConListView --


proConListViewBody : List (Attribute msg) -> List (Html msg) -> Html msg
proConListViewBody =
    styled div
        [ flexDirection column
        , displayFlex
        , overflow hidden
        , backgroundColor theme.background
        , width (Css.pct 100)
        , maxWidth (Css.vw 90)
        , justifyContent flexStart
        , flex (int 1)
        ]


proConListViewTitleContainer : List (Attribute msg) -> List (Html msg) -> Html msg
proConListViewTitleContainer =
    styled div
        [ height (Css.em 2)
        , backgroundColor theme.secondaryBack
        , displayFlex
        , basicFont
        , alignItems center
        , boxShadow4 (Css.em 0) (Css.em 0.2) (Css.em 0.3) theme.textShadow
        , zIndex (int 9)
        ]


proConListViewStatic : List (Attribute msg) -> List (Html msg) -> Html msg
proConListViewStatic =
    styled div
        [ marginLeft (Css.em 2)
        , flex (int 10)
        , minHeight (Css.em 1)
        , overflow auto
        ]


proConListViewEdit : List (Attribute msg) -> List (Html msg) -> Html msg
proConListViewEdit =
    styled textarea
        [ marginLeft (Css.em 2)
        , flex (int 10)
        , minHeight (Css.em 1)
        , resize none
        , overflow auto
        ]


proConListViewContent : List (Attribute msg) -> List (Html msg) -> Html msg
proConListViewContent =
    styled div
        [ flex (int 1)
        , displayFlex
        , overflowX auto
        , overflowY hidden
        , flexDirection row
        , padding (Css.em 0.25)
        , zIndex (int 7)
        ]



-- ProConList --


proConListBody : List (Attribute msg) -> List (Html msg) -> Html msg
proConListBody =
    styled div
        [ displayFlex
        , flexDirection column
        , overflowX hidden
        , overflowY auto
        , backgroundColor theme.background
        , minWidth (Css.pct 50)
        , margin (Css.em 0.25)
        , justifyContent flexStart
        , boxShadow4 (Css.em 0) (Css.em 0.2) (Css.em 0.3) theme.textShadow
        , zIndex (int 8)
        ]


proConListTitleText : List (Attribute msg) -> List (Html msg) -> Html msg
proConListTitleText =
    styled div
        [ basicFont
        , minHeight (Css.em 3)
        , resize none
        , backgroundColor theme.primaryBack
        , displayFlex
        , alignItems center
        ]


proConListButton : List (Attribute msg) -> List (Html msg) -> Html msg
proConListButton =
    styled div
        [ backgroundColor theme.primaryBack
        , basicFont
        , btnStyle
        , margin (Css.em 0.05)
        , flex (int 1)
        , hover
            [ backgroundColor theme.primaryEmp
            ]
        ]


proConListStatic : List (Attribute msg) -> List (Html msg) -> Html msg
proConListStatic =
    styled div
        [ width (Css.pc 100)
        , minHeight (Css.em 1)
        , justifyContent center
        , textAlign center
        , overflow auto
        ]


proConListEdit : List (Attribute msg) -> List (Html msg) -> Html msg
proConListEdit =
    styled textarea
        [ width (Css.pc 100)
        , minHeight (Css.em 1)
        , justifyContent center
        , textAlign center
        , resize none
        , overflow auto
        ]



-- CardList --


cardListBody : List (Attribute msg) -> List (Html msg) -> Html msg
cardListBody =
    styled div
        [ displayFlex
        , flex (int 1)
        , flexDirection column
        , backgroundColor theme.background
        , margin (Css.em 0.2)
        , justifyContent flexStart
        , boxShadow4 (Css.em 0) (Css.em 0.3) (Css.em 0.4) theme.textShadow
        , zIndex (int 4)
        ]


cardListTitle : List (Attribute msg) -> List (Html msg) -> Html msg
cardListTitle =
    styled div
        [ basicFont
        , height (Css.em 1.8)
        , minHeight (Css.em 1.8)
        , resize none
        , backgroundColor theme.secondaryEmp
        , displayFlex
        , alignItems center
        , justifyContent center
        ]


cardListCardContainer : List (Attribute msg) -> List (Html msg) -> Html msg
cardListCardContainer =
    styled div [ displayFlex, flexDirection column ]



-- Card --


card : List (Attribute msg) -> List (Html msg) -> Html msg
card =
    styled div
        [ boxShadow4 (Css.em 0) (Css.em 0) (Css.em 0.5) theme.textShadow
        , backgroundColor theme.background
        , displayFlex
        , margin (Css.em 0.25)
        , padding (Css.em 0.25)
        , height (Css.em 6)
        ]


cardButtonContainer : List (Attribute msg) -> List (Html msg) -> Html msg
cardButtonContainer =
    styled div
        [ backgroundColor theme.secondaryBack
        , basicFont
        , displayFlex
        , margin (Css.em 0.05)
        , justifyContent spaceBetween
        , flexDirection column
        ]


cardStatic : List (Attribute msg) -> List (Html msg) -> Html msg
cardStatic =
    styled div
        [ inverseFont, flex (int 10), overflow auto ]


cardEdit : List (Attribute msg) -> List (Html msg) -> Html msg
cardEdit =
    styled textarea
        [ inverseFont, flex (int 10), resize none, overflow auto ]



-- Graph --


graphPageContainer : List (Attribute msg) -> List (Html msg) -> Html msg
graphPageContainer =
    styled div
        [ displayFlex, flexDirection column, flex (int 1), minHeight (Css.em 1.5) ]


modeButtonContainer : List (Attribute msg) -> List (Html msg) -> Html msg
modeButtonContainer =
    styled div
        [ displayFlex

        -- , minHeight (Css.em 1.5)
        ]


graphModeBtn : List (Attribute msg) -> List (Html msg) -> Html msg
graphModeBtn =
    styled div
        [ backgroundColor theme.primary
        , basicFont
        , btnStyle
        , flex (int 1)
        , hover
            [ backgroundColor theme.primaryEmp
            ]
        ]



-- ChartColumn --


chartColumnBody : List (Attribute msg) -> List (Html msg) -> Html msg
chartColumnBody =
    styled div
        [ displayFlex
        , flexDirection column
        , flex (int 1)
        , boxShadow4 (Css.em 0) (Css.em 0.3) (Css.em 0.4) theme.textShadow
        , margin (Css.em 0.25)
        ]


chartContainer : List (Attribute msg) -> List (Html msg) -> Html msg
chartContainer =
    styled div
        [ displayFlex
        , minHeight fitContent
        , margin (Css.em 0.25)
        , justifyContent spaceAround
        , flex (int 1)
        ]


pieChart : Int -> Dataset -> Html msg
pieChart i datum =
    svg
        []
        [ pie i datum ]


chartContentsList : List (Attribute msg) -> List (Html msg) -> Html msg
chartContentsList =
    styled div
        [ displayFlex, flexDirection column, flex (int 3) ]


labelRow : List (Attribute msg) -> List (Html msg) -> Html msg
labelRow =
    styled div
        [ displayFlex
        , height (Css.em 1.5)
        , margin (Css.em 0.25)
        , paddingLeft (Css.em 1)
        , paddingRight (Css.em 1)
        , alignItems center
        ]


coloredBox : String -> List (Attribute msg) -> List (Html msg) -> Html msg
coloredBox color =
    styled div
        [ boxShadow4 (Css.em 0) (Css.em 0.3) (Css.em 0.4) theme.textShadow
        , width (Css.em 1)
        , height (Css.em 1)
        , backgroundColor (hex color)
        , marginRight (Css.em 1)
        ]
