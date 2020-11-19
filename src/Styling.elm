module Styling exposing (..)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src)
import Html.Styled.Events exposing (onClick)


theme :
    { secondaryBack : Color
    , secondaryEmp : Color
    , secondary : Color
    , primaryBack : Color
    , primary : Color
    , primaryEmp : Color
    , tertiaryBack : Color
    , tertiary : Color
    , tertiaryEmp : Color
    , textColor : Color
    }
theme =
    { primaryBack = hex "8fa3cc"
    , primary = hex "96a9cf"
    , primaryEmp = hex "a8b7d7"
    , secondaryBack = hex "74a772"
    , secondary = hex "84b082"
    , secondaryEmp = hex "93ba91"
    , tertiaryBack = hex "f9b086"
    , tertiary = hex "f9b38a"
    , tertiaryEmp = hex "fac09e"
    , textColor = hex "ffffff"
    }


basicFont : Style
basicFont =
    Css.batch
        [ textShadow4 (Css.px 0) (Css.px 0) (Css.px 4) (hex "000000")
        , color theme.textColor
        ]


maincontent : List (Attribute msg) -> List (Html msg) -> Html msg
maincontent =
    styled div
        [ displayFlex
        , maxHeight (Css.vh 100)
        , maxWidth (Css.vw 100)
        , height (Css.vh 100)
        , width (Css.vw 100)
        ]


nocntntdiv : List (Attribute msg) -> List (Html msg) -> Html msg
nocntntdiv =
    styled div
        [ displayFlex
        , flex (int 1)
        , alignItems center
        , justifyContent center
        , basicFont
        ]


pdivcol : List (Attribute msg) -> List (Html msg) -> Html msg
pdivcol =
    styled div
        [ displayFlex
        , flex (int 1)
        , maxWidth (Css.vw 10)
        , flexGrow (int 1)
        , flexDirection column
        , backgroundColor theme.primaryBack
        , padding (Css.em 0.2)
        , boxShadow4 (Css.em 0) (Css.em 0.4) (Css.em 0.5) (hex "000000")
        , zIndex (int 10)
        ]


btnStyle : Style
btnStyle =
    Css.batch
        [ justifyContent center
        , alignItems center
        , paddingBottom (Css.em 0.2)
        , paddingTop (Css.em 0.2)
        , paddingRight (Css.em 0.5)
        , paddingLeft (Css.em 0.5)
        , zIndex (int 10)
        , displayFlex
        , hover
            [ fontWeight bold
            , boxShadow4 (Css.em 0) (Css.em 0.3) (Css.em 0.4) (hex "000000")
            , zIndex (int 11)
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


tbtn : List (Attribute msg) -> List (Html msg) -> Html msg
tbtn =
    styled div
        [ backgroundColor theme.tertiary
        , basicFont
        , btnStyle
        , hover
            [ backgroundColor theme.tertiaryEmp
            ]
        ]



-- ProConListView --


proConListViewBody : List (Attribute msg) -> List (Html msg) -> Html msg
proConListViewBody =
    styled div
        [ flexDirection column
        , displayFlex
        , overflow hidden
        , width (Css.pc 100)
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
        , boxShadow4 (Css.em 0) (Css.em 0.2) (Css.em 0.3) (hex "000000")
        , zIndex (int 9)
        ]


proConListViewStatic : List (Attribute msg) -> List (Html msg) -> Html msg
proConListViewStatic =
    styled div
        [ marginLeft (Css.em 2)
        , flex (int 10)
        , minHeight (Css.em 1)
        ]


proConListViewEdit : List (Attribute msg) -> List (Html msg) -> Html msg
proConListViewEdit =
    styled textarea
        [ marginLeft (Css.em 2)
        , flex (int 10)
        , minHeight (Css.em 1)
        , resize none
        ]


proConListViewContent : List (Attribute msg) -> List (Html msg) -> Html msg
proConListViewContent =
    styled div
        [ flex (int 1)
        , displayFlex
        , overflowX scroll
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
        , overflowY scroll
        , minWidth (Css.em 40)
        , margin (Css.em 0.25)
        , justifyContent flexStart
        , boxShadow4 (Css.em 0) (Css.em 0.2) (Css.em 0.3) (hex "000000")
        , zIndex (int 8)
        ]


proConListTitleText : List (Attribute msg) -> List (Html msg) -> Html msg
proConListTitleText =
    styled div
        [ basicFont
        , minHeight (Css.em 3)
        , resize none
        , backgroundColor theme.tertiaryBack
        , displayFlex
        , alignItems center
        ]


proConListButton : List (Attribute msg) -> List (Html msg) -> Html msg
proConListButton =
    styled div
        [ backgroundColor theme.tertiary
        , basicFont
        , btnStyle
        , margin (Css.em 0.05)
        , flex (int 1)
        , hover
            [ backgroundColor theme.tertiaryEmp
            ]
        ]


proConListStatic : List (Attribute msg) -> List (Html msg) -> Html msg
proConListStatic =
    styled div
        [ width (Css.pc 100)
        , minHeight (Css.em 1)
        , justifyContent center
        , textAlign center
        ]


proConListEdit : List (Attribute msg) -> List (Html msg) -> Html msg
proConListEdit =
    styled textarea
        [ width (Css.pc 100)
        , minHeight (Css.em 1)
        , justifyContent center
        , textAlign center
        , resize none
        ]



-- CardList --


cardListBody : List (Attribute msg) -> List (Html msg) -> Html msg
cardListBody =
    styled div
        [ displayFlex
        , flex (int 1)
        , flexDirection column
        , margin (Css.em 0.2)
        , justifyContent flexStart
        , boxShadow4 (Css.em 0) (Css.em 0.3) (Css.em 0.4) (hex "000000")
        , zIndex (int 4)
        ]


cardListTitle : List (Attribute msg) -> List (Html msg) -> Html msg
cardListTitle =
    styled div
        [ basicFont
        , height (Css.em 1.5)
        , minHeight (Css.em 1.5)
        , resize none
        , backgroundColor theme.primaryEmp
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
        [ boxShadow4 (Css.em 0) (Css.em 0) (Css.em 0.5) (hex "000000")
        , displayFlex
        , margin (Css.em 0.25)
        , padding (Css.em 0.25)
        , height (Css.em 6)
        ]


cardButtonContainer : List (Attribute msg) -> List (Html msg) -> Html msg
cardButtonContainer =
    styled div
        [ backgroundColor theme.tertiary
        , basicFont
        , displayFlex
        , margin (Css.em 0.05)
        , justifyContent spaceBetween
        , flexDirection column
        ]


cardStatic : List (Attribute msg) -> List (Html msg) -> Html msg
cardStatic =
    styled div
        [ flex (int 10) ]


cardEdit : List (Attribute msg) -> List (Html msg) -> Html msg
cardEdit =
    styled textarea
        [ flex (int 10), resize none ]
