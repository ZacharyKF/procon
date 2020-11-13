module Styling exposing (..)

import Css exposing (..)
import Html
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
    { primaryBack = hex "ff8000"
    , primary = hex "ff8c1a"
    , primaryEmp = hex "ff9933"
    , secondaryBack = hex "ff0055"
    , secondary = hex "ff1a66"
    , secondaryEmp = hex "ff3377"
    , tertiaryBack = hex "8585ad"
    , tertiary = hex "9494b8"
    , tertiaryEmp = hex "a3a3c2"
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


pclistview : List (Attribute msg) -> List (Html msg) -> Html msg
pclistview =
    styled div
        [ flexDirection column
        , displayFlex
        , overflow hidden
        , width (Css.pc 100)
        , maxWidth (Css.vw 90)
        , justifyContent flexStart
        , flex (int 1)
        ]


pctitle : List (Attribute msg) -> List (Html msg) -> Html msg
pctitle =
    styled div
        [ height (Css.em 2)
        , backgroundColor theme.secondaryBack
        , displayFlex
        , alignItems center
        , justifyContent spaceBetween
        , boxShadow4 (Css.em 0) (Css.em 0.2) (Css.em 0.3) (hex "000000")
        , zIndex (int 9)
        ]


pctitletext : List (Attribute msg) -> List (Html msg) -> Html msg
pctitletext =
    styled div
        [ basicFont
        , marginLeft (Css.em 2)
        , height (Css.em 2)
        , resize none
        , displayFlex
        , alignItems center
        ]


pcltitletext : List (Attribute msg) -> List (Html msg) -> Html msg
pcltitletext =
    styled div
        [ basicFont
        , height (Css.em 3)
        , minHeight (Css.em 3)
        , resize none
        , backgroundColor theme.tertiaryBack
        , displayFlex
        , alignItems center
        , justifyContent center
        ]


pccontent : List (Attribute msg) -> List (Html msg) -> Html msg
pccontent =
    styled div
        [ flex (int 1)
        , displayFlex
        , overflowX scroll
        , overflowY hidden
        , flexDirection row
        , padding (Css.em 0.25)
        , zIndex (int 7)
        ]


pcl : List (Attribute msg) -> List (Html msg) -> Html msg
pcl =
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


cl : List (Attribute msg) -> List (Html msg) -> Html msg
cl =
    styled div
        [ displayFlex
        , flex (int 1)
        , flexDirection column
        , margin (Css.em 0.2)
        , justifyContent flexStart
        , boxShadow4 (Css.em 0) (Css.em 0.3) (Css.em 0.4) (hex "000000")
        , zIndex (int 4)
        ]


ctitle : List (Attribute msg) -> List (Html msg) -> Html msg
ctitle =
    styled div
        [ basicFont
        , height (Css.em 1.2)
        , minHeight (Css.em 1.2)
        , resize none
        , backgroundColor theme.primaryEmp
        , displayFlex
        , alignItems center
        , justifyContent center
        ]


card : List (Attribute msg) -> List (Html msg) -> Html msg
card =
    styled div
        [ boxShadow4 (Css.em 0) (Css.em 0) (Css.em 0.5) (hex "000000")
        , displayFlex
        , margin (Css.em 0.25)
        , padding (Css.em 0.25)
        , height (Css.em 6)
        ]
