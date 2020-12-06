module ProConListView exposing (..)

import Array exposing (Array)
import Card.Card exposing (CardModel, CardMsg(..))
import Card.CardList exposing (CardListMsg(..))
import Chart exposing (pie)
import ChartColumn exposing (..)
import Css exposing (displayFlex)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Json.Decode as D exposing (Decoder, bool, field, int, map4, string)
import Json.Encode as E exposing (..)
import ProConList exposing (..)
import Styling exposing (..)
import Svg exposing (..)
import WithId exposing (..)
import WithText exposing (..)


type alias ProConListViewModel =
    WithId (WithText (ProConListViewData {}))


type alias ProConListViewData a =
    { a | proConLists : Array ProConListModel, viewMode : ViewMode }


cons : Int -> String -> Bool -> Array ProConListModel -> ProConListViewModel
cons id text edit lists =
    { id = id, text = text, edit = edit, proConLists = lists, viewMode = Lists }


init : Int -> ProConListViewModel
init id =
    cons id "Title Here..." False Array.empty


proConListViewDecoder : Decoder ProConListViewModel
proConListViewDecoder =
    map4 cons
        (field "id" D.int)
        (field "title" D.string)
        (field "edit" D.bool)
        (field "pro_con_lists" (D.array proConListDecoder))


proConListViewEncoder : ProConListViewModel -> E.Value
proConListViewEncoder model =
    E.object
        [ ( "edit", E.bool model.edit )
        , ( "id", E.int model.id )
        , ( "pro_con_lists", E.array proConListEncoder model.proConLists )
        , ( "title", E.string model.text )
        ]


keyEncoder : ( Int, Int, Int ) -> String
keyEncoder ( a, b, c ) =
    String.fromInt a ++ ":" ++ String.fromInt b ++ ":" ++ String.fromInt c


type ViewMode
    = Lists
    | GlobalSort
    | Graph


type ProConListViewMsg
    = ToProConList Int ProConListMsg
    | TextAction WithTextAction
    | WithIdAction WithIdAction
    | ActOnLists WithIdAction
    | ConfirmFirst String ProConListViewMsg
    | SetView ViewMode
    | GlobalActOnCards Int Int Int Int WithIdAction


update : ProConListViewModel -> ProConListViewMsg -> ( ProConListViewModel, Cmd ProConListViewMsg )
update model msg =
    case msg of
        ToProConList id pcmsg ->
            let
                ( newArr, nmsg ) =
                    updateId model.proConLists id pcmsg ProConList.update ToProConList Cmd.none
            in
            ( { model | proConLists = newArr }, nmsg )

        ActOnLists action ->
            let
                newArr =
                    updateHasIdArray model.proConLists action ProConList.init
            in
            ( { model | proConLists = newArr }, Cmd.none )

        TextAction action ->
            ( updateWithText model action, Cmd.none )

        WithIdAction _ ->
            ( model, Cmd.none )

        ConfirmFirst _ _ ->
            ( model, Cmd.none )

        SetView mode ->
            ( { model | viewMode = mode }, Cmd.none )

        GlobalActOnCards proConId proOrCon cardId rank action ->
            case action of
                -- So this is a little hairy, we want to edit the card alllll the way in the bottom.
                Move id dir ->
                    let
                        proConList =
                            Array.get proConId model.proConLists

                        subList =
                            case proConList of
                                Nothing ->
                                    Nothing

                                Just aList ->
                                    case proOrCon of
                                        0 ->
                                            Just aList.proList

                                        _ ->
                                            Just aList.conList

                        card =
                            case subList of
                                Nothing ->
                                    Nothing

                                Just cList ->
                                    Array.get cardId cList.cards

                        allThree =
                            ( proConList, subList, card )
                    in
                    case allThree of
                        ( Just pcl, Just cl, Just c ) ->
                            let
                                newCard =
                                    case dir of
                                        Up ->
                                            { c | rank = Basics.max (c.rank + 1) 1 }

                                        Down ->
                                            { c | rank = Basics.max (c.rank - 1) 1 }

                                newList =
                                    { cl | cards = Array.set cardId newCard cl.cards }

                                newProCon =
                                    case proOrCon of
                                        0 ->
                                            { pcl | proList = newList }

                                        _ ->
                                            { pcl | conList = newList }
                            in
                            ( { model | proConLists = Array.set proConId newProCon model.proConLists }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )


view : ProConListViewModel -> (Int -> ProConListViewMsg -> msg) -> Html msg
view model lift =
    let
        pclvLift =
            lift model.id
    in
    proConListViewBody
        []
        [ proConListViewTitleContainer
            []
            [ sbtn [ SetView Lists |> pclvLift |> onClick ] [ Html.Styled.text "📝" ]
            , sbtn [ SetView GlobalSort |> pclvLift |> onClick ] [ Html.Styled.text "📑" ]
            , sbtn [ SetView Graph |> pclvLift |> onClick ] [ Html.Styled.text "📊" ]
            , TextAction
                >> pclvLift
                |> getClickableTextArea proConListViewStatic proConListViewEdit model
            , WithIdAction
                >> pclvLift
                |> getButton sbtn False (Move model.id Up)
            , WithIdAction
                >> pclvLift
                |> getButton sbtn False (Move model.id Down)
            , WithIdAction
                >> ConfirmFirst "Are you sure you want to delete this ProConListView?"
                >> pclvLift
                |> getButton sbtn False (Delete model.id)
            ]
        , case model.viewMode of
            Lists ->
                listsView model pclvLift

            GlobalSort ->
                globalSortView model pclvLift

            Graph ->
                graphView model pclvLift
        ]


listsView : ProConListViewModel -> (ProConListViewMsg -> msg) -> Html msg
listsView model lift =
    let
        buildListView list =
            Html.Styled.map lift (ProConList.view list liftProConMsg)
    in
    (Array.toList model.proConLists |> List.map buildListView)
        ++ [ ActOnLists
                >> lift
                |> getButton pbtn False Add
           ]
        |> proConListViewContent []


liftProConMsg : Int -> ProConListMsg -> ProConListViewMsg
liftProConMsg id listmsg =
    case listmsg of
        ProConAction action ->
            ActOnLists action

        ProConList.ConfirmFirst str msg ->
            ConfirmFirst str (liftProConMsg id msg)

        _ ->
            ToProConList id listmsg


globalSortView : ProConListViewModel -> (ProConListViewMsg -> msg) -> Html msg
globalSortView model lift =
    let
        sortCards card =
            card.card.rank

        proList =
            List.map (flatMapLists 0) (Array.toList model.proConLists)
                |> List.concat
                |> List.sortBy sortCards
                |> List.reverse

        conList =
            List.map (flatMapLists 1) (Array.toList model.proConLists)
                |> List.concat
                |> List.sortBy sortCards
                |> List.reverse
    in
    simpleScrollableFlexDiv []
        [ cardListBody []
            [ cardListTitle [] [ Html.Styled.text "👍" ]
            , proList |> List.map (buildCardView lift) |> cardListCardContainer []
            ]
        , cardListBody []
            [ cardListTitle [] [ Html.Styled.text "👎" ]
            , conList |> List.map (buildCardView lift) |> cardListCardContainer []
            ]
        ]


type alias GlobalCard =
    { proConListId : Int, proOrCon : Int, card : CardModel }


flatMapLists : Int -> ProConListModel -> List GlobalCard
flatMapLists proOrCon proConList =
    let
        cardlist =
            case proOrCon of
                0 ->
                    proConList.proList |> .cards |> Array.toList

                _ ->
                    proConList.conList |> .cards |> Array.toList

        mapList card =
            { proConListId = proConList.id, proOrCon = proOrCon, card = card }
    in
    List.map mapList cardlist


buildCardView : (ProConListViewMsg -> msg) -> GlobalCard -> Html msg
buildCardView lift { proConListId, proOrCon, card } =
    div []
        [ simplePrimaryTitle [] [ Html.Styled.text (String.fromInt card.rank) ]
        , liftCardMsg proConListId proOrCon card.rank
            |> Card.Card.view card False
            |> Html.Styled.map lift
        ]


liftCardMsg : Int -> Int -> Int -> Int -> CardMsg -> ProConListViewMsg
liftCardMsg proConId proOrCon rank cardId cardMsg =
    case cardMsg of
        Card.Card.CardAction action ->
            GlobalActOnCards proConId proOrCon cardId rank action

        _ ->
            cardMsg |> ToCard cardId |> ToList proOrCon |> ToProConList proConId


graphView : ProConListViewModel -> (ProConListViewMsg -> msg) -> Html msg
graphView model lift =
    let
        posData =
            { data = model.proConLists |> Array.toList |> List.map (extractTitleAndTotal 0), title = "Share of Total Pro" }

        negData =
            { data = model.proConLists |> Array.toList |> List.map (extractTitleAndTotal 1), title = "Share of Total Con" }
    in
    graphPageContainer [] [ chartContainer [] [ ChartColumn.view posData, ChartColumn.view negData ] ]


extractTitleAndTotal : Int -> ProConListModel -> ( String, Int )
extractTitleAndTotal proOrCon model =
    let
        list =
            case proOrCon of
                0 ->
                    model.proList.cards

                _ ->
                    model.conList.cards
    in
    ( model.text, list |> Array.toList |> List.map .rank |> List.sum )
