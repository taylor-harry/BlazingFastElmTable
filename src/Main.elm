module Main exposing (main)

--imports

import Browser exposing (element)
import Color
import Dict exposing (Dict, map, toList)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Attribute, Html)
import Html.Attributes exposing (classList, disabled, style)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, andThen, bool, decodeString, fail, field, float, int, list, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import List
import Paginate exposing (..)
import Process
import String
import Task
import Time
import Data exposing (initDataList)


main : Program { height : Int, width : Int } Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



--MODEL


type alias Model =
    { width : Int
    , height : Int
    , viewport : { height : Int, width : Int }
    , page_data : PaginatedList Data
    , query : String
    , data : List Data
    , parentData : ParentData
    , list_success : String
    , search_toggle : SearchState
    , loading : LoadState
    , hidden_toggle : MenuState
    , reverse_first_name : ReverseState
    , reverse_last_name : ReverseState
    , reverse_email : ReverseState
    , reverse_fieldstreet_and_number : ReverseState
    , reverse_fieldlandline_number : ReverseState
    , reverse_avatar : ReverseState
    , raw_data : String
    }



--Initial data


init : { width : Int, height : Int } -> ( Model, Cmd Msg )
init flags =
    ( { width = 800
      , height = 800
      , viewport = flags
      , page_data = Paginate.fromList paginateNum initDataList
      , query = ""
      , data = initDataList
      , parentData = initParentData
      , list_success = ""
      , search_toggle = NO
      , loading = NotLoading
      , hidden_toggle = CLOSED
      , reverse_first_name = OFF
      , reverse_last_name = OFF
      , reverse_email = OFF
      , reverse_fieldstreet_and_number = OFF
      , reverse_fieldlandline_number = OFF
      , reverse_avatar = OFF
      , raw_data = ""
      }
    , Cmd.batch
        [ Cmd.none
        , getData "none"
        ]
    )



--State type definitions

type ReverseState
    = ON
    | OFF
    | STANDBY


type MenuState
    = OPEN
    | CLOSED


type SearchState
    = YES
    | NO


type LoadState
    = Loading
    | NotLoading


--GET requests & JSON decode

getData : String -> Cmd Msg
getData filt =
    Http.get
        { url = "https://reqres.in/api/users?page=2"
        , expect = Http.expectString UpdateList
        }


type alias Data =
    { avatar : String
    , email : String
    , last_name : String
    , first_name : String
    , id : String
    , hiddenToggleOpen : Bool
    }


decodeDataValue : Decoder Data
decodeDataValue =
    succeed Data
        |> required "avatar" string
        |> required "email" string
        |> required "last_name" string
        |> required "first_name" string
        |> optional "id" string ""
        |> optional "hiddenToggleOpen" bool False


decodeSupportValue : Decoder DataSupport
decodeSupportValue =
    succeed DataSupport
        |> required "url" string
        |> required "text" string


decodeParentDataValue : Decoder ParentData
decodeParentDataValue =
    succeed ParentData
        |> required "page" int
        |> required "per_page" int
        |> required "total" int
        |> required "total_pages" int
        |> optional "data" (list decodeDataValue) initDataList
        |> required "support" decodeSupportValue


type alias ParentData =
    { page : Int
    , per_page : Int
    , total : Int
    , total_pages : Int
    , data : List Data
    , support : DataSupport
    }


type alias DataSupport =
    { url : String
    , text : String
    }


type alias ListContainer =
    { data : List Data }


decodingData : String -> ParentData
decodingData listData =
    case decodeString decodeParentDataValue listData of
        Ok data ->
            data

        Err _ ->
            initParentDataFail




initDataSupport : DataSupport
initDataSupport =
    DataSupport "null" "null"


initParentData : ParentData
initParentData =
    ParentData 2 6 12 2 initDataList initDataSupport


initParentDataFail : ParentData
initParentDataFail =
    ParentData 2 6 12 2 initDataList initDataSupport



--MESSAGES


type Msg
    = NoOp
    | SetQuery String
    | Next
    | Prev
    | First
    | Last
    | GoTo Int
    | UpdateList (Result Http.Error String)
    | ToggleHidden String
    | SearchToggle String
    | DelayButtonSearch String
    | DelaySearch String
    | SearchOff
    | KeyDown
    | SortFirstName
    | SortLastName
    | SortEmail
    | SortAvatar



--UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateList (Ok listData) ->
            let
                parsed_data =
                    decodingData listData
            in
            ( { model | raw_data = listData, page_data = Paginate.fromList paginateNum parsed_data.data, data = parsed_data.data, parentData = parsed_data, list_success = "Raw data success" }, Cmd.none )

        UpdateList (Err e) ->
            ( { model | list_success = "No list data found" }, Cmd.none )

        DelayButtonSearch newQuery ->
            ( { model | loading = Loading }
            , Process.sleep searchButtonDelay |> Task.perform (always (SearchToggle newQuery))
            )

        DelaySearch newQuery ->
            ( model
            , Process.sleep searchDelay |> Task.perform (always (SetQuery newQuery))
            )

        SetQuery newQuery ->
            ( { model
                | query = newQuery
                , search_toggle = NO
                , page_data =
                    case model.search_toggle of
                        YES ->
                            Paginate.fromList paginateNum (List.filter (queryLogic model newQuery) model.data)

                        NO ->
                            Paginate.fromList paginateNum (List.filter (queryLogic model newQuery) model.data)
              }
            , Cmd.none
            )

        SearchToggle newQuery ->
            ( { model
                | search_toggle =
                    case model.search_toggle of
                        YES ->
                            YES

                        NO ->
                            YES
                , page_data = Paginate.fromList paginateNum (List.filter (queryLogic model newQuery) model.data)
                , loading = NotLoading
              }
            , Cmd.none
            )

        SearchOff ->
            ( { model | search_toggle = NO }
            , Cmd.none
            )

        GoTo index ->
            ( { model | page_data = Paginate.goTo index model.page_data }
            , Cmd.none
            )

        Next ->
            ( { model | page_data = Paginate.next model.page_data }
            , Cmd.none
            )

        Prev ->
            ( { model | page_data = Paginate.prev model.page_data }
            , Cmd.none
            )

        First ->
            ( { model | page_data = Paginate.first model.page_data }
            , Cmd.none
            )

        Last ->
            ( { model | page_data = Paginate.last model.page_data }
            , Cmd.none
            )

        KeyDown ->
            ( model, Process.sleep 0.0 |> Task.perform (always (DelayButtonSearch model.query)) )

        SortFirstName ->
            let
                sort =
                    if model.reverse_first_name == ON then
                        Paginate.fromList paginateNum
                            (List.reverse
                                (List.sortBy .first_name
                                    (List.filter (queryLogic model model.query) model.data)
                                )
                            )

                    else
                        Paginate.fromList paginateNum
                            (List.sortBy .first_name
                                (List.filter (queryLogic model model.query) model.data)
                            )

                sortData =
                    if model.reverse_first_name == ON then
                        List.reverse (List.sortBy .first_name model.data)

                    else
                        List.sortBy .first_name model.data
            in
            ( { model
                | page_data = sort
                , data = sortData
                , reverse_first_name =
                    case model.reverse_first_name of
                        ON ->
                            STANDBY

                        OFF ->
                            ON

                        STANDBY ->
                            ON
                , reverse_last_name = OFF
                , reverse_email = OFF
                , reverse_fieldstreet_and_number = OFF
                , reverse_fieldlandline_number = OFF
                , reverse_avatar = OFF
              }
            , Cmd.none
            )

        SortLastName ->
            let
                sort =
                    if model.reverse_last_name == ON then
                        Paginate.fromList paginateNum
                            (List.reverse
                                (List.sortBy .last_name
                                    (List.filter (queryLogic model model.query) model.data)
                                )
                            )

                    else
                        Paginate.fromList paginateNum
                            (List.sortBy .last_name
                                (List.filter (queryLogic model model.query) model.data)
                            )

                sortData =
                    if model.reverse_last_name == ON then
                        List.reverse (List.sortBy .last_name model.data)

                    else
                        List.sortBy .last_name model.data
            in
            ( { model
                | page_data = sort
                , data = sortData
                , reverse_first_name = OFF
                , reverse_last_name =
                    case model.reverse_last_name of
                        ON ->
                            STANDBY

                        OFF ->
                            ON

                        STANDBY ->
                            ON
                , reverse_email = OFF
                , reverse_avatar = OFF
              }
            , Cmd.none
            )

        SortEmail ->
            let
                sort =
                    if model.reverse_email == ON then
                        Paginate.fromList paginateNum
                            (List.reverse
                                (List.sortBy .email
                                    (List.filter (queryLogic model model.query) model.data)
                                )
                            )

                    else
                        Paginate.fromList paginateNum
                            (List.sortBy .email
                                (List.filter (queryLogic model model.query) model.data)
                            )

                sortData =
                    if model.reverse_email == ON then
                        List.reverse (List.sortBy .email model.data)

                    else
                        List.sortBy .email model.data
            in
            ( { model
                | page_data = sort
                , data = sortData
                , reverse_first_name = OFF
                , reverse_last_name = OFF
                , reverse_email =
                    case model.reverse_email of
                        ON ->
                            STANDBY

                        OFF ->
                            ON

                        STANDBY ->
                            ON
                , reverse_avatar = OFF
              }
            , Cmd.none
            )

        ToggleHidden item_id ->
            let
                updateToggle record =
                    if record.id == item_id then
                        { record | hiddenToggleOpen = not record.hiddenToggleOpen }

                    else
                        record

                toggleData =
                    Paginate.map
                        (\_ ->
                            List.map updateToggle
                                (List.filter
                                    (queryLogic model model.query)
                                    model.data
                                )
                        )
                        model.page_data

                listData =
                    List.map updateToggle model.data
            in
            ( { model | page_data = toggleData, data = listData }
            , Cmd.none
            )

        SortAvatar ->
            let
                sort =
                    if model.reverse_avatar == ON then
                        Paginate.fromList paginateNum
                            (List.reverse
                                (List.sortBy .avatar
                                    (List.filter (queryLogic model model.query) model.data)
                                )
                            )

                    else
                        Paginate.fromList paginateNum
                            (List.sortBy .avatar
                                (List.filter (queryLogic model model.query) model.data)
                            )

                sortData =
                    if model.reverse_avatar == ON then
                        List.reverse (List.sortBy .avatar model.data)

                    else
                        List.sortBy .avatar model.data
            in
            ( { model
                | page_data = sort
                , data = sortData
                , reverse_first_name = OFF
                , reverse_last_name = OFF
                , reverse_email = OFF
                , reverse_avatar =
                    case model.reverse_avatar of
                        ON ->
                            STANDBY

                        OFF ->
                            ON

                        STANDBY ->
                            ON
              }
            , Cmd.none
            )


--Query logic

searchButtonDelay =
    0.0

searchDelay =
    0.0

searchCharDelay =
    2

paginateNum =
    20

dataGetters =
    [ .avatar
    , .email
    , .last_name
    , .first_name
    ]

queryLogic : Model -> String -> Data -> Bool
queryLogic model str data =
    let
        
        query =
            String.toLower str

        values =
            dataGetters |> List.map (\g -> g data)

    in
    values |> List.map (String.contains query) |> List.foldl (||) False



--VIEW


view model =
    layout [ Background.color (rgb255 177 194 222) ] <|
        tableView model




tableView : Model -> Element Msg
tableView model =
    let
        displayInfoView =
            el
                [ centerX
                , Font.size 14
                , Font.color (rgb255 102 102 102)
                , padding 8
                , Background.color (rgb255 225 225 225)
                , Border.rounded 6
                ]
                (Element.text
                    (String.join " "
                        [ String.fromInt <| Paginate.currentPage (model.page_data)
                        , "of"
                        , String.fromInt <| Paginate.totalPages (model.page_data)
                        ]
                    )
                )

        buttonStyling =
            [ style "font-size" "10px"
            , style "border-color" "#DDDDDD"
            , style "box-shadow" "0px 0px"
            , style "padding" "10px 14px 10px 14px"
            , style "background-color" "white"
            , style "border-width" "1px"
            , style "cursor" "pointer"
            ]

        prevButtons =
            Element.row
                []
                [ Element.html
                    (Html.button
                        (List.append buttonStyling
                            [ style "border-top-left-radius" "5px"
                            , style "border-bottom-left-radius" "5px"
                            , style "cursor" "pointer"
                            , style "border-width" "0px"
                            , style "font-weight" "dark grey"
                            , style "color" "black"
                            , onClick First
                            , disabled <| Paginate.isFirst (model.page_data)
                            ]
                        )
                        [ Html.text "FIRST" ]
                    )
                , Element.html
                    (Html.button
                        (List.append buttonStyling
                            [ style "cursor" "pointer"
                            , style "font-weight" "dark grey"
                            , style "border-width" "0px"
                            , style "color" "black"
                            , onClick Prev
                            , disabled <| Paginate.isFirst (model.page_data)
                            ]
                        )
                        [ Html.text "PREV" ]
                    )
                ]

        nextButtons =
            Element.row
                []
                [ Element.html
                    (Html.button
                        (List.append buttonStyling
                            [ style "font-weight" "dark grey"
                            , style "border-width" "0px"
                            , style "color" "black"
                            , onClick Next
                            , disabled <| Paginate.isLast (model.page_data)
                            ]
                        )
                        [ Html.text "NEXT" ]
                    )
                , Element.html
                    (Html.button
                        (List.append buttonStyling
                            [ style "border-top-right-radius" "5px"
                            , style "border-bottom-right-radius" "5px"
                            , style "border-width" "0px"
                            , style "font-weight" "dark grey"
                            , style "color" "black"
                            , onClick Last
                            , disabled <| Paginate.isLast (model.page_data)
                            ]
                        )
                        [ Html.text "LAST" ]
                    )
                ]

        pagerButtonView index isActive =
            Element.html
                (Html.button
                    [ style "font-weight"
                        (if isActive then
                            "bold"

                         else
                            "normal"
                        )
                    , onClick <| GoTo index
                    ]
                    [ Html.text <| String.fromInt index ]
                )

        searchButton =
            let
                icon =
                    Element.el [ Font.size 14, Background.color (rgb255 67 130 222) ] (text "Search")
            in
            Element.el
                [ Background.color (rgb255 67 130 222)
                , paddingEach { top = 10, bottom = 10, left = 9, right = 9 }
                , Font.color (rgb 1 1 1)
                , Events.onClick (DelayButtonSearch model.query)
                , Font.size 22
                , padding 4
                ]
                icon

        eraseButton =
            let
                icon =
                    fa "fas fa-times"
            in
            Element.el
                [ paddingEach { top = 10, bottom = 10, left = 9, right = 9 }
                , Events.onClick (SetQuery "")
                , Font.size 16
                ]
                icon

        searchBox =
            row
                [ centerX
                , pointer
                , centerY
                , height (px 36)
                ]
                [ Element.el
                    [ Element.inFront <|
                        Element.el
                            [ alignRight
                            , moveDown 5
                            ]
                            eraseButton
                    , moveUp 2.5
                    ]
                    (Input.text
                        [ height (px 31)
                        , Border.rounded 0
                        , Font.size 16
                        , Border.rounded 6
                        , Border.color (rgb255 196 196 196)
                        , width (px 100)
                        , Border.widthEach
                            { bottom = 1
                            , left = 1
                            , right = 1
                            , top = 1
                            }
                        , paddingEach { top = 12, bottom = 0, left = 0, right = 30 }
                        ]
                        { onChange = SetQuery
                        , text = model.query
                        , placeholder = Nothing
                        , label = Input.labelAbove [] (text "")
                        }
                    )
                , Element.el [ padding 5 ] searchButton
                ]

        pageNavigation =
            if List.length (Paginate.page model.page_data) == 0 then
                Element.el [] Element.none

            else
                Element.column [ centerX ]
                    [ Element.row
                        [ centerX, paddingXY 0 20, alignBottom ]
                        [ prevButtons
                        , nextButtons
                        ]
                    , displayInfoView
                    ]

        responsiveSearch =
            if model.viewport.width <= 700 then
                Element.column
                    [ centerX, spacing 5 ]
                    [ searchBox ]

            else
                Element.row
                    [ centerX, spacing 5, padding 10 ]
                    [ searchBox ]
    in
    column
        [ centerX
        , width fill
        , padding 20
        , Background.color (rgb255 177 194 222)
        , Font.color (rgb255 57 76 128)
        ]
        [ responsiveSearch
        , table model
        , pageNavigation
        ]


table : Model -> Element Msg
table model =
    let
        sortIconFirstName =
            if model.reverse_first_name == ON then
                fa "fas fa-sort-amount-up"

            else if model.reverse_first_name == STANDBY then
                fa "fas fa-sort-amount-down"

            else
                fa "fas fa-sort"

        sort_icon_last_name =
            if model.reverse_last_name == ON then
                fa "fas fa-sort-amount-up"

            else if model.reverse_last_name == STANDBY then
                fa "fas fa-sort-amount-down"

            else
                fa "fas fa-sort"

        sort_icon_email =
            if model.reverse_email == ON then
                 fa "fas fa-sort-amount-up"

            else if model.reverse_email == STANDBY then
                fa "fas fa-sort-amount-down"

            else
                fa "fas fa-sort"

        sort_icon_avatar =
            if model.reverse_avatar == ON then
                fa "fas fa-sort-amount-up"

            else if model.reverse_avatar == STANDBY then
                fa "fas fa-sort-amount-down"

            else
                fa "fas fa-sort"

        responsive_columns =
            if model.viewport.width <= 700 then
                List.take 2 mainColumns

            else
                mainColumns

        hidden_icon item =
            if model.viewport.width <= 700 then
                if item.hiddenToggleOpen == False then
                    Element.el [ paddingXY 4 2, Events.onClick (ToggleHidden item.id), Font.color  (rgb255 57 76 128) ] (fa "fas fa-chevron-down")

                else
                    Element.el [ paddingXY 4 2, Events.onClick (ToggleHidden item.id), Font.color  (rgb255 57 76 128) ] (fa "fas fa-chevron-up")

            else
                Element.el [] Element.none

        hidden_columns data =
            if model.viewport.width <= 700 then
                if data.hiddenToggleOpen == True then
                    [ Element.row [ padding 2, width fill, alignBottom ]
                        [ Element.column [ Font.bold, Font.size 13, Font.color (rgb255 102 102 102), paddingXY 1 0 ]
                            [ Element.text "email"
                            , Element.text "avatar"
                            ]
                        ]
                    ]

                else
                    [ Element.el [] Element.none ]

            else
                [ Element.el [] Element.none ]

        hidden_column_data item =
            if item.hiddenToggleOpen == True then
                Element.column [ padding 2, height (fill |> minimum 16), Font.size 13, Font.color (rgb255 102 102 102), width fill ]
                    [ Element.text item.email
                    , Element.text item.avatar
                    ]

            else
                Element.el [] Element.none

        mainColumns =
            [ { header = Element.row [ Events.onClick SortFirstName, Border.widthEach { bottom = 1, left = 0, right = 0, top = 1 }, Border.color (rgb255 231 234 236), paddingEach { top = 8, bottom = 8, left = 0, right = 0 } ] [ Element.el [] <| text "first name  ", sortIconFirstName ]
              , width = fill
              , view =
                    \item ->
                        Element.column [ spacing 5, height (fill |> minimum 15), Font.light, Font.size 13, Font.color  (rgb255 57 76 128), Border.color (rgb255 231 234 236), paddingEach { top = 8, bottom = 8, left = 0, right = 0 }, Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 } ]
                            (List.concat
                                [ [ Element.row [ padding 2, spacing 4 ]
                                        [ hidden_icon item
                                        ,  Element.text item.first_name
                                        ]
                                  ]
                                , hidden_columns item
                                ]
                            )
              }
            , { header = Element.row [ Events.onClick SortLastName, Border.widthEach { bottom = 1, left = 0, right = 0, top = 1 }, Border.color (rgb255 231 234 236), paddingEach { top = 8, bottom = 8, left = 0, right = 0 } ] [ Element.el [] <| text "last name  ", sort_icon_last_name ]
              , width = fill
              , view =
                    \item ->
                        Element.column [ spacing 4, height (fill |> minimum 15), Font.light, Font.size 13, Font.color (rgb255 57 76 128), Border.color (rgb255 231 234 236), paddingEach { top = 8, bottom = 8, left = 0, right = 0 }, Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 } ]
                            [ Element.text item.last_name 
                            , hidden_column_data item
                            ]
              }
            , { header = Element.row [ Events.onClick SortEmail, Border.widthEach { bottom = 1, left = 0, right = 0, top = 1 }, Border.color (rgb255 231 234 236), paddingEach { top = 8, bottom = 8, left = 0, right = 0 } ] [ Element.el [] <| text "email  ", sort_icon_email ]
              , width = fill
              , view =
                    \item ->
                        Element.column [ height (fill |> minimum 15), Font.light, Font.size 13, Font.color  (rgb255 57 76 128), Border.color (rgb255 231 234 236), paddingEach { top = 8, bottom = 8, left = 0, right = 0 }, Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 } ]
                            [ Element.text item.email 
                            ]
              }
            , { header = Element.row [ Events.onClick SortAvatar, Border.widthEach { bottom = 1, left = 0, right = 0, top = 1 }, Border.color (rgb255 231 234 236), paddingEach { top = 8, bottom = 8, left = 0, right = 0 } ] [ Element.el [] <| text "avatar  ", sort_icon_avatar ]
              , width = fill
              , view =
                    \item ->
                        Element.column [ height (fill |> minimum 15), Font.light, Font.size 13, Font.color (rgb255 57 76 128), Border.color (rgb255 231 234 236), paddingEach { top = 8, bottom = 8, left = 0, right = 0 }, Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 } ]
                            [ Element.text item.avatar 
                            ]
              }
            ]
    in
    Element.table
        [ centerX
        , centerY
        , spacing 0
        , Font.bold
        , Font.size 13
        , Font.color  (rgb255 57 76 128)
        , Background.color (rgb255 255 255 255)
        , alignLeft
        , paddingXY 16 16
        , Border.rounded 6
        ]
        { data = Paginate.page (model.page_data)
        , columns = responsive_columns
        }

-- Font Awesome hack
fa : String -> Element msg
fa faClasses =
    Element.html <|
        Html.i
            [ classList <|
                List.map (\s -> ( s, True )) <|
                    String.split " " faClasses
            ]
            []
