module Main exposing (Action(..), Model, TODO, init, main, update, view)

import Browser
import Css exposing (..)
import FeatherIcons
import Html exposing (Html, button, div, h1, input, li, option, select, text, ul)
import Html.Attributes exposing (id, placeholder, style, title, value)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra exposing (onEnter)
import Html.Extra as Html


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias TODO =
    { deleted : Bool, done : Bool, content : String, id : String }


type alias Model =
    { text : String, todos : List TODO, filter : String }


init : Model
init =
    { text = "", todos = [], filter = "3" }



-- UPDATE


type Action
    = Change String
    | ToggleDone String
    | AddTodo String
    | ToggleDelete String
    | ChangeFilter String


generateID : Model -> String
generateID model =
    String.fromInt (List.length model.todos)


update : Action -> Model -> Model
update action model =
    case action of
        Change newValue ->
            { model | text = newValue }

        ToggleDone id ->
            let
                updateTodo : TODO -> TODO
                updateTodo todo =
                    if todo.id == id then
                        if todo.done then
                            { todo | done = False }

                        else
                            { todo | done = True }

                    else
                        todo

                todos =
                    List.map updateTodo model.todos
            in
            { model | todos = todos }

        ToggleDelete id ->
            let
                deleteTodo : TODO -> TODO
                deleteTodo todo =
                    if todo.id == id then
                        { todo | deleted = todo.deleted == False }

                    else
                        { todo | deleted = todo.deleted }

                todos =
                    List.map deleteTodo model.todos
            in
            { model | todos = todos }

        ChangeFilter option ->
            { model | filter = option }

        AddTodo content ->
            if content == "" then
                { model | todos = model.todos }

            else
                { model | todos = TODO False False content (generateID model) :: model.todos, text = "" }



-- VIEW


getTextColor : TODO -> String
getTextColor todo =
    if todo.done then
        "green"

    else
        ""


getBackgroundColor : TODO -> String
getBackgroundColor todo =
    if todo.done then
        "#E8FFDA"

    else
        "#F9F9F9"


filterTODO todo filter =
    if filter == "3" then
        (todo.done == False) && (todo.deleted == False)

    else if filter == "0" then
        todo.deleted == True

    else if filter == "1" then
        todo.done == True

    else
        True


getToggleDoneTitle todo =
    if todo.done then
        "Mark as undone"

    else
        "Mark as done"


getToggleDeletedTitle todo =
    if todo.deleted then
        "Restore"

    else
        "Delete"


renderButtonAdd description =
    button
        [ onClick (AddTodo description)
        , style "padding" "12px"
        , style "border" "1px solid cornflowerblue"
        , style "color" "cornflowerblue"
        , style "border-top-right-radius" "4px"
        , style "border-bottom-right-radius" "4px"
        , style "background-color" "#F6F6FF"
        , style "cursor" "pointer"
        , style "font-size" "18px"
        , style "text-transform" "uppercase"
        ]
        [ text "Add TODO" ]


renderTextField description =
    input
        [ placeholder "What do you need to do?"
        , onInput Change
        , onEnter (AddTodo description)
        , value description
        , style "padding" "12px"
        , style "border" "1px solid cornflowerblue"
        , style "border-top-left-radius" "4px"
        , style "border-bottom-left-radius" "4px"
        , style "border-right" "none"
        , style "flex" "1"
        , style "font-size" "18px"
        ]
        []


renderInput text =
    div
        [ style "padding" "24px 0"
        , style "display" "flex"
        , style "width" "100%"
        , style "max-width" "720px"
        ]
        [ renderTextField text
        , renderButtonAdd text
        ]


renderDeleteIcon todo =
    if todo.deleted then
        FeatherIcons.refreshCw
            |> FeatherIcons.toHtml []

    else
        FeatherIcons.trash
            |> FeatherIcons.toHtml []


renderDoneIcon todo =
    if todo.done then
        FeatherIcons.clock
            |> FeatherIcons.toHtml []

    else
        FeatherIcons.check
            |> FeatherIcons.toHtml []


renderTODO todo =
    li
        [ style "color" (getTextColor todo)
        , style "padding" "24px"
        , style "cursor" "pointer"
        , style "border" "1px solid silver"
        , style "border-radius" "4px"
        , style "margin-bottom" "24px"
        , style "display" "flex"
        , style "align-items" "center"
        , style "background-color" (getBackgroundColor todo)
        ]
        [ div
            [ style "flex" "1"
            , style "font-size" "18px"
            ]
            [ text todo.content ]
        , button
            [ style "color" "red"
            , style "background-color" "transparent"
            , style "border" "none"
            , style "cursor" "pointer"
            , style "text-align" "right"
            , title (getToggleDeletedTitle todo)
            , onClick (ToggleDelete todo.id)
            ]
            [ renderDeleteIcon todo
            ]
        , button
            [ style "color" "green"
            , style "background-color" "transparent"
            , style "border" "none"
            , style "cursor" "pointer"
            , style "text-align" "right"
            , title (getToggleDoneTitle todo)
            , onClick (ToggleDone todo.id)
            ]
            [ renderDoneIcon todo
            ]
        ]


renderFilter filter =
    select
        [ style "position" "fixed"
        , style "bottom" "24px"
        , style "right" "24px"
        , onInput ChangeFilter
        ]
        [ option [ value "3" ] [ text "To do" ]
        , option [ value "2" ] [ text "All" ]
        , option [ value "1" ] [ text "Done" ]
        , option [ value "0" ] [ text "Deleted" ]
        ]


renderList todos filter =
    todos
        |> List.filter
            (\todo -> filterTODO todo filter)
        |> List.map
            (\todo -> renderTODO todo)
        |> ul
            [ style "margin" "0"
            , style "width" "100%"
            , style "max-width" "720px"
            , style "list-style-type" "none"
            , style "padding" "0"
            ]


renderTitle =
    h1
        [ style "color" "cornflowerblue"
        , style "font-size" "62px"
        , style "font-weight" "100"
        , style "margin" "0"
        ]
        [ text "TODO List" ]


view : Model -> Html Action
view model =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "center"
        , style "justify-content" "center"
        , style "padding" "48px"
        , style "font-family" "sans-serif"
        ]
        [ renderTitle
        , renderInput model.text
        , renderFilter model.filter
        , renderList model.todos model.filter
        ]
