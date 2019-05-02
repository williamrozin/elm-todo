module Main exposing (Action(..), Model, TODO, init, main, update, view)

import Browser
import Css exposing (..)
import Html exposing (Html, button, div, h1, input, li, text, ul)
import Html.Attributes exposing (id, placeholder, style, value)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra exposing (onEnter)
import Html.Extra as Html


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias TODO =
    { done : Bool, content : String, id : String }


type alias Model =
    { text : String, todos : List TODO }


init : Model
init =
    { text = "", todos = [] }



-- UPDATE


type Action
    = Change String
    | ToggleDone String
    | AddTodo String


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

        AddTodo content ->
            if content == "" then
                { model | todos = model.todos }

            else
                { model | todos = TODO False content (generateID model) :: model.todos, text = "" }



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


renderTODO todo =
    li
        [ style "color" (getTextColor todo)
        , style "padding" "24px"
        , style "cursor" "pointer"
        , style "border" "1px solid silver"
        , style "border-radius" "4px"
        , style "margin-bottom" "24px"
        , style "background-color" (getBackgroundColor todo)
        , onClick (ToggleDone todo.id)
        ]
        [ text todo.content ]


renderList todos =
    todos
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
        , renderList model.todos
        ]
