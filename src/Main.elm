module Main exposing (Action(..), Model, TODO, init, main, update, view)

import Browser
import Html exposing (Html, button, div, input, li, text, ul)
import Html.Attributes exposing (id, placeholder, style, value)
import Html.Events exposing (onClick, onInput)


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
            { model | todos = TODO False content (generateID model) :: model.todos, text = "" }



-- VIEW


getTextStyle : TODO -> String
getTextStyle l =
    if l.done then
        "line-through"

    else
        ""


getTextColor : TODO -> String
getTextColor l =
    if l.done then
        "green"

    else
        ""


renderList lst =
    lst
        |> List.map
            (\l ->
                li
                    [ style "text-decoration" (getTextStyle l)
                    , style "color" (getTextColor l)
                    , onClick (ToggleDone l.id)
                    ]
                    [ text l.content ]
            )
        |> ul []


view : Model -> Html Action
view model =
    div []
        [ input [ placeholder "Describe your next TODO", onInput Change, value model.text ] []
        , button [ onClick (AddTodo model.text) ] [ text "Add TODO" ]
        , renderList model.todos
        ]
