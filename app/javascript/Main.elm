module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onInput)


-- MAIN


main : Program Never Model Message
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { uri : String }



-- INIT


init : ( Model, Cmd Message )
init =
    ( Model "", Cmd.none )



-- VIEW


view : Model -> Html Message
view model =
    input [ onInput UpdateUri ] []



-- MESSAGE


type Message
    = UpdateUri String



-- UPDATE


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        UpdateUri uri ->
            ( {model | uri = uri}, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Message
subscriptions model =
    Sub.none
