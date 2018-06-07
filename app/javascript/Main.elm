module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Http exposing (get, send)
import Json.Decode as Decode


-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { uri : String
    , captions : List Caption
    , errorMessage : String
    }


type alias Caption =
    { time : Float
    , caption : String
    }



-- INIT


init : ( Model, Cmd Msg )
init =
    ( Model "" [] "", Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ onInput UpdateUri ] []
        , button [ onClick FetchCaptions ] [ text <| "Submit" ]
        ]



-- MESSAGE


type Msg
    = UpdateUri String
    | FetchCaptions
    | NewCaptions (Result Http.Error (List Caption))



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        UpdateUri uri ->
            ( { model | uri = uri }, Cmd.none )

        FetchCaptions ->
            ( model, fetchCaptions model.uri )

        NewCaptions (Ok newCaptions) ->
            ( { model | captions = newCaptions }, Cmd.none )

        NewCaptions (Err message) ->
            ( { model | errorMessage = errorMessage message }, Cmd.none )


errorMessage : Http.Error -> String
errorMessage message =
    case message of
        Http.BadPayload msg _ ->
            msg

        _ ->
            "Some other error"


fetchCaptions : String -> Cmd Msg
fetchCaptions uri =
    let
        url =
            "http://localhost:3000/api/transcript?uri=" ++ uri

        request =
            Http.get url decodeCaptionJson
    in
    Http.send NewCaptions request


decodeCaptionJson : Decode.Decoder (List Caption)
decodeCaptionJson =
    Decode.list captionDecoder

captionDecoder : Decode.Decoder Caption
captionDecoder = 
    Decode.map2 Caption 
      (Decode.field "time" Decode.float)
      (Decode.field "text" Decode.string)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
