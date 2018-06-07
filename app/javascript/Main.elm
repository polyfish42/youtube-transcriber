module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (attribute, src)
import Html.Events exposing (onClick, onInput)
import Http exposing (get, send)
import Json.Decode exposing (Decoder, field, float, list, map2, string)
import Regex exposing (find, regex)


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
    , text : String
    }



-- INIT


init : ( Model, Cmd Msg )
init =
    ( Model "" [] "", Cmd.none )



-- VIEW
-- <iframe width="560" height="315" src="https://www.youtube.com/embed/7Tx4PXDW35g" frameborder="0" allow="autoplay; encrypted-media" allowfullscreen></iframe>


view : Model -> Html Msg
view model =
    div []
        [ p [] [ text model.errorMessage ]
        , viewVideo model.uri
        , input [ onInput UpdateUri ] []
        , button [ onClick FetchCaptions ] [ text <| "Submit" ]
        , div [] <| viewCaptions model.captions
        ]


viewVideo : String -> Html Msg
viewVideo uri =
    let
        videoCode =
            find (Regex.AtMost 1) (regex "\\?v=(.+)$") uri
                |> List.map .submatches
    in
    case videoCode of
        [ [ Just code ] ] ->
            viewIframe code

        _ ->
           p [] [ text <| "video not found" ]


viewIframe : String -> Html Msg
viewIframe videoCode =
    iframe [ src ("https://www.youtube.com/embed/" ++ videoCode), attribute "frameborder" "0" ] []


viewCaptions : List Caption -> List (Html Msg)
viewCaptions captions =
    List.map (\caption -> p [] [ text <| toString caption.time ++ ": " ++ caption.text ]) captions



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
        Http.BadUrl msg ->
            msg

        Http.BadPayload msg _ ->
            msg

        Http.NetworkError ->
            "We weren't able to fetch the captions for this video. Please check your internet connection."

        Http.BadStatus res ->
            res.body

        _ ->
            "There was an error processing your request"


fetchCaptions : String -> Cmd Msg
fetchCaptions uri =
    let
        url =
            "http://localhost:3000/api/transcript?uri=" ++ uri

        request =
            Http.get url decodeCaptionJson
    in
    Http.send NewCaptions request


decodeCaptionJson : Decoder (List Caption)
decodeCaptionJson =
    list captionDecoder


captionDecoder : Decoder Caption
captionDecoder =
    map2 Caption
        (field "time" float)
        (field "text" string)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
