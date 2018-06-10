port module Main exposing (..)

import Combine exposing ((*>), (>>=), end, manyTill, or, parse, regex, while)
import Combine.Char exposing (anyChar)
import Debug
import Html exposing (..)
import Html.Attributes exposing (attribute, class, src)
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



-- MESSAGE


type Msg
    = UpdateUri String
    | FetchCaptions
    | NewCaptions (Result Http.Error (List Caption))
    | SkipToTime Float



-- UPDATE


port loadVideo : String -> Cmd msg


port skipToTime : Float -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        UpdateUri uri ->
            ( { model | uri = uri }, Cmd.none )

        FetchCaptions ->
            ( model, fetchCaptions model.uri )

        NewCaptions (Ok newCaptions) ->
            ( { model | captions = formatCaptions newCaptions }, loadVideo (videoId model.uri) )

        NewCaptions (Err message) ->
            ( { model | errorMessage = errorMessage message }, Cmd.none )

        SkipToTime time ->
            ( model, skipToTime time )


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


videoId : String -> String
videoId uri =
    case parse youTubeURIParser uri of
        Ok ( _, stream, result ) ->
            result

        Err ( _, stream, errors ) ->
            String.join " or " errors


youTubeURIParser : Combine.Parser state String
youTubeURIParser =
    let
        normalURI =
            manyTill anyChar (Combine.regex "v=")
                *> while ((/=) '&')

        shareURI =
            while ((/=) '?')

        chooseParser str =
            if str == "https://www.youtube.com/watch?" then
                normalURI
            else
                shareURI
    in
    or (Combine.string "https://www.youtube.com/watch?") (Combine.string "https://youtu.be/")
        >>= chooseParser


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


formatCaptions : List Caption -> List Caption
formatCaptions captions =
    List.map (\cap -> { time = cap.time, text = noHTMLCode cap.text }) captions


noHTMLCode : String -> String
noHTMLCode capText =
    Regex.replace Regex.All (Regex.regex "&#39;") (\_ -> "'") capText



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ p [] [ text model.errorMessage ]
        , input [ onInput UpdateUri ] []
        , button [ onClick FetchCaptions ] [ text <| "Submit" ]
        , div [ class "transcript" ] <| viewCaptions model.captions
        ]


viewCaptions : List Caption -> List (Html Msg)
viewCaptions captions =
    List.map
        (\caption ->
            p
                [ onClick <| SkipToTime caption.time
                , class "transcript__caption"
                ]
                [ text caption.text ]
        )
        captions


viewTime : Float -> String
viewTime time =
    let
        totalSeconds =
            floor time

        seconds =
            totalSeconds % 60

        minutes =
            (totalSeconds // 60) % 60

        hours =
            (totalSeconds // (60 * 60)) % 60
    in
    if totalSeconds > 60 * 60 then
        padTime hours ++ ":" ++ padTime minutes ++ ":" ++ padTime seconds
    else
        padTime minutes ++ ":" ++ padTime seconds


padTime : Int -> String
padTime time =
    if time < 10 then
        "0" ++ toString time
    else
        toString time
