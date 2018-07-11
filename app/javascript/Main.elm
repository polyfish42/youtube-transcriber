port module Main exposing (..)

import Combine exposing ((*>), (>>=), end, manyTill, or, parse, regex, while)
import Combine.Char exposing (anyChar)
import Html exposing (..)
import Html.Attributes exposing (attribute, checked, class, selected, src, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (get, send)
import Json.Decode exposing (Decoder, field, float, list, map2, string)


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
    , caption : Maybe CaptionHistory
    , errorMessage : Maybe String
    }


type alias CaptionHistory =
    { current : Caption
    , rest : List Caption
    }


type alias Caption =
    { name : String
    , captions : List Line
    }


type alias Line =
    { time : Float
    , text : String
    }



-- INIT


init : ( Model, Cmd Msg )
init =
    ( Model "" Nothing Nothing, Cmd.none )



-- MESSAGE


type Msg
    = UpdateUri String
    | FetchCaptions
    | NewCaptions (Result Http.Error (List Caption))
    | SelectCaption String
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
            ( { model | errorMessage = Nothing }, fetchCaptions model.uri )

        NewCaptions (Ok newCaptions) ->
            ( addCaptions model newCaptions, loadVideo (videoId model.uri) )

        NewCaptions (Err message) ->
            ( { model | errorMessage = Just <| errorMessage message }, Cmd.none )

        SelectCaption name ->
            let
                selectCurrent capList =
                    Just (List.partition (\x -> x.name == name) capList)

                setCurrent capTuple =
                    case capTuple of
                        ( [ current ], rest ) ->
                            Just { current = current, rest = rest }

                        _ ->
                            Nothing

                newCaptions =
                    model.caption
                        |> Maybe.andThen (\c -> Just (c.current :: c.rest))
                        |> Maybe.andThen selectCurrent
                        |> Maybe.andThen setCurrent
            in
            ( { model | caption = newCaptions }, Cmd.none )

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


addCaptions : Model -> List Caption -> Model
addCaptions model captions =
    case captions of
        x :: xs ->
            { model | caption = Just { current = x, rest = xs } }

        _ ->
            model


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
        (field "name" string)
        (field "captions" <| list lineDecoder)


lineDecoder : Decoder Line
lineDecoder =
    map2 Line
        (field "time" float)
        (field "text" string)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ p [] [ viewErrorMessage model.errorMessage ]
        , input [ onInput UpdateUri ] []
        , button [ onClick FetchCaptions ] [ text <| "Submit" ]
        , viewCaptionPicker model.caption
        , div [ class "transcript" ] (viewCaption model.caption)
        ]


viewCaptionPicker : Maybe CaptionHistory -> Html Msg
viewCaptionPicker caption =
    case caption of
        Just c ->
            select [ onInput SelectCaption ] <| currentCaptionOption c.current :: captionOptions c.rest

        Nothing ->
            div [] []


currentCaptionOption : Caption -> Html Msg
currentCaptionOption caption =
    option [ selected True, value caption.name ] [ text caption.name ]


captionOptions : List Caption -> List (Html Msg)
captionOptions captions =
    List.map
        (\cap -> option [ selected False, value cap.name ] [ text cap.name ])
        captions


viewCaption : Maybe CaptionHistory -> List (Html Msg)
viewCaption captionHistory =
    case captionHistory of
        Just history ->
            List.map
                (\caption ->
                    p
                        [ onClick <| SkipToTime caption.time
                        , class "transcript__caption"
                        ]
                        [ text <| viewTime caption.time ++ ": " ++ caption.text ]
                )
                history.current.captions

        Nothing ->
            [ p [] [ text "No captions found for this video" ] ]


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


viewErrorMessage : Maybe String -> Html Msg
viewErrorMessage message =
    case message of
        Just message ->
            text message

        Nothing ->
            text ""
