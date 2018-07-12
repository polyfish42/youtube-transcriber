port module Main exposing (..)

import Combine exposing ((*>), (>>=), end, manyTill, or, parse, regex, while)
import Combine.Char exposing (anyChar)
import Fuzzy exposing (addPenalty, match, removePenalty)
import Html exposing (..)
import Html.Attributes exposing (attribute, checked, class, selected, src, style, value)
import Html.Events exposing (onClick, onInput, onWithOptions)
import Http exposing (get, send)
import Json.Decode exposing (Decoder, field, float, list, map2, string, succeed)
import Mouse
import Regex exposing (contains, regex)


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
    , search : String
    , caption : Maybe CaptionHistory
    , errorMessage : Maybe String
    , dropDownStatus : DropDownStatus
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


type DropDownStatus
    = Open
    | Closed



-- INIT


init : ( Model, Cmd Msg )
init =
    ( Model "" "apple" Nothing Nothing Closed, Cmd.none )



-- MESSAGE


type Msg
    = UpdateUri String
    | FetchCaptions
    | NewCaptions (Result Http.Error (List Caption))
    | ToggleDropDown
    | BlurDropDown
    | CaptionPicked Caption
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

        ToggleDropDown ->
            let
                newDropDownStatus =
                    if model.dropDownStatus == Closed then
                        Open
                    else
                        Closed
            in
            ( { model | dropDownStatus = newDropDownStatus }, Cmd.none )

        BlurDropDown ->
            ( { model | dropDownStatus = Closed }, Cmd.none )

        CaptionPicked caption ->
            let
                selectCurrent capList =
                    Just (List.partition (\x -> x.name == caption.name) capList)

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
            ( { model | caption = newCaptions, dropDownStatus = Closed }, Cmd.none )

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
    let
        isEnglish caption =
            String.contains "English" caption.name
                && not (String.contains "(auto-generated)" caption.name)
    in
    case List.partition isEnglish captions of
        ( [ x ], xs ) ->
            { model | caption = Just { current = x, rest = xs } }

        ( x :: xs, rest ) ->
            { model | caption = Just { current = x, rest = xs ++ rest } }

        ( [], x :: xs ) ->
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
    case model.dropDownStatus of
        Closed ->
            Sub.none

        Open ->
            Mouse.clicks (always BlurDropDown)



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ p [] [ viewErrorMessage model.errorMessage ]
        , input [ onInput UpdateUri ] []
        , button [ onClick FetchCaptions ] [ text <| "Submit" ]
        , viewCaptionPicker model
        , div [ class "transcript" ] (viewCaption model.caption model.search)
        ]


viewCaptionPicker : Model -> Html Msg
viewCaptionPicker model =
    case model.caption of
        Just cap ->
            let
                displayStyle =
                    if model.dropDownStatus == Open then
                        [ ( "display", "block" ) ]
                    else
                        [ ( "display", "none" ) ]
            in
            div
                [ onClick ToggleDropDown ]
                [ p
                    []
                    [ span [] [ text cap.current.name ]
                    , span [] [ text "▾" ]
                    ]
                , ul
                    [ style displayStyle ]
                    (List.map viewCaptionOptions <| allCaptions cap)
                ]

        Nothing ->
            div [] []


allCaptions : CaptionHistory -> List Caption
allCaptions captionHistory =
    List.sortBy .name <| captionHistory.current :: captionHistory.rest


viewCaptionOptions : Caption -> Html Msg
viewCaptionOptions caption =
    li
        [ onClick <| CaptionPicked caption ]
        [ text <| caption.name ]


onClick : msg -> Attribute msg
onClick message =
    onWithOptions
        "click"
        { stopPropagation = True
        , preventDefault = False
        }
        (succeed message)


viewCaption : Maybe CaptionHistory -> String -> List (Html Msg)
viewCaption captionHistory search =
    case captionHistory of
        Just history ->
            filterBySearch history search
                |> viewCaptions

        Nothing ->
            [ p [] [ text "No captions found for this video" ] ]


filterBySearch : CaptionHistory -> String -> List Line
filterBySearch history search =
    let
        isLetter char =
            contains (Regex.regex "\\w") <| toString char

        words line =
            line
                |> String.toLower
                |> String.filter isLetter
                |> String.split " "

        wordScore word =
            match [ addPenalty 1, removePenalty 1 ] [] search word |> .score

        totalWordScore line =
            words line
                |> List.map wordScore
                |> List.foldl (+) 0
    in
    List.filter (\c -> totalWordScore c.text >= 2000) history.current.captions


viewCaptions : List Line -> List (Html Msg)
viewCaptions lines =
    List.map
        (\caption ->
            p
                [ onClick <| SkipToTime caption.time
                , class "transcript__caption"
                ]
                [ text <| viewTime caption.time ++ ": " ++ caption.text ]
        )
        lines


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
