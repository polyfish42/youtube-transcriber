port module Main exposing (..)

import Combine exposing ((*>), (>>=), end, manyTill, or, parse, regex, while)
import Combine.Char exposing (anyChar)
import Dom exposing (blur)
import Fuzzy exposing (addPenalty, match, removePenalty)
import Html exposing (..)
import Html.Attributes exposing (attribute, checked, class, classList, href, id, placeholder, selected, src, style, value)
import Html.Events exposing (onClick, onInput, onSubmit, onWithOptions)
import Http exposing (get, send)
import Json.Decode exposing (Decoder, field, float, list, map2, string, succeed)
import Mouse
import Task


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
    ( Model "" "" Nothing Nothing Closed, Cmd.none )



-- MESSAGE


type Msg
    = UpdateUri String
    | FetchCaptions
    | NewCaptions (Result Http.Error (List Caption))
    | ToggleDropDown
    | BlurDropDown
    | CaptionPicked Caption
    | UpdateSearch String
    | SkipToTime Float
    | BlurResult (Result Dom.Error ())



-- UPDATE


port loadVideo : String -> Cmd msg


port skipToTime : Float -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        UpdateUri uri ->
            ( { model | uri = uri }, Cmd.none )

        FetchCaptions ->
            { model | errorMessage = Nothing } ! [ fetchCaptions model.uri, blur "uri-input" |> Task.attempt BlurResult ]

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

        UpdateSearch search ->
            ( { model | search = String.toLower search }, Cmd.none )

        SkipToTime time ->
            ( model, skipToTime time )

        BlurResult _ ->
            ( model, Cmd.none )


errorMessage : Http.Error -> String
errorMessage message =
    case message of
        Http.BadUrl msg ->
            msg

        Http.BadPayload msg _ ->
            msg

        Http.NetworkError ->
            "We weren't able to fetch the captions for this video. Please check your internet connection."

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
    div [ class "content" ]
        [ div [ classList [ ( "header", True ) ] ]
            [ h1 [] [ text "YouTube Transcriber" ]
            , form
                [ onSubmit FetchCaptions ]
                [ input
                    [ onInput UpdateUri
                    , placeholder "Enter YouTube URL"
                    , id "uri-input"
                    ]
                    []
                ]
            , h1 [ class "bio" ]
                [ text "Made by "
                , a [ href "https://www.jakebrady.me/" ] [ text "Jake Brady" ]
                ]
            , viewErrorMessage model.errorMessage
            ]
        , div [ id "player" ] []
        , viewSearchBar model
        , div [ class "transcript" ] (viewCaption model.caption model.search)
        ]


captionDoesExist : Maybe CaptionHistory -> Bool
captionDoesExist history =
    case history of
        Just _ ->
            True

        Nothing ->
            False


viewSearchBar : Model -> Html Msg
viewSearchBar model =
    case model.caption of
        Just caption ->
            div [ classList [ ( "search-bar", True ) ] ]
                [ viewCaptionPicker caption model.dropDownStatus
                , viewSearchCaptions
                ]

        Nothing ->
            div [] []


viewCaptionPicker : CaptionHistory -> DropDownStatus -> Html Msg
viewCaptionPicker caption dropDownStatus =
    let
        displayStyle =
            if dropDownStatus == Open then
                [ ( "display", "block" ) ]
            else
                [ ( "display", "none" ) ]
    in
    div
        [ onClick ToggleDropDown, class "caption-dropdown" ]
        [ p
            []
            [ span [] [ text caption.current.name ]
            , span [] [ text "▾" ]
            ]
        , ul
            [ style displayStyle, class "caption-dropdown-list" ]
            (List.map viewCaptionOptions <| allCaptions caption)
        ]


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


viewSearchCaptions : Html Msg
viewSearchCaptions =
    div []
        [ input [ onInput UpdateSearch, placeholder "Search" ] []
        ]


viewCaption : Maybe CaptionHistory -> String -> List (Html Msg)
viewCaption captionHistory search =
    case captionHistory of
        Just history ->
            viewCaptions history search

        Nothing ->
            [ div [] [] ]


viewCaptions : CaptionHistory -> String -> List (Html Msg)
viewCaptions history search =
    let
        cleanStr line =
            line
                |> String.toLower

        lines =
            history.current.captions

        fuzzyMatch line =
            let
                text =
                    cleanStr line.text
            in
            ( match [] [ " " ] search text, line )

        scoredLines =
            List.map fuzzyMatch lines

        sortedLines =
            List.sortBy (\m -> Tuple.first m |> .score) scoredLines
    in
    if search == "" then
        List.map viewOneCaption history.current.captions
    else
        List.map viewSearchedCaption sortedLines


viewOneCaption : Line -> Html Msg
viewOneCaption line =
    div
        [ onClick <| SkipToTime line.time
        , class "transcript__caption"
        ]
        [ span [ class "time" ] [ text <| viewTime line.time ++ ": " ]
        , text <| line.text
        ]


viewSearchedCaption : ( Fuzzy.Result, Line ) -> Html Msg
viewSearchedCaption ( result, line ) =
    let
        isKey index =
            List.foldl
                (\e sum ->
                    if not sum then
                        List.member (index - e.offset) e.keys
                    else
                        sum
                )
                False
                result.matches

        isMatch index =
            List.foldl
                (\e sum ->
                    if not sum then
                        e.offset <= index && (e.offset + e.length) > index
                    else
                        sum
                )
                False
                result.matches

        color index =
            if isKey index then
                [ ( "color", "red" ) ]
            else
                []

        bgColor index =
            if isMatch index then
                [ ( "background-color", "yellow" ) ]
            else
                []

        hStyle index =
            style (color index ++ bgColor index)

        accumulateChar c ( sum, index ) =
            ( sum ++ [ span [ hStyle index ] [ c |> String.fromChar |> text ] ], index + 1 )

        highlight =
            String.foldl accumulateChar ( [ span [ class "time" ] [ viewTime line.time ++ ": " |> text ] ], 0 ) line.text
    in
    p
        [ onClick <| SkipToTime line.time
        , class "transcript__caption"
        ]
        (Tuple.first highlight)


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
            p [ class "error-message" ] [ text message ]

        Nothing ->
            text ""
