module MainTest exposing (..)

import Expect exposing (Expectation)
import Main exposing (videoId)
import Test exposing (..)


suite : Test
suite =
    describe "Parse URL"
        [ test "Grabs YouTube Id out of URL with other parameters present" <|
            \_ ->
                let
                    uri =
                        "https://www.youtube.com/watch?v=y62zj9ozPOM&t=15m26s"
                in
                Expect.equal "y62zj9ozPOM" (videoId uri)
        , test "Grabs YouTube Id out of URL with no other parameters present" <|
            \_ ->
                let
                    uri =
                        "https://www.youtube.com/watch?v=y62zj9ozPOM"
                in
                Expect.equal "y62zj9ozPOM" (videoId uri)
        , test "Handles share video url" <|
            \_ ->
                let
                    uri =
                        "https://youtu.be/y62zj9ozPOM?t=15m26s"
                in
                Expect.equal "y62zj9ozPOM" (videoId uri)
        ]
