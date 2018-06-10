module MainTest exposing (..)

import Expect exposing (Expectation)
import Main exposing (videoId)
import Test exposing (..)


suite : Test
suite =
    describe "Parse URL"
        [ test "Grabs YouTube Id out of URL" <|
            \_ ->
                let
                    uri =
                        "https://www.youtube.com/watch?v=y62zj9ozPOM&t=384"
                in
                Expect.equal "y62zj9ozPOM" (videoId uri)
        ]
