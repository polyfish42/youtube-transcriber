module Test.Generated.Main3729662777 exposing (main)

import MainTest

import Test.Reporter.Reporter exposing (Report(..))
import Console.Text exposing (UseColor(..))
import Test.Runner.Node
import Test
import Json.Encode

main : Test.Runner.Node.TestProgram
main =
    [     Test.describe "MainTest" [MainTest.suite] ]
        |> Test.concat
        |> Test.Runner.Node.runWithOptions { runs = Nothing, report = (ConsoleReport UseColor), seed = Nothing, processes = 4, paths = []}