module Tests exposing (..)

import Backend
import Base64
import Bytes.Encode
import Date
import Dict
import Duration
import Effect.Http exposing (Response(..))
import Effect.Test exposing (RequestedBy(..))
import Env
import Expect exposing (Expectation)
import Frontend
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Time exposing (Month(..))
import Types exposing (BackendModel, BackendMsg, FrontendModel, FrontendMsg, ToBackend, ToFrontend)
import Unsafe


config : Effect.Test.Config ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel
config =
    { frontendApp = Frontend.appFunctions
    , backendApp = Backend.appFunctions
    , handleHttpRequest =
        \{ currentRequest } ->
            if currentRequest.url == "https://www.reddit.com/api/v1/access_token" then
                """{ "access_token": \""""
                    ++ accessToken
                    ++ """" }"""
                    |> Bytes.Encode.string
                    |> Bytes.Encode.encode
                    |> GoodStatus_
                        { url = currentRequest.url
                        , statusCode = 200
                        , statusText = "OK"
                        , headers = Dict.empty
                        }

            else if currentRequest.url == "https://oauth.reddit.com/api/submit" then
                """{"jquery": [[0, 1, "call", ["body"]], [1, 2, "attr", "find"], [2, 3, "call", [".status"]], [3, 4, "attr", "hide"], [4, 5, "call", []], [5, 6, "attr", "html"], [6, 7, "call", [""]], [7, 8, "attr", "end"], [8, 9, "call", []], [1, 10, "attr", "redirect"], [10, 11, "call", ["https://www.reddit.com/r/elm/comments/xipj6t/easy_questions_beginners_thread_week_of_20220920/"]], [1, 12, "attr", "find"], [12, 13, "call", ["*[name=url]"]], [13, 14, "attr", "val"], [14, 15, "call", [""]], [15, 16, "attr", "end"], [16, 17, "call", []], [1, 18, "attr", "find"], [18, 19, "call", ["*[name=text]"]], [19, 20, "attr", "val"], [20, 21, "call", [""]], [21, 22, "attr", "end"], [22, 23, "call", []], [1, 24, "attr", "find"], [24, 25, "call", ["*[name=title]"]], [25, 26, "attr", "val"], [26, 27, "call", [" "]], [27, 28, "attr", "end"], [28, 29, "call", []]], "success": true}"""
                    |> Bytes.Encode.string
                    |> Bytes.Encode.encode
                    |> GoodStatus_
                        { url = currentRequest.url
                        , statusCode = 200
                        , statusText = "OK"
                        , headers = Dict.empty
                        }

            else
                NetworkError_
    , handlePortToJs = always Nothing
    , handleFileRequest = always Nothing
    , domain = Unsafe.url "https://reddit-bot.lamdera.app"
    }


accessToken =
    "abc123"


previousThreadUrl =
    Unsafe.url "https://www.reddit.com/r/elm/comments/xikc54/easy_questions_beginners_thread_week_of_20220919/"


suite : Test
suite =
    let
        auth =
            Env.clientId ++ ":" ++ Env.secret |> Base64.fromString |> Maybe.withDefault ""
    in
    Effect.Test.start config "Happy path"
        |> Effect.Test.simulateTime (Duration.minutes 61)
        |> Effect.Test.checkState
            (\state ->
                case state.httpRequests of
                    [ request2, request1 ] ->
                        let
                            expected1 : Effect.Test.HttpRequest
                            expected1 =
                                { requestedBy = RequestedByBackend
                                , method = "POST"
                                , url = "https://www.reddit.com/api/v1/access_token"
                                , body =
                                    Effect.Test.StringBody
                                        { contentType = "application/x-www-form-urlencoded"
                                        , content =
                                            Backend.formUrlencoded
                                                [ ( "grant_type", "password" )
                                                , ( "username", Env.username )
                                                , ( "password", Env.password )
                                                ]
                                        }
                                , headers = [ ( "Authorization", "Basic " ++ auth ) ]
                                }

                            expected2 =
                                { requestedBy = RequestedByBackend
                                , method = "POST"
                                , url = "https://oauth.reddit.com/api/submit"
                                , body =
                                    Effect.Test.StringBody
                                        { contentType = "application/x-www-form-urlencoded"
                                        , content =
                                            Backend.formUrlencoded
                                                [ ( "sr", "elm" )
                                                , ( "title"
                                                  , "Easy Questions / Beginners Thread (Week of "
                                                        ++ Date.toIsoString (Date.fromCalendarDate 1970 Jan 1)
                                                        ++ ")"
                                                  )
                                                , ( "text", Backend.beginnerQuestionsBody previousThreadUrl )
                                                , ( "kind", "self" )
                                                ]
                                        }
                                , headers = [ ( "Authorization", "bearer " ++ accessToken ) ]
                                }
                        in
                        if request1 == expected1 then
                            if request2 == expected2 then
                                Ok ()

                            else
                                "Incorrect second request:\n\n"
                                    ++ Debug.toString request2
                                    ++ "\n\nexpected:\n\n"
                                    ++ Debug.toString expected2
                                    |> Err

                        else
                            Err ("Incorrect first request: " ++ Debug.toString request1)

                    _ ->
                        Err "Wrong number of requests made"
            )
        |> Effect.Test.toTest
