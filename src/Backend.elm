module Backend exposing (..)

import Base64
import Env
import Html
import Http
import Json.Decode exposing (Decoder)
import Lamdera exposing (ClientId, SessionId)
import Process
import Task exposing (Task)
import Types exposing (..)
import Url exposing (Url)


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \m -> Sub.none
        }


init : ( BackendModel, Cmd BackendMsg )
init =
    ( { previousThread = defaultUrl, lastCheck = Nothing }, Cmd.none )


defaultUrl : Url
defaultUrl =
    unsafeUrl "https://www.reddit.com/r/elm/comments/xikc54/easy_questions_beginners_thread_week_of_20220919/"


unsafeUrl : String -> Url
unsafeUrl text =
    case Url.fromString text of
        Just url ->
            url

        Nothing ->
            unreachable ()


unreachable : () -> a
unreachable () =
    let
        a =
            (\() -> 0) == (\() -> 0)
    in
    unreachable ()


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        RedditApiRequestMade result ->
            let
                _ =
                    Debug.log "a" result
            in
            ( model, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )


formUrlencoded : List ( String, String ) -> String
formUrlencoded object =
    object
        |> List.map
            (\( name, value ) ->
                Url.percentEncode name
                    ++ "="
                    ++ Url.percentEncode value
            )
        |> String.join "&"


requestAccessToken : Task Http.Error String
requestAccessToken =
    let
        auth =
            Env.clientId ++ ":" ++ Env.secret |> Base64.fromString |> Maybe.withDefault ""
    in
    Http.task
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Basic " ++ auth) ]
        , url = "https://www.reddit.com/api/v1/access_token"
        , body =
            formUrlencoded
                [ ( "grant_type", "password" )
                , ( "username", Env.username )
                , ( "password", Env.password )
                ]
                |> Http.stringBody "application/x-www-form-urlencoded"
        , resolver =
            Http.stringResolver
                (\response ->
                    case response of
                        Http.BadUrl_ string ->
                            Err (Http.BadUrl string)

                        Http.Timeout_ ->
                            Err Http.Timeout

                        Http.NetworkError_ ->
                            Err Http.NetworkError

                        Http.BadStatus_ metadata body_ ->
                            Err (Http.BadStatus metadata.statusCode)

                        Http.GoodStatus_ metadata body_ ->
                            case Json.Decode.decodeString accessTokenDecoder body_ of
                                Ok ok ->
                                    Ok ok

                                Err error ->
                                    Http.BadBody (Json.Decode.errorToString error) |> Err
                )
        , timeout = Just 30000
        }


accessTokenDecoder : Decoder String
accessTokenDecoder =
    Json.Decode.field "access_token" Json.Decode.string


beginnerQuestionsBody : Url -> String
beginnerQuestionsBody previousThread =
    """Hey [r/elm](https://www.reddit.com/r/elm/)! Let's answer your questions and get you unstuck. No question is too simple; if you're confused or need help with _anything at all_, please ask.

Other good places for these types of questions:

* The #beginners and #general channels on [The Elm Slack](https://elm-lang.org/community/slack)

* The "Learn" category on [Discourse](https://discourse.elm-lang.org/c/learn)

* [The elm-community FAQ page](http://faq.elm-community.org/)

[Last week](""" ++ Url.toString previousThread ++ """)"""



--{"jquery": [[0, 1, "call", ["body"]], [1, 2, "attr", "find"], [2, 3, "call", [".status"]], [3, 4, "attr", "hide"], [4, 5, "call", []], [5, 6, "attr", "html"], [6, 7, "call", [""]], [7, 8, "attr", "end"], [8, 9, "call", []], [1, 10, "attr", "redirect"], [10, 11, "call", ["https://www.reddit.com/r/elm/comments/xipj6t/easy_questions_beginners_thread_week_of_20220920/"]], [1, 12, "attr", "find"], [12, 13, "call", ["*[name=url]"]], [13, 14, "attr", "val"], [14, 15, "call", [""]], [15, 16, "attr", "end"], [16, 17, "call", []], [1, 18, "attr", "find"], [18, 19, "call", ["*[name=text]"]], [19, 20, "attr", "val"], [20, 21, "call", [""]], [21, 22, "attr", "end"], [22, 23, "call", []], [1, 24, "attr", "find"], [24, 25, "call", ["*[name=title]"]], [25, 26, "attr", "val"], [26, 27, "call", [" "]], [27, 28, "attr", "end"], [28, 29, "call", []]], "success": true}


submitDecoder : Decoder Url
submitDecoder =
    Json.Decode.field
        "jquery"
        (Json.Decode.list decodePart)
        |> Json.Decode.andThen
            (\list ->
                case List.filterMap identity list of
                    [ text ] ->
                        case Url.fromString text of
                            Just url ->
                                Json.Decode.succeed url

                            Nothing ->
                                Json.Decode.fail (text ++ " is not a valid url")

                    items ->
                        Json.Decode.fail ("Too many \"call\" attributes found: " ++ String.join "," items)
            )


decodePart : Decoder (Maybe String)
decodePart =
    Json.Decode.index 2 Json.Decode.string
        |> Json.Decode.andThen
            (\name ->
                if name == "call" then
                    Json.Decode.index 3 (Json.Decode.index 0 Json.Decode.string) |> Json.Decode.map Just

                else
                    Json.Decode.succeed Nothing
            )


postBeginnerQuestionsThread : Url -> String -> Task Http.Error Url
postBeginnerQuestionsThread previousThread accessToken =
    Http.task
        { method = "POST"
        , headers = [ Http.header "authorization" ("bearer " ++ accessToken) ]
        , url = "https://oauth.reddit.com/api/submit"
        , body =
            0 formUrlencoded
                [ ( "sr", "elm" )
                , ( "title", "Easy Questions / Beginners Thread (Week of 2022-09-20)" )
                , ( "text", beginnerQuestionsBody previousThread )
                , ( "kind", "self" )
                ]
                |> Http.stringBody "application/x-www-form-urlencoded"
        , resolver =
            Http.stringResolver
                (\response ->
                    case response of
                        Http.BadUrl_ string ->
                            Err (Http.BadUrl string)

                        Http.Timeout_ ->
                            Err Http.Timeout

                        Http.NetworkError_ ->
                            Err Http.NetworkError

                        Http.BadStatus_ metadata body_ ->
                            Err (Http.BadStatus metadata.statusCode)

                        Http.GoodStatus_ metadata body_ ->
                            case Json.Decode.decodeString submitDecoder body_ of
                                Ok ok ->
                                    Ok ok

                                Err error ->
                                    Http.BadBody (Json.Decode.errorToString error) |> Err
                )
        , timeout = Just 30000
        }
