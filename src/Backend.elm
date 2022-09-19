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
import Url


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \m -> Sub.none
        }


init : ( BackendModel, Cmd BackendMsg )
init =
    ( { message = "Hello!" }
    , Cmd.none
    )


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
        body =
            formUrlencoded
                [ ( "grant_type", "password" )
                , ( "username", Env.username )
                , ( "password", Env.password )
                ]
                |> Http.stringBody "application/x-www-form-urlencoded"

        auth =
            Env.clientId ++ ":" ++ Env.secret |> Base64.fromString |> Maybe.withDefault ""
    in
    Http.task
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Basic " ++ auth) ]
        , url = "https://www.reddit.com/api/v1/access_token"
        , body = body
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
