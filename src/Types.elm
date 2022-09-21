module Types exposing (..)

import Browser
import Effect.Browser.Navigation exposing (Key)
import Effect.Http
import Effect.Time
import Url exposing (Url)


type alias FrontendModel =
    { key : Effect.Browser.Navigation.Key }


type alias BackendModel =
    { previousThread : Url
    , lastCheck : Maybe Effect.Time.Posix
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | RedditApiRequestMade_ (Result Effect.Http.Error String)


type ToBackend
    = NoOpToBackend


type BackendMsg
    = RedditApiRequestMade (Result Effect.Http.Error Url)
    | CheckedTime Effect.Time.Posix


type ToFrontend
    = NoOpToFrontend
