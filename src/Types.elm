module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Http
import Time
import Url exposing (Url)


type alias FrontendModel =
    { key : Key }


type alias BackendModel =
    { previousThread : Url
    , lastCheck : Maybe Time.Posix
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | RedditApiRequestMade_ (Result Http.Error String)


type ToBackend
    = NoOpToBackend


type BackendMsg
    = RedditApiRequestMade (Result Http.Error String)
    | CheckedTime Time.Posix


type ToFrontend
    = NoOpToFrontend
