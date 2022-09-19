module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Http
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , message : String
    }


type alias BackendModel =
    { message : String
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | RedditApiRequestMade_ (Result Http.Error String)


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg
    | RedditApiRequestMade (Result Http.Error String)


type ToFrontend
    = NoOpToFrontend
