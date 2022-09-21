module Types exposing (..)

import Browser
import Effect.Browser.Navigation exposing (Key)
import Effect.Http
import Effect.Lamdera exposing (ClientId, SessionId)
import Effect.Time
import Time
import Url exposing (Url)


type alias FrontendModel =
    { key : Effect.Browser.Navigation.Key, errors : LoadState }


type LoadState
    = Loading
    | Loaded (List { time : Time.Posix, error : Effect.Http.Error })


type alias BackendModel =
    { previousThread : Url
    , lastCheck : Maybe Time.Posix
    , errors : List { time : Time.Posix, error : Effect.Http.Error }
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | RedditApiRequestMade_ (Result Effect.Http.Error String)


type ToBackend
    = NoOpToBackend


type BackendMsg
    = RedditApiRequestMade Time.Posix (Result Effect.Http.Error Url)
    | CheckedTime Effect.Time.Posix
    | ClientConnected SessionId ClientId


type ToFrontend
    = SendErrors (List { time : Time.Posix, error : Effect.Http.Error })
