module Frontend exposing (app, appFunctions)

import Browser
import Date
import Effect.Browser.Navigation
import Effect.Command as Command exposing (Command, FrontendOnly)
import Effect.Http exposing (Error(..))
import Effect.Lamdera
import Effect.Subscription as Subscription exposing (Subscription)
import Html
import Iso8601
import Lamdera
import Time
import Types exposing (..)
import Url exposing (Url)


app =
    Effect.Lamdera.frontend Lamdera.sendToBackend appFunctions


appFunctions =
    { init = init
    , onUrlRequest = UrlClicked
    , onUrlChange = UrlChanged
    , update = update
    , updateFromBackend = updateFromBackend
    , subscriptions = \m -> Subscription.none
    , view = view
    }


init : Url -> Effect.Browser.Navigation.Key -> ( FrontendModel, Command FrontendOnly ToBackend FrontendMsg )
init url key =
    ( { key = key, errors = Loading }, Command.none )


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Command FrontendOnly ToBackend FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Effect.Browser.Navigation.pushUrl model.key (Url.toString url)
                    )

                Browser.External url ->
                    ( model
                    , Effect.Browser.Navigation.load url
                    )

        UrlChanged url ->
            ( model, Command.none )

        NoOpFrontendMsg ->
            ( model, Command.none )

        RedditApiRequestMade_ result ->
            let
                _ =
                    Debug.log "a" result
            in
            ( model, Command.none )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Command FrontendOnly toMsg FrontendMsg )
updateFromBackend msg model =
    case msg of
        SendErrors errors ->
            ( { model | errors = Loaded errors }, Command.none )


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = "Reddit bot"
    , body =
        case model.errors of
            Loading ->
                [ Html.text "Loading..." ]

            Loaded errors ->
                List.map
                    (\{ time, error } ->
                        Html.div
                            []
                            [ Iso8601.fromTime time
                                ++ ": "
                                ++ (case error of
                                        BadUrl url ->
                                            "Bad url " ++ url

                                        Timeout ->
                                            "Timeout"

                                        NetworkError ->
                                            "NetworkError"

                                        BadStatus int ->
                                            "BadStatus " ++ String.fromInt int

                                        BadBody string ->
                                            "BadBody, " ++ string
                                   )
                                |> Html.text
                            ]
                    )
                    errors
    }
