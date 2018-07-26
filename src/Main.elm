module Main exposing (main)

import Article.FeedSources as FeedSources
import Article.Slug exposing (Slug)
import AuthToken exposing (AuthToken)
import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (..)
import Json.Decode as Decode exposing (Value)
import Me exposing (Me)
import Page.Article as Article
import Page.Article.Editor as Editor
import Page.Errored as Errored exposing (PageLoadError)
import Page.Home as Home
import Page.Login as Login
import Page.NotFound as NotFound
import Page.Profile as Profile
import Page.Register as Register
import Page.Settings as Settings
import Route exposing (Route)
import Session exposing (Session)
import Task
import Time
import Url exposing (Url)
import Username exposing (Username)
import Views.Page as Page exposing (ActivePage)



-- WARNING: Based on discussions around how asset management features
-- like code splitting and lazy loading have been shaping up, I expect
-- most of this file to become unnecessary in a future release of Elm.
-- Avoid putting things in here unless there is no alternative!


type CurrentPage
    = Blank
    | NotFound
    | Errored PageLoadError
    | Home Home.Model
    | Settings Settings.Model
    | Login Login.Model
    | Register Register.Model
    | Profile Username Profile.Model
    | Article Article.Model
    | Editor (Maybe Slug) Editor.Model


type PageState
    = Loaded CurrentPage
    | TransitioningFrom CurrentPage



-- MODEL


type alias Model =
    { session : Session
    , navKey : Nav.Key
    , pageState : PageState
    }


init : Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    setRoute (Route.fromUrl url)
        { pageState = Loaded initialPage
        , navKey = navKey
        , session = fromFlags flags
        }
        |> Tuple.mapSecond (\cmd -> Cmd.batch [ cmd, Task.perform GotTimeZone Time.here ])


fromFlags : Value -> Session
fromFlags flags =
    Session.init Time.utc (decodeFlags flags)


decodeFlags : Value -> Maybe ( Me, AuthToken )
decodeFlags json =
    case Decode.decodeValue Decode.string json of
        Ok str ->
            case Decode.decodeString Me.decoderWithToken str of
                Ok tuple ->
                    Just tuple

                Err _ ->
                    Nothing

        Err _ ->
            Nothing


initialPage : CurrentPage
initialPage =
    Blank



-- VIEW


view : Model -> Document Msg
view model =
    case model.pageState of
        Loaded page ->
            viewCurrentPage model.session False page

        TransitioningFrom page ->
            viewCurrentPage model.session True page


mapBody : (msgA -> msgB) -> Document msgA -> Document msgB
mapBody transform { body, title } =
    { title = title
    , body = List.map (Html.map transform) body
    }


viewCurrentPage : Session -> Bool -> CurrentPage -> Document Msg
viewCurrentPage session isLoading page =
    let
        frame =
            Page.frame isLoading (Session.me session)
    in
    case page of
        NotFound ->
            NotFound.view
                |> frame Page.Other

        Blank ->
            -- This is for the very initial page load, while we are loading
            -- data via HTTP. We could also render a spinner here.
            { title = "Loading", content = Html.text "" }
                |> frame Page.Other

        Errored subModel ->
            Errored.view session subModel
                |> frame Page.Other

        Settings subModel ->
            Settings.view session subModel
                |> frame Page.Other
                |> mapBody GotSettingsMsg

        Home subModel ->
            Home.view session subModel
                |> frame Page.Home
                |> mapBody GotHomeMsg

        Login subModel ->
            Login.view session subModel
                |> frame Page.Other
                |> mapBody GotLoginMsg

        Register subModel ->
            Register.view session subModel
                |> frame Page.Other
                |> mapBody GotRegisterMsg

        Profile username subModel ->
            Profile.view session subModel
                |> frame (Page.Profile username)
                |> mapBody GotProfileMsg

        Article subModel ->
            Article.view session subModel
                |> frame Page.Other
                |> mapBody GotArticleMsg

        Editor maybeSlug subModel ->
            let
                framePage =
                    if maybeSlug == Nothing then
                        Page.NewArticle

                    else
                        Page.Other
            in
            Editor.view subModel
                |> frame framePage
                |> mapBody GotEditorMsg



-- SUBSCRIPTIONS
-- Note: we aren't currently doing any page subscriptions, but I thought it would
-- be a good idea to put this in here as an example. If I were actually
-- maintaining this in production, I wouldn't bother until I needed this!


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ pageSubscriptions (getCurrentPage model.pageState)
        , Sub.map ChangedSession (Session.changes (Session.timeZone model.session))
        ]


getCurrentPage : PageState -> CurrentPage
getCurrentPage pageState =
    case pageState of
        Loaded page ->
            page

        TransitioningFrom page ->
            page


pageSubscriptions : CurrentPage -> Sub Msg
pageSubscriptions page =
    case page of
        Blank ->
            Sub.none

        Errored _ ->
            Sub.none

        NotFound ->
            Sub.none

        Settings _ ->
            Sub.none

        Home _ ->
            Sub.none

        Login _ ->
            Sub.none

        Register _ ->
            Sub.none

        Profile _ _ ->
            Sub.none

        Article _ ->
            Sub.none

        Editor _ _ ->
            Sub.none



-- UPDATE


type Msg
    = ChangedRoute (Maybe Route)
    | ChangedSession Session
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | LoadedArticle (Result PageLoadError Article.Model)
    | LoadedProfile Username (Result PageLoadError Profile.Model)
    | LoadedEditArticle Slug (Result PageLoadError Editor.Model)
    | LoadedHome (Result PageLoadError Home.Model)
    | GotTimeZone Time.Zone
    | GotHomeMsg Home.Msg
    | GotSettingsMsg Settings.Msg
    | GotLoginMsg Login.Msg
    | GotRegisterMsg Register.Msg
    | GotProfileMsg Profile.Msg
    | GotArticleMsg Article.Msg
    | GotEditorMsg Editor.Msg


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    let
        transition toMsg task =
            ( { model | pageState = TransitioningFrom (getCurrentPage model.pageState) }
            , Task.attempt toMsg task
            )

        errored =
            pageErrored model
    in
    case maybeRoute of
        Nothing ->
            ( { model | pageState = Loaded NotFound }, Cmd.none )

        Just Route.NewArticle ->
            if Session.isLoggedIn model.session then
                ( { model | pageState = Loaded (Editor Nothing Editor.initNew) }, Cmd.none )

            else
                errored Page.NewArticle "You must be signed in to post an article."

        Just (Route.EditArticle slug) ->
            if Session.isLoggedIn model.session then
                transition (LoadedEditArticle slug)
                    (Editor.initEdit (Session.token model.session) slug)

            else
                errored Page.Other "You must be signed in to edit an article."

        Just Route.Settings ->
            case Session.me model.session of
                Just me ->
                    ( { model | pageState = Loaded (Settings (Settings.init me)) }, Cmd.none )

                Nothing ->
                    errored Page.Settings "You must be signed in to access your settings."

        Just Route.Home ->
            transition LoadedHome (Home.init (Session.token model.session))

        Just Route.Root ->
            ( model, Route.replaceUrl model.navKey Route.Home )

        Just Route.Login ->
            ( { model | pageState = Loaded (Login Login.initialModel) }, Cmd.none )

        Just Route.Logout ->
            ( { model | session = Session.clear model.session }
            , Cmd.batch
                [ Session.logout
                , Route.replaceUrl model.navKey Route.Home
                ]
            )

        Just Route.Register ->
            ( { model | pageState = Loaded (Register Register.initialModel) }, Cmd.none )

        Just (Route.Profile username) ->
            transition (LoadedProfile username) (Profile.init (Session.token model.session) username)

        Just (Route.Article slug) ->
            transition LoadedArticle (Article.init (Session.token model.session) slug)


pageErrored : Model -> ActivePage -> String -> ( Model, Cmd msg )
pageErrored model activePage errorMessage =
    let
        error =
            Errored.pageLoadError activePage errorMessage
    in
    ( { model | pageState = Loaded (Errored error) }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updateCurrentPage (getCurrentPage model.pageState) msg model


updateCurrentPage : CurrentPage -> Msg -> Model -> ( Model, Cmd Msg )
updateCurrentPage page msg model =
    let
        session =
            model.session

        toPage toModel toMsg subUpdate subMsg subModel =
            let
                ( newModel, newCmd ) =
                    subUpdate subMsg subModel
            in
            ( { model | pageState = Loaded (toModel newModel) }, Cmd.map toMsg newCmd )

        errored =
            pageErrored model
    in
    case ( msg, page ) of
        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    case url.fragment of
                        Nothing ->
                            -- If we got a link that didn't include a fragment,
                            -- it's from one of those (href "") attributes that
                            -- we have to include to make the RealWorld CSS work.
                            --
                            -- In a typical application, this whole
                            -- `case url.fragment of` expression would be
                            -- unnecessary.
                            ( model, Cmd.none )

                        Just _ ->
                            ( model
                            , Nav.pushUrl model.navKey (Url.toString url)
                            )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ( ChangedUrl url, _ ) ->
            setRoute (Route.fromUrl url) model

        ( ChangedRoute route, _ ) ->
            setRoute route model

        ( LoadedHome (Ok subModel), _ ) ->
            ( { model | pageState = Loaded (Home subModel) }, Cmd.none )

        ( LoadedHome (Err error), _ ) ->
            ( { model | pageState = Loaded (Errored error) }, Cmd.none )

        ( LoadedProfile username (Ok subModel), _ ) ->
            ( { model | pageState = Loaded (Profile username subModel) }, Cmd.none )

        ( LoadedProfile username (Err error), _ ) ->
            ( { model | pageState = Loaded (Errored error) }, Cmd.none )

        ( LoadedArticle (Ok subModel), _ ) ->
            ( { model | pageState = Loaded (Article subModel) }, Cmd.none )

        ( LoadedArticle (Err error), _ ) ->
            ( { model | pageState = Loaded (Errored error) }, Cmd.none )

        ( LoadedEditArticle slug (Ok subModel), _ ) ->
            ( { model | pageState = Loaded (Editor (Just slug) subModel) }, Cmd.none )

        ( LoadedEditArticle slug (Err error), _ ) ->
            ( { model | pageState = Loaded (Errored error) }, Cmd.none )

        ( ChangedSession newSession, _ ) ->
            let
                cmd =
                    -- If we just signed out, then redirect to Home.
                    case ( Session.token session, Session.token newSession ) of
                        ( Just _, Nothing ) ->
                            Route.replaceUrl model.navKey Route.Home

                        ( _, _ ) ->
                            Cmd.none
            in
            ( { model | session = newSession }
            , cmd
            )

        ( GotSettingsMsg subMsg, Settings subModel ) ->
            case Session.token model.session of
                Just authToken ->
                    let
                        ( ( pageModel, cmd ), msgFromPage ) =
                            Settings.update model.navKey authToken subMsg subModel

                        newModel =
                            case msgFromPage of
                                Settings.NoOp ->
                                    model

                                Settings.ChangedMe me ->
                                    { model
                                        | session =
                                            Session.init
                                                (Session.timeZone model.session)
                                                (Just ( me, authToken ))
                                    }
                    in
                    ( { newModel | pageState = Loaded (Settings pageModel) }
                    , Cmd.map GotSettingsMsg cmd
                    )

                Nothing ->
                    ( model, Cmd.none )

        ( GotLoginMsg subMsg, Login subModel ) ->
            let
                ( ( pageModel, cmd ), msgFromPage ) =
                    Login.update model.navKey subMsg subModel

                newModel =
                    case msgFromPage of
                        Login.NoOp ->
                            model

                        Login.ChangedMeAndToken pair ->
                            { model
                                | session =
                                    Session.init
                                        (Session.timeZone model.session)
                                        (Just pair)
                            }
            in
            ( { newModel | pageState = Loaded (Login pageModel) }
            , Cmd.map GotLoginMsg cmd
            )

        ( GotRegisterMsg subMsg, Register subModel ) ->
            let
                ( ( pageModel, cmd ), msgFromPage ) =
                    Register.update model.navKey subMsg subModel

                newModel =
                    case msgFromPage of
                        Register.NoOp ->
                            model

                        Register.ChangedMeAndToken pair ->
                            { model
                                | session =
                                    Session.init
                                        (Session.timeZone model.session)
                                        (Just pair)
                            }
            in
            ( { newModel | pageState = Loaded (Register pageModel) }
            , Cmd.map GotRegisterMsg cmd
            )

        ( GotHomeMsg subMsg, Home subModel ) ->
            toPage Home GotHomeMsg (Home.update (Session.token session)) subMsg subModel

        ( GotProfileMsg subMsg, Profile username subModel ) ->
            toPage (Profile username) GotProfileMsg (Profile.update (Session.token model.session)) subMsg subModel

        ( GotArticleMsg subMsg, Article subModel ) ->
            toPage Article GotArticleMsg (Article.update model.navKey model.session) subMsg subModel

        ( GotEditorMsg subMsg, Editor slug subModel ) ->
            case Session.token model.session of
                Nothing ->
                    if slug == Nothing then
                        errored Page.NewArticle
                            "You must be signed in to post articles."

                    else
                        errored Page.Other
                            "You must be signed in to edit articles."

                Just token ->
                    toPage (Editor slug) GotEditorMsg (Editor.update token model.navKey) subMsg subModel

        ( _, NotFound ) ->
            -- Disregard incoming messages when we're on the
            -- NotFound page.
            ( model, Cmd.none )

        ( _, _ ) ->
            -- Disregard incoming messages that arrived for the wrong page
            ( model, Cmd.none )



-- MAIN


main : Program Value Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
