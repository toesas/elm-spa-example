module Views.Article.Feed exposing (Model, Msg, init, selectTag, update, viewArticles, viewFeedSources)

{-| NOTE: This module has its own Model, view, and update. This is not normal!
If you find yourself doing this often, please watch <https://www.youtube.com/watch?v=DoA4Txr4GUs>

This is the reusable Article Feed that appears on both the Home page as well as
on the Profile page. There's a lot of logic here, so it's more convenient to use
the heavyweight approach of giving this its own Model, view, and update.

This means callers must use Html.map and Cmd.map to use this thing, but in
this case that's totally worth it because of the amount of logic wrapped up
in this thing.

For every other reusable view in this application, this API would be totally
overkill, so we use simpler APIs instead.

-}

import Article exposing (Article, Preview)
import Article.Feed as Feed exposing (Feed)
import Article.FeedSources as FeedSources exposing (FeedSources, Source(..))
import Article.Slug as ArticleSlug
import Article.Tag as Tag exposing (Tag)
import AuthToken exposing (AuthToken)
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder, src)
import Html.Events exposing (onClick)
import Http
import Me exposing (Me)
import Profile
import Session exposing (Session)
import Task exposing (Task)
import Time
import Username exposing (Username)
import Util exposing (onClickStopPropagation)
import Views.Article
import Views.Errors as Errors
import Views.Spinner exposing (spinner)



-- MODEL


type Model
    = Model InternalModel


{-| This should not be exposed! We want to benefit from the guarantee that only
this module can create or alter this model. This way if it ever ends up in
a surprising state, we know exactly where to look: this module.
-}
type alias InternalModel =
    { errors : List String
    , feed : Feed
    , feedSources : FeedSources
    , activePage : Int
    , isLoading : Bool
    }


init : Maybe Me -> FeedSources -> Task Http.Error Model
init maybeMe feedSources =
    let
        source =
            FeedSources.selected feedSources

        toModel ( activePage, feed ) =
            Model
                { errors = []
                , activePage = activePage
                , feed = feed
                , feedSources = feedSources
                , isLoading = False
                }
    in
    source
        |> fetch maybeMe 1
        |> Task.map toModel



-- VIEW


viewArticles : Time.Zone -> Model -> List (Html Msg)
viewArticles timeZone (Model { activePage, feed, feedSources }) =
    List.map (Views.Article.view ClickedFavorite timeZone) feed.articles
        ++ [ pagination activePage feed (FeedSources.selected feedSources) ]


viewFeedSources : Model -> Html Msg
viewFeedSources (Model { feedSources, isLoading, errors }) =
    let
        errorsHtml =
            Errors.view ClickedDismissErrors errors

        spinnerHtml =
            if isLoading then
                spinner

            else
                text ""
    in
    ul [ class "nav nav-pills outline-active" ] <|
        List.concat
            [ List.map (viewFeedSource False) (FeedSources.before feedSources)
            , [ viewFeedSource True (FeedSources.selected feedSources) ]
            , List.map (viewFeedSource False) (FeedSources.after feedSources)
            , [ errorsHtml, spinnerHtml ]
            ]


viewFeedSource : Bool -> Source -> Html Msg
viewFeedSource isSelected source =
    li [ class "nav-item" ]
        [ a
            [ classList [ ( "nav-link", True ), ( "active", isSelected ) ]
            , onClick (ClickedFeedSource source)

            -- The RealWorld CSS requires an href to work properly.
            , href ""
            ]
            [ text (sourceName source) ]
        ]


selectTag : Maybe Me -> Tag -> Cmd Msg
selectTag maybeMe tagName =
    let
        source =
            TagFeed tagName
    in
    source
        |> fetch maybeMe 1
        |> Task.attempt (CompletedFeedLoad source)


sourceName : Source -> String
sourceName source =
    case source of
        YourFeed ->
            "Your Feed"

        GlobalFeed ->
            "Global Feed"

        TagFeed tagName ->
            "#" ++ Tag.toString tagName

        FavoritedFeed username ->
            "Favorited Articles"

        AuthorFeed username ->
            "My Articles"


limit : Source -> Int
limit feedSource =
    case feedSource of
        YourFeed ->
            10

        GlobalFeed ->
            10

        TagFeed tagName ->
            10

        FavoritedFeed username ->
            5

        AuthorFeed username ->
            5


pagination : Int -> Feed -> Source -> Html Msg
pagination activePage feed feedSource =
    let
        articlesPerPage =
            limit feedSource

        totalPages =
            ceiling (toFloat feed.articlesCount / toFloat articlesPerPage)
    in
    if totalPages > 1 then
        List.range 1 totalPages
            |> List.map (\page -> pageLink page (page == activePage))
            |> ul [ class "pagination" ]

    else
        Html.text ""


pageLink : Int -> Bool -> Html Msg
pageLink page isActive =
    li [ classList [ ( "page-item", True ), ( "active", isActive ) ] ]
        [ a
            [ class "page-link"
            , onClick (ClickedFeedPage page)

            -- The RealWorld CSS requires an href to work properly.
            , href ""
            ]
            [ text (String.fromInt page) ]
        ]



-- UPDATE


type Msg
    = ClickedDismissErrors
    | ClickedFavorite (Article Preview)
    | ClickedFeedPage Int
    | ClickedFeedSource Source
    | CompletedFavorite (Result Http.Error (Article Preview))
    | CompletedFeedLoad Source (Result Http.Error ( Int, Feed ))


update : Maybe Me -> Msg -> Model -> ( Model, Cmd Msg )
update maybeMe msg (Model internalModel) =
    updateInternal maybeMe msg internalModel
        |> Tuple.mapFirst Model


updateInternal : Maybe Me -> Msg -> InternalModel -> ( InternalModel, Cmd Msg )
updateInternal maybeMe msg model =
    case msg of
        ClickedDismissErrors ->
            ( { model | errors = [] }, Cmd.none )

        ClickedFeedSource source ->
            source
                |> fetch maybeMe 1
                |> Task.attempt (CompletedFeedLoad source)
                |> Tuple.pair { model | isLoading = True }

        CompletedFeedLoad source (Ok ( activePage, feed )) ->
            ( { model
                | feed = feed
                , feedSources = FeedSources.select source model.feedSources
                , activePage = activePage
                , isLoading = False
              }
            , Cmd.none
            )

        CompletedFeedLoad _ (Err error) ->
            ( { model
                | errors = model.errors ++ [ "Server error while trying to load feed" ]
                , isLoading = False
              }
            , Cmd.none
            )

        ClickedFavorite article ->
            case maybeMe of
                Nothing ->
                    ( { model | errors = model.errors ++ [ "You are currently signed out. You must sign in to favorite articles." ] }
                    , Cmd.none
                    )

                Just me ->
                    Article.toggleFavorite article me
                        |> Http.send CompletedFavorite
                        |> Tuple.pair model

        CompletedFavorite (Ok article) ->
            let
                feed =
                    model.feed

                newFeed =
                    { feed | articles = List.map (replaceArticle article) feed.articles }
            in
            ( { model | feed = newFeed }, Cmd.none )

        CompletedFavorite (Err error) ->
            ( { model | errors = model.errors ++ [ "Server error while trying to favorite article." ] }
            , Cmd.none
            )

        ClickedFeedPage page ->
            let
                source =
                    FeedSources.selected model.feedSources
            in
            source
                |> fetch maybeMe page
                |> Task.andThen (\feed -> Task.map (\_ -> feed) scrollToTop)
                |> Task.attempt (CompletedFeedLoad source)
                |> Tuple.pair model


scrollToTop : Task x ()
scrollToTop =
    Dom.setViewport 0 0
        -- It's not worth showing the user anything special if scrolling fails.
        -- If anything, we'd log this to an error recording service.
        |> Task.onError (\_ -> Task.succeed ())


fetch : Maybe Me -> Int -> Source -> Task Http.Error ( Int, Feed )
fetch maybeMe page feedSource =
    let
        defaultListConfig =
            Feed.defaultListConfig

        articlesPerPage =
            limit feedSource

        offset =
            (page - 1) * articlesPerPage

        listConfig =
            { defaultListConfig | offset = offset, limit = articlesPerPage }

        task =
            case feedSource of
                YourFeed ->
                    case maybeMe of
                        Just me ->
                            let
                                defaultFeedConfig =
                                    Feed.defaultFeedConfig

                                feedConfig =
                                    { defaultFeedConfig | offset = offset, limit = articlesPerPage }
                            in
                            Feed.feed feedConfig me
                                |> Http.toTask

                        Nothing ->
                            Http.BadUrl "You need to be signed in to view your feed."
                                |> Task.fail

                GlobalFeed ->
                    Feed.list listConfig maybeMe
                        |> Http.toTask

                TagFeed tagName ->
                    Feed.list { listConfig | tag = Just tagName } maybeMe
                        |> Http.toTask

                FavoritedFeed username ->
                    Feed.list { listConfig | favorited = Just username } maybeMe
                        |> Http.toTask

                AuthorFeed username ->
                    Feed.list { listConfig | author = Just username } maybeMe
                        |> Http.toTask
    in
    task
        |> Task.map (\feed -> ( page, feed ))


replaceArticle : Article a -> Article a -> Article a
replaceArticle newArticle oldArticle =
    if Article.slug newArticle == Article.slug oldArticle then
        newArticle

    else
        oldArticle
