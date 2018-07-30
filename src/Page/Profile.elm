module Page.Profile exposing (Model, Msg, init, update, view)

{-| Viewing a user's profile.
-}

import Article.Feed as Feed exposing (ListConfig, defaultListConfig)
import Article.FeedSources as FeedSources exposing (FeedSources, Source(..))
import AuthToken exposing (AuthToken)
import Author exposing (Author(..), FollowedAuthor, UnfollowedAuthor)
import Avatar exposing (Avatar)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Me exposing (Me)
import Page.Errored exposing (PageLoadError, pageLoadError)
import Profile exposing (Profile)
import Session exposing (Session)
import Task exposing (Task)
import Time
import Username exposing (Username)
import Util
import Views.Article.Feed as Feed
import Views.Errors as Errors
import Views.Follow as Follow
import Views.Page as Page



-- MODEL


type alias Model =
    { errors : List String
    , username : Username
    , author : Author
    , feed : Feed.Model
    }


init : Maybe Me -> Username -> Task PageLoadError Model
init maybeMe username =
    let
        config : ListConfig
        config =
            { defaultListConfig | limit = 5, author = Just username }

        loadProfile =
            Author.fetch username maybeMe
                |> Http.toTask

        loadFeedSources =
            Feed.init maybeMe (defaultFeedSources username)

        handleLoadError _ =
            "Profile is currently unavailable."
                |> pageLoadError (Page.Profile username)
    in
    Task.map2 (Model [] username) loadProfile loadFeedSources
        |> Task.mapError handleLoadError



-- VIEW


view : Session -> Model -> { title : String, content : Html Msg }
view session model =
    let
        profile =
            Author.profile model.author

        username =
            Author.username model.author

        ( title, followButton ) =
            case model.author of
                IsMe _ _ ->
                    ( "My Profile"
                    , text ""
                    )

                IsFollowing followedAuthor ->
                    ( titleForOther (Author.followedUsername followedAuthor)
                    , Follow.unfollowButton ClickedUnfollow followedAuthor
                    )

                IsNotFollowing unfollowedAuthor ->
                    ( titleForOther (Author.unfollowedUsername unfollowedAuthor)
                    , Follow.followButton ClickedFollow unfollowedAuthor
                    )
    in
    { title = title
    , content =
        div [ class "profile-page" ]
            [ Errors.view ClickedDismissErrors model.errors
            , div [ class "user-info" ]
                [ div [ class "container" ]
                    [ div [ class "row" ]
                        [ div [ class "col-xs-12 col-md-10 offset-md-1" ]
                            [ img [ class "user-img", Avatar.src (Profile.avatar profile) ] []
                            , h4 [] [ Username.toHtml username ]
                            , p [] [ text (Maybe.withDefault "" (Profile.bio profile)) ]
                            , followButton
                            ]
                        ]
                    ]
                ]
            , div [ class "container" ]
                [ div [ class "row" ] [ viewFeed (Session.timeZone session) model.feed ] ]
            ]
    }


titleForOther : Username -> String
titleForOther otherUsername =
    "Profile — " ++ Username.toString otherUsername


viewFeed : Time.Zone -> Feed.Model -> Html Msg
viewFeed timeZone feed =
    div [ class "col-xs-12 col-md-10 offset-md-1" ] <|
        div [ class "articles-toggle" ]
            [ Feed.viewFeedSources feed |> Html.map GotFeedMsg ]
            :: (Feed.viewArticles timeZone feed |> List.map (Html.map GotFeedMsg))



-- UPDATE


type Msg
    = ClickedDismissErrors
    | ClickedFollow UnfollowedAuthor
    | ClickedUnfollow FollowedAuthor
    | CompletedFollowChange (Result Http.Error Author)
    | GotFeedMsg Feed.Msg


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        ClickedDismissErrors ->
            ( { model | errors = [] }, Cmd.none )

        ClickedUnfollow followedAuthor ->
            let
                cmdFromAuth authToken =
                    Author.requestUnfollow followedAuthor authToken
                        |> Http.send CompletedFollowChange
            in
            session
                |> Session.attempt "unfollow" cmdFromAuth
                |> Util.updateFromResult model Cmd.none

        ClickedFollow unfollowedAuthor ->
            let
                cmdFromAuth authToken =
                    Author.requestFollow unfollowedAuthor authToken
                        |> Http.send CompletedFollowChange
            in
            session
                |> Session.attempt "follow" cmdFromAuth
                |> Util.updateFromResult model Cmd.none

        CompletedFollowChange (Ok newAuthor) ->
            ( { model | author = newAuthor }
            , Cmd.none
            )

        CompletedFollowChange (Err error) ->
            ( model, Cmd.none )

        GotFeedMsg subMsg ->
            let
                ( newFeed, subCmd ) =
                    Feed.update (Session.me session) subMsg model.feed
            in
            ( { model | feed = newFeed }
            , Cmd.map GotFeedMsg subCmd
            )



-- INTERNAL --


defaultFeedSources : Username -> FeedSources
defaultFeedSources username =
    FeedSources.fromLists (AuthorFeed username) [ FavoritedFeed username ]
