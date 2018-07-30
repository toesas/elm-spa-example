module Page.Article exposing (Model, Msg, init, update, view)

{-| Viewing an individual article.
-}

import Article exposing (Article, Full)
import Article.Body
import Article.Comment as Comment exposing (Comment)
import Article.Slug as Slug exposing (Slug)
import AuthToken exposing (AuthToken, withAuthorization)
import Author exposing (Author(..), FollowedAuthor, UnfollowedAuthor)
import Avatar
import Browser.Navigation as Nav
import CommentId exposing (CommentId)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, disabled, href, id, placeholder)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import HttpBuilder exposing (RequestBuilder, withBody, withExpect, withQueryParams)
import Me exposing (Me)
import Page.Errored exposing (PageLoadError, pageLoadError)
import Profile exposing (Profile)
import Route
import Session exposing (LoggedInUser, Session)
import Task exposing (Task)
import Time
import Username exposing (Username)
import Util
import Views.Article
import Views.Article.Favorite as Favorite
import Views.Author
import Views.Errors
import Views.Follow as Follow
import Views.Page as Page



-- MODEL


type alias Model =
    { errors : List String
    , commentText : String
    , commentInFlight : Bool
    , article : Article Full
    , comments : List Comment
    }


init : Maybe Me -> Slug -> Task PageLoadError Model
init maybeMe slug =
    let
        loadArticle =
            Article.fetch maybeMe slug
                |> Http.toTask

        loadComments =
            Comment.list maybeMe slug
                |> Http.toTask

        handleLoadError _ =
            pageLoadError Page.Other "Article is currently unavailable."
    in
    Task.map2 (Model [] "" False) loadArticle loadComments
        |> Task.mapError handleLoadError



-- VIEW


view : Session -> Model -> { title : String, content : Html Msg }
view session model =
    let
        loggedInUser =
            Session.loggedInUser session

        maybeMe =
            Maybe.map .me loggedInUser

        article =
            model.article

        { title } =
            Article.metadata article

        author =
            Article.author article

        avatar =
            Profile.avatar (Author.profile author)

        buttons =
            viewButtons article author

        timeZone =
            Session.timeZone session

        postingDisabled =
            model.commentInFlight
    in
    { title = title
    , content =
        div [ class "article-page" ]
            [ viewBanner timeZone model.errors article author
            , div [ class "container page" ]
                [ div [ class "row article-content" ]
                    [ div [ class "col-md-12" ]
                        [ Article.Body.toHtml (Article.body article) [] ]
                    ]
                , hr [] []
                , div [ class "article-actions" ]
                    [ div [ class "article-meta" ] <|
                        [ a [ Route.href (Route.Profile (Author.username author)) ]
                            [ img [ Avatar.src avatar ] [] ]
                        , div [ class "info" ]
                            [ Views.Author.view (Author.username author)
                            , Views.Article.viewTimestamp timeZone article
                            ]
                        ]
                            ++ buttons
                    ]
                , div [ class "row" ]
                    [ div [ class "col-xs-12 col-md-8 offset-md-2" ] <|
                        viewAddComment postingDisabled loggedInUser
                            :: List.map (viewComment timeZone maybeMe) model.comments
                    ]
                ]
            ]
    }


viewBanner : Time.Zone -> List String -> Article a -> Author -> Html Msg
viewBanner timeZone errors article author =
    let
        { title } =
            Article.metadata article

        buttons =
            viewButtons article author

        profile =
            Author.profile author
    in
    div [ class "banner" ]
        [ div [ class "container" ]
            [ h1 [] [ text title ]
            , div [ class "article-meta" ] <|
                [ a [ Route.href (Route.Profile (Author.username author)) ]
                    [ img [ Avatar.src (Profile.avatar profile) ] [] ]
                , div [ class "info" ]
                    [ Views.Author.view (Author.username author)
                    , Views.Article.viewTimestamp timeZone article
                    ]
                ]
                    ++ buttons
            , Views.Errors.view ClickedDismissErrors errors
            ]
        ]


viewAddComment : Bool -> Maybe LoggedInUser -> Html Msg
viewAddComment postingDisabled loggedInUser =
    case loggedInUser of
        Nothing ->
            p []
                [ a [ Route.href Route.Login ] [ text "Sign in" ]
                , text " or "
                , a [ Route.href Route.Register ] [ text "sign up" ]
                , text " to add comments on this article."
                ]

        Just { me, profile } ->
            Html.form [ class "card comment-form", onSubmit ClickedPostComment ]
                [ div [ class "card-block" ]
                    [ textarea
                        [ class "form-control"
                        , placeholder "Write a comment..."
                        , attribute "rows" "3"
                        , onInput EnteredCommentText
                        ]
                        []
                    ]
                , div [ class "card-footer" ]
                    [ img [ class "comment-author-img", Avatar.src (Profile.avatar profile) ] []
                    , button
                        [ class "btn btn-sm btn-primary"
                        , disabled postingDisabled
                        ]
                        [ text "Post Comment" ]
                    ]
                ]


viewButtons : Article a -> Author -> List (Html Msg)
viewButtons article author =
    case author of
        IsFollowing followedAuthor ->
            Follow.unfollowButton ClickedUnfollow followedAuthor
                |> withFavoriteButton article

        IsNotFollowing unfollowedAuthor ->
            Follow.followButton ClickedFollow unfollowedAuthor
                |> withFavoriteButton article

        IsMe _ _ ->
            [ editButton article
            , text " "
            , deleteButton article
            ]


withFavoriteButton : Article a -> Html Msg -> List (Html Msg)
withFavoriteButton article html =
    [ html
    , text " "
    , favoriteButton article
    ]


viewComment : Time.Zone -> Maybe Me -> Comment -> Html Msg
viewComment timeZone maybeMe comment =
    let
        author =
            Comment.author comment

        profile =
            Author.profile author

        authorUsername =
            Author.username author

        isMine =
            case maybeMe of
                Just me ->
                    Me.username me == authorUsername

                Nothing ->
                    False

        timestamp =
            Util.formatTimestamp timeZone (Comment.createdAt comment)
    in
    div [ class "card" ]
        [ div [ class "card-block" ]
            [ p [ class "card-text" ] [ text (Comment.body comment) ] ]
        , div [ class "card-footer" ]
            [ a [ class "comment-author", href "" ]
                [ img [ class "comment-author-img", Avatar.src (Profile.avatar profile) ] []
                , text " "
                ]
            , text " "
            , a [ class "comment-author", Route.href (Route.Profile authorUsername) ]
                [ text (Username.toString authorUsername) ]
            , span [ class "date-posted" ] [ text timestamp ]
            , if isMine then
                span
                    [ class "mod-options"
                    , onClick (ClickedDeleteComment (Comment.id comment))
                    ]
                    [ i [ class "ion-trash-a" ] [] ]

              else
                text ""
            ]
        ]



-- UPDATE


type Msg
    = ClickedDeleteArticle
    | ClickedDeleteComment CommentId
    | ClickedDismissErrors
    | ClickedFavorite
    | ClickedFollow UnfollowedAuthor
    | ClickedUnfollow FollowedAuthor
    | ClickedPostComment
    | EnteredCommentText String
    | CompletedDeleteArticle (Result Http.Error ())
    | CompletedDeleteComment CommentId (Result Http.Error ())
    | CompletedFavoriteChange (Result Http.Error (Article Full))
    | CompletedFollowChange (Result Http.Error Author)
    | CompletedPostComment (Result Http.Error Comment)


update : Nav.Key -> Session -> Msg -> Model -> ( Model, Cmd Msg )
update navKey session msg model =
    let
        article =
            model.article

        author =
            Article.author article

        oldBody =
            Article.body article
    in
    case msg of
        ClickedDismissErrors ->
            ( { model | errors = [] }, Cmd.none )

        ClickedFavorite ->
            case Session.me session of
                Nothing ->
                    ( { model | errors = model.errors ++ [ "Please sign in to favorite this article." ] }
                    , Cmd.none
                    )

                Just me ->
                    ( model
                    , Article.toggleFavorite article me
                        |> Http.toTask
                        |> Task.map (Article.fromPreview oldBody)
                        |> Task.attempt CompletedFavoriteChange
                    )

        CompletedFavoriteChange (Ok newArticle) ->
            ( { model | article = newArticle }, Cmd.none )

        CompletedFavoriteChange (Err error) ->
            -- In a serious production application, we would log the error to
            -- a logging service so we could investigate later.
            ( { model | errors = model.errors ++ [ "There was a server error trying to record your Favorite. Sorry!" ] }
            , Cmd.none
            )

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
            ( { model | article = Article.mapAuthor (\_ -> newAuthor) article }, Cmd.none )

        CompletedFollowChange (Err error) ->
            ( { model | errors = "Unable to follow user." :: model.errors }, Cmd.none )

        EnteredCommentText commentText ->
            ( { model | commentText = commentText }, Cmd.none )

        ClickedPostComment ->
            let
                comment =
                    model.commentText
            in
            if model.commentInFlight || String.isEmpty comment then
                ( model, Cmd.none )

            else
                case Session.me session of
                    Nothing ->
                        ( { model | errors = model.errors ++ [ "Please sign in to post a comment." ] }
                        , Cmd.none
                        )

                    Just me ->
                        ( { model | commentInFlight = True }
                        , me
                            |> Comment.post (Article.slug model.article) comment
                            |> Http.send CompletedPostComment
                        )

        CompletedPostComment (Ok comment) ->
            ( { model
                | commentInFlight = False
                , comments = comment :: model.comments
              }
            , Cmd.none
            )

        CompletedPostComment (Err error) ->
            ( { model | errors = model.errors ++ [ "Server error while trying to post comment." ] }
            , Cmd.none
            )

        ClickedDeleteComment id ->
            let
                cmdFromAuth authToken =
                    authToken
                        |> Comment.delete (Article.slug model.article) id
                        |> Http.send (CompletedDeleteComment id)
            in
            session
                |> Session.attempt "delete comments" cmdFromAuth
                |> Util.updateFromResult model Cmd.none

        CompletedDeleteComment id (Ok ()) ->
            ( { model | comments = withoutComment id model.comments }
            , Cmd.none
            )

        CompletedDeleteComment id (Err error) ->
            ( { model | errors = model.errors ++ [ "Server error while trying to delete comment." ] }
            , Cmd.none
            )

        ClickedDeleteArticle ->
            let
                cmdFromAuth authToken =
                    authToken
                        |> delete (Article.slug model.article)
                        |> Http.send CompletedDeleteArticle
            in
            session
                |> Session.attempt "delete articles" cmdFromAuth
                |> Util.updateFromResult model Cmd.none

        CompletedDeleteArticle (Ok ()) ->
            ( model, Route.replaceUrl navKey Route.Home )

        CompletedDeleteArticle (Err error) ->
            ( { model | errors = model.errors ++ [ "Server error while trying to delete article." ] }
            , Cmd.none
            )



-- HTTP


delete : Slug -> AuthToken -> Http.Request ()
delete slug token =
    Article.url slug []
        |> HttpBuilder.delete
        |> withAuthorization (Just token)
        |> HttpBuilder.toRequest



-- INTERNAL


withoutComment : CommentId -> List Comment -> List Comment
withoutComment id =
    List.filter (\comment -> Comment.id comment /= id)


favoriteButton : Article a -> Html Msg
favoriteButton article =
    let
        { favoritesCount } =
            Article.metadata article

        favoriteText =
            " Favorite Article (" ++ String.fromInt favoritesCount ++ ")"
    in
    Favorite.button (\_ -> ClickedFavorite) article [] [ text favoriteText ]


deleteButton : Article a -> Html Msg
deleteButton article =
    button [ class "btn btn-outline-danger btn-sm", onClick ClickedDeleteArticle ]
        [ i [ class "ion-trash-a" ] [], text " Delete Article" ]


editButton : Article a -> Html Msg
editButton article =
    a [ class "btn btn-outline-secondary btn-sm", Route.href (Route.EditArticle (Article.slug article)) ]
        [ i [ class "ion-edit" ] [], text " Edit Article" ]
