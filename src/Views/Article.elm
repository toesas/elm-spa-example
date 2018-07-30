module Views.Article exposing (view, viewTimestamp)

{-| Viewing a preview of an individual article, excluding its body.
-}

import Article exposing (Article)
import Author
import Avatar exposing (Avatar)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder, src)
import Profile
import Route exposing (Route)
import Time
import Util
import Views.Article.Favorite as Favorite
import Views.Author



-- TIMESTAMP ONLY


{-| Some pages want to view only the timestamp, not the entire article preview.
-}
viewTimestamp : Time.Zone -> Article a -> Html msg
viewTimestamp timeZone article =
    let
        { createdAt } =
            Article.metadata article
    in
    span [ class "date" ] [ text (Util.formatTimestamp timeZone createdAt) ]



-- ARTICLE PREVIEW


view : (Article a -> msg) -> Time.Zone -> Article a -> Html msg
view toggleFavorite timeZone article =
    let
        { title, description, favoritesCount } =
            Article.metadata article

        author =
            Article.author article

        profile =
            Author.profile author

        username =
            Author.username author
    in
    div [ class "article-preview" ]
        [ div [ class "article-meta" ]
            [ a [ Route.href (Route.Profile username) ]
                [ img [ Avatar.src (Profile.avatar profile) ] [] ]
            , div [ class "info" ]
                [ Views.Author.view username
                , viewTimestamp timeZone article
                ]
            , Favorite.button
                toggleFavorite
                article
                [ class "pull-xs-right" ]
                [ text (" " ++ String.fromInt favoritesCount) ]
            ]
        , a [ class "preview-link", Route.href (Route.Article (Article.slug article)) ]
            [ h1 [] [ text title ]
            , p [] [ text description ]
            , span [] [ text "Read more..." ]
            ]
        ]
