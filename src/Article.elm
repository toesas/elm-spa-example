module Article
    exposing
        ( Article
        , Full
        , Preview
        , body
        , create
        , delete
        , followAuthor
        , fromPreview
        , get
        , metadata
        , previewDecoder
        , slug
        , toggleFavorite
        , update
        )

{-| The interface to the Article data structure.

This includes:

  - The Article type itself
  - Ways to make HTTP requests to retrieve and modify articles
  - Ways to access parts of an article
  - Converting between various types

The constructor for Article is not exposed, and neither are its encoders and decoders. This means it's only possible to obtain an Article by
using the functions exposed in this module - the HTTP requests and such.

-}

import Api
import Article.Body as Body exposing (Body)
import Article.Slug as Slug exposing (Slug)
import Article.Tag as Tag exposing (Tag)
import AuthToken exposing (AuthToken, withAuthorization)
import Html exposing (Attribute, Html)
import Http
import HttpBuilder exposing (RequestBuilder, withBody, withExpect, withQueryParams)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, hardcoded, required)
import Json.Encode as Encode
import Markdown
import Profile exposing (Profile)
import Time
import Username as Username exposing (Username)
import Util



-- TYPES


{-| An article, optionally with an article body.

To see the difference between { extraInfo : a } and { extraInfo : Maybe Body },
consider the difference between the "view individual article" page (which
renders one article, including its body) and the "article feed" -
which displays multiple articles, but without bodies.

This definition for `Article` means we can write:

viewArticle : Article Full -> Html msg
viewFeed : List (Article Preview) -> Html msg

This indicates that `viewArticle` requires an article _with a `body` present_,
wereas `viewFeed` accepts articles with no bodies. (We could also have written
it as `List (Article a)` to specify that feeds can accept either articles that
have `body` present or not. Either work, given that feeds do not attempt to
read the `body` field from articles.)

This is an important distinction, because in Request.Article, the `feed`
function produces `List (Article Preview)` because the API does not return bodies.
Those articles are useful to the feed, but not to the individual article view.

-}
type Article a
    = Article Slug Metadata a


{-| Metadata about the article - its title, description, and so on.

Importantly, this module's public API exposes a way to read this metadata, but
not to alter it. This is read-only information! Only the server can alter it.

If we find ourselves using any particular piece of metadata often,
for example `title`, we could expose a convenience function like this:

Article.title : Article a -> String

-}
type alias Metadata =
    { description : String
    , title : String
    , tags : List String
    , createdAt : Time.Posix
    , favorited : Bool
    , favoritesCount : Int
    , author : Profile
    }


type Preview
    = Preview


type Full
    = Full Body



-- INFO


metadata : Article a -> Metadata
metadata (Article _ val _) =
    val


slug : Article a -> Slug
slug (Article val _ _) =
    val


body : Article Full -> Body
body (Article _ _ (Full val)) =
    val



-- TRANSFORM


followAuthor : Bool -> Article a -> Article a
followAuthor isFollowing (Article slugVal meta extras) =
    Article slugVal
        { meta | author = Profile.follow isFollowing meta.author }
        extras


fromPreview : Body -> Article Preview -> Article Full
fromPreview newBody (Article newSlug newMetadata Preview) =
    Article newSlug newMetadata (Full newBody)



-- SERIALIZATION


previewDecoder : Decoder (Article Preview)
previewDecoder =
    Decode.succeed Article
        |> required "slug" Slug.decoder
        |> custom metadataDecoder
        |> hardcoded Preview


fullDecoder : Decoder (Article Full)
fullDecoder =
    Decode.succeed Article
        |> required "slug" Slug.decoder
        |> custom metadataDecoder
        |> required "body" (Decode.map Full Body.decoder)


metadataDecoder : Decoder Metadata
metadataDecoder =
    Decode.succeed Metadata
        |> required "description" (Decode.map (Maybe.withDefault "") (Decode.nullable Decode.string))
        |> required "title" Decode.string
        |> required "tagList" (Decode.list Decode.string)
        |> required "createdAt" Util.dateStringDecoder
        |> required "favorited" Decode.bool
        |> required "favoritesCount" Decode.int
        |> required "author" Profile.decoder



-- SINGLE


get : Maybe AuthToken -> Slug -> Http.Request (Article Full)
get maybeToken articleSlug =
    let
        expect =
            fullDecoder
                |> Decode.field "article"
                |> Http.expectJson
    in
    articleUrl articleSlug []
        |> HttpBuilder.get
        |> HttpBuilder.withExpect expect
        |> withAuthorization maybeToken
        |> HttpBuilder.toRequest



-- FAVORITE


toggleFavorite : Article a -> AuthToken -> Http.Request (Article Preview)
toggleFavorite (Article slugVal { favorited } _) authToken =
    if favorited then
        unfavorite slugVal authToken

    else
        favorite slugVal authToken


favorite : Slug -> AuthToken -> Http.Request (Article Preview)
favorite =
    buildFavorite HttpBuilder.post


unfavorite : Slug -> AuthToken -> Http.Request (Article Preview)
unfavorite =
    buildFavorite HttpBuilder.delete


buildFavorite :
    (String -> RequestBuilder a)
    -> Slug
    -> AuthToken
    -> Http.Request (Article Preview)
buildFavorite builderFromUrl articleSlug token =
    let
        expect =
            previewDecoder
                |> Decode.field "article"
                |> Http.expectJson

        url =
            articleUrl articleSlug [ "favorite" ]
    in
    builderFromUrl url
        |> withAuthorization (Just token)
        |> withExpect expect
        |> HttpBuilder.toRequest



-- CREATE


type alias CreateConfig record =
    { record
        | title : String
        , description : String
        , body : String
        , tags : List String
    }


type alias EditConfig record =
    { record
        | title : String
        , description : String
        , body : String
    }


create : CreateConfig record -> AuthToken -> Http.Request (Article Full)
create config token =
    let
        expect =
            fullDecoder
                |> Decode.field "article"
                |> Http.expectJson

        article =
            Encode.object
                [ ( "title", Encode.string config.title )
                , ( "description", Encode.string config.description )
                , ( "body", Encode.string config.body )
                , ( "tagList", Encode.list Encode.string config.tags )
                ]

        jsonBody =
            Encode.object [ ( "article", article ) ]
                |> Http.jsonBody
    in
    Api.url [ "articles" ]
        |> HttpBuilder.post
        |> withAuthorization (Just token)
        |> withBody jsonBody
        |> withExpect expect
        |> HttpBuilder.toRequest



-- UPDATE


update : Slug -> EditConfig record -> AuthToken -> Http.Request (Article Full)
update articleSlug config token =
    let
        expect =
            fullDecoder
                |> Decode.field "article"
                |> Http.expectJson

        article =
            Encode.object
                [ ( "title", Encode.string config.title )
                , ( "description", Encode.string config.description )
                , ( "body", Encode.string config.body )
                ]

        jsonBody =
            Encode.object [ ( "article", article ) ]
                |> Http.jsonBody
    in
    articleUrl articleSlug []
        |> HttpBuilder.put
        |> withAuthorization (Just token)
        |> withBody jsonBody
        |> withExpect expect
        |> HttpBuilder.toRequest



-- DELETE


delete : Slug -> AuthToken -> Http.Request ()
delete articleSlug token =
    articleUrl articleSlug []
        |> HttpBuilder.delete
        |> withAuthorization (Just token)
        |> HttpBuilder.toRequest



-- URLS


articleUrl : Slug -> List String -> String
articleUrl articleSlug paths =
    allArticlesUrl (Slug.toString articleSlug :: paths)


allArticlesUrl : List String -> String
allArticlesUrl paths =
    Api.url ("articles" :: paths)
