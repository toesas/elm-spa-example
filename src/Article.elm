module Article
    exposing
        ( Article
        , Full
        , Preview
        , author
        , body
        , create
        , delete
        , fetch
        , followAuthor
        , fromPreview
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
    = Article Slug Profile Metadata a


{-| Metadata about the article - its title, description, and so on.

Importantly, this module's public API exposes a way to read this metadata, but
not to alter it. There is only one

If we find ourselves using any particular piece of metadata often,
for example `title`, we could expose a convenience function like this:

Article.title : Article a -> String

If you like, it's totally reasonable to expose a function like that for every one
of these fields!

(Okay, to be completely honest, exposing one function per field is how I prefer
to do it, and that's how I originally wrote this module. However, I'm aware that
this code base has become a common reference point for beginners, and I think it
is _extremely important_ that slapping some "getters and setters" on a record
does not become a habit for anyone who is getting started with Elm. The whole
point of making the Article type opaque is to create guarantees through
_selectively choosing boundaries_ around it. If you aren't selective about
where those boundaries are, and instead expose a "getter and setter" for every
field in the record, the result is an API with no more guarantees than if you'd
exposed the entire record directly! It is so important to me that beginners not
fall into the terrible "getters and setters" trap that I've exposed this
Metadata record instead of exposing a single function for each of its fields,
as I did originally. This record is not a bad way to do it, by any means,
but if this seems at odds with <https://youtu.be/x1FU3e0sT1I> - now you know why!
See commit c2640ae3abd60262cdaafe6adee3f41d84cd85c3 for how it looked before.
)
-}
type alias Metadata =
    { description : String
    , title : String
    , tags : List String
    , createdAt : Time.Posix
    , favorited : Bool
    , favoritesCount : Int
    }


type Preview
    = Preview


type Full
    = Full Body



-- INFO


author : Article a -> Profile
author (Article _ val _ _) =
    val


metadata : Article a -> Metadata
metadata (Article _ _ val _) =
    val


slug : Article a -> Slug
slug (Article val _ _ _) =
    val


body : Article Full -> Body
body (Article _ _ _ (Full val)) =
    val



-- TRANSFORM


followAuthor : Bool -> Article a -> Article a
followAuthor isFollowing (Article slugVal authorVal meta extras) =
    Article slugVal (Profile.follow isFollowing authorVal) meta extras


fromPreview : Body -> Article Preview -> Article Full
fromPreview newBody (Article newSlug newAuthor newMetadata Preview) =
    Article newSlug newAuthor newMetadata (Full newBody)



-- SERIALIZATION


previewDecoder : Decoder (Article Preview)
previewDecoder =
    Decode.succeed Article
        |> required "slug" Slug.decoder
        |> required "author" Profile.decoder
        |> custom metadataDecoder
        |> hardcoded Preview


fullDecoder : Decoder (Article Full)
fullDecoder =
    Decode.succeed Article
        |> required "slug" Slug.decoder
        |> required "author" Profile.decoder
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



-- SINGLE


fetch : Maybe AuthToken -> Slug -> Http.Request (Article Full)
fetch maybeToken articleSlug =
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
toggleFavorite (Article slugVal _ { favorited } _) authToken =
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
