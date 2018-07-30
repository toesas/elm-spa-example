module Article.Feed
    exposing
        ( Feed
        , FeedConfig
        , ListConfig
        , defaultFeedConfig
        , defaultListConfig
        , feed
        , list
        )

import Api
import Article exposing (Article, Preview)
import Article.Tag as Tag exposing (Tag)
import AuthToken exposing (AuthToken, addAuthHeader)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect, withQueryParams)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Username exposing (Username)



-- TYPES


type alias Feed =
    { articles : List (Article Preview)
    , articlesCount : Int
    }



-- LIST


type alias ListConfig =
    { tag : Maybe Tag
    , author : Maybe Username
    , favorited : Maybe Username
    , limit : Int
    , offset : Int
    }


defaultListConfig : ListConfig
defaultListConfig =
    { tag = Nothing
    , author = Nothing
    , favorited = Nothing
    , limit = 20
    , offset = 0
    }


list : ListConfig -> Maybe AuthToken -> Http.Request Feed
list config maybeToken =
    [ Maybe.map (\tag -> ( "tag", Tag.toString tag )) config.tag
    , Maybe.map (\author -> ( "author", Username.toString author )) config.author
    , Maybe.map (\favorited -> ( "favorited", Username.toString favorited )) config.favorited
    , Just ( "limit", String.fromInt config.limit )
    , Just ( "offset", String.fromInt config.offset )
    ]
        |> List.filterMap identity
        |> buildFromQueryParams maybeToken (Api.url [ "articles" ])
        |> addAuthHeader maybeToken
        |> HttpBuilder.toRequest



-- FEED


type alias FeedConfig =
    { limit : Int
    , offset : Int
    }


defaultFeedConfig : FeedConfig
defaultFeedConfig =
    { limit = 10
    , offset = 0
    }


feed : FeedConfig -> AuthToken -> Http.Request Feed
feed config authToken =
    [ ( "limit", String.fromInt config.limit )
    , ( "offset", String.fromInt config.offset )
    ]
        |> buildFromQueryParams (Just authToken) (Api.url [ "articles", "feed" ])
        |> addAuthHeader (Just authToken)
        |> HttpBuilder.toRequest



-- SERIALIZATION


decoder : Maybe AuthToken -> Decoder Feed
decoder maybeToken =
    Decode.succeed Feed
        |> required "articles" (Decode.list (Article.previewDecoder maybeToken))
        |> required "articlesCount" Decode.int



-- REQUEST


buildFromQueryParams : Maybe AuthToken -> String -> List ( String, String ) -> RequestBuilder Feed
buildFromQueryParams maybeToken url queryParams =
    HttpBuilder.get url
        |> withExpect (Http.expectJson (decoder maybeToken))
        |> withQueryParams queryParams
