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
import AuthToken exposing (AuthToken, withAuthorization)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect, withQueryParams)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Me exposing (Me)
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


list : ListConfig -> Maybe Me -> Http.Request Feed
list config maybeMe =
    [ Maybe.map (\tag -> ( "tag", Tag.toString tag )) config.tag
    , Maybe.map (\author -> ( "author", Username.toString author )) config.author
    , Maybe.map (\favorited -> ( "favorited", Username.toString favorited )) config.favorited
    , Just ( "limit", String.fromInt config.limit )
    , Just ( "offset", String.fromInt config.offset )
    ]
        |> List.filterMap identity
        |> buildFromQueryParams maybeMe (Api.url [ "articles" ])
        |> withAuthorization (Maybe.map Me.authToken maybeMe)
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


feed : FeedConfig -> Me -> Http.Request Feed
feed config me =
    [ ( "limit", String.fromInt config.limit )
    , ( "offset", String.fromInt config.offset )
    ]
        |> buildFromQueryParams (Just me) (Api.url [ "articles", "feed" ])
        |> withAuthorization (Just (Me.authToken me))
        |> HttpBuilder.toRequest



-- SERIALIZATION


decoder : Maybe Me -> Decoder Feed
decoder maybeMe =
    Decode.succeed Feed
        |> required "articles" (Decode.list (Article.previewDecoder maybeMe))
        |> required "articlesCount" Decode.int



-- REQUEST


buildFromQueryParams : Maybe Me -> String -> List ( String, String ) -> RequestBuilder Feed
buildFromQueryParams maybeMe url queryParams =
    HttpBuilder.get url
        |> withExpect (Http.expectJson (decoder maybeMe))
        |> withQueryParams queryParams
