module Api exposing (listErrors, url)

import Http
import Json.Decode as Decode exposing (Decoder, decodeString, field)
import Url.Builder



-- URL


{-| Get a URL to the Conduit API.
-}
url : List String -> String
url paths =
    -- NOTE: Url.Builder takes care of percent-encoding special URL characters.
    -- See https://package.elm-lang.org/packages/elm/url/latest/Url#percentEncode
    Url.Builder.crossOrigin "https://conduit.productionready.io"
        ("api" :: paths)
        []



-- ERRORS


{-| Many API endpoints include an "errors" field in their BadStatus responses.
-}
listErrors : String -> Decoder (List String) -> Http.Error -> List String
listErrors fieldName decoder error =
    case error of
        Http.BadStatus response ->
            response.body
                |> decodeString (field fieldName decoder)
                |> Result.withDefault defaultServerErrors

        err ->
            defaultServerErrors


defaultServerErrors : List String
defaultServerErrors =
    [ "server error" ]
