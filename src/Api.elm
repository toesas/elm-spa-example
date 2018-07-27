module Api exposing (url)

import Url.Builder


{-| Get a URL to the Conduit API.
-}
url : List String -> String
url paths =
    -- NOTE: Url.Builder takes care of percent-encoding special URL characters.
    -- See https://package.elm-lang.org/packages/elm/url/latest/Url#percentEncode
    Url.Builder.crossOrigin "https://conduit.productionready.io"
        ("api" :: paths)
        []
