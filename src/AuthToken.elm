module AuthToken exposing (AuthToken, addAuthHeader, decoder, encode, username)

import HttpBuilder exposing (RequestBuilder, withHeader)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (Value)
import Username exposing (Username)


{-| The authentication token for the currently logged-in user.

The token records the username associated with this token, which you can ask it for.

By design, there is no way to access the token directly as a String. You can encode it for persistence, and you can add it to a header to a HttpBuilder for a request, but that's it.

-}



-- TYPES


type AuthToken
    = AuthToken Username String



-- INFO


username : AuthToken -> Username
username (AuthToken val _) =
    val



-- SERIALIZATION


decoder : Decoder AuthToken
decoder =
    Decode.succeed AuthToken
        |> required "username" Username.decoder
        |> required "token" Decode.string



-- TRANSFORM


encode : AuthToken -> Value
encode (AuthToken _ str) =
    Encode.string str


addAuthHeader : Maybe AuthToken -> RequestBuilder a -> RequestBuilder a
addAuthHeader maybeToken builder =
    case maybeToken of
        Just (AuthToken _ str) ->
            builder
                |> withHeader "authorization" ("Token " ++ str)

        Nothing ->
            builder
