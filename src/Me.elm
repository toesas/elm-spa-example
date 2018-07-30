module Me exposing (Me, authToken, decoder, username, withAuthorization)

import Api
import AuthToken exposing (AuthToken, withAuthorization)
import Avatar exposing (Avatar)
import Email exposing (Email)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, required)
import Json.Encode as Encode exposing (Value)
import Profile exposing (Profile)
import Username exposing (Username)


{-| The currently signed-in user.

This is used for things like Session, Login, Logout, and Settings.

-}
type Me
    = Me Username AuthToken



-- INFO


username : Me -> Username
username (Me val _) =
    val


authToken : Me -> AuthToken
authToken (Me _ val) =
    val


withAuthorization : Maybe Me -> RequestBuilder a -> RequestBuilder a
withAuthorization maybeMe builder =
    AuthToken.withAuthorization (Maybe.map authToken maybeMe) builder



-- SERIALIZATION


decoder : Decoder Me
decoder =
    Decode.succeed Me
        |> required "username" Username.decoder
        |> required "token" AuthToken.decoder
