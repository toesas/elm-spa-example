module Me exposing (Me, avatar, bio, decoder, decoderWithToken, email, login, register, username)

import Api
import AuthToken exposing (AuthToken, withAuthorization)
import Avatar exposing (Avatar)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, required)
import Json.Encode as Encode exposing (Value)
import Username exposing (Username)


{-| The currently signed-in user.

This is used for things like login, logout, and settings.

Contrast with Profile, which is a user whose profile you're viewing.

-}
type Me
    = Me Internals


type alias Internals =
    { username : Username
    , bio : Maybe String
    , avatar : Avatar
    , email : String
    }



-- INFO


username : Me -> Username
username (Me info) =
    info.username


bio : Me -> Maybe String
bio (Me info) =
    info.bio


avatar : Me -> Avatar
avatar (Me info) =
    info.avatar


email : Me -> String
email (Me info) =
    info.email



-- SESSION MANAGEMENT


login : { r | email : String, password : String } -> Http.Request ( Me, AuthToken )
login params =
    let
        user =
            Encode.object
                [ ( "email", Encode.string params.email )
                , ( "password", Encode.string params.password )
                ]

        body =
            Encode.object [ ( "user", user ) ]
                |> Http.jsonBody
    in
    Decode.field "user" decoderWithToken
        |> Http.post (Api.url [ "users", "login" ]) body



-- REGISTER


register : { r | username : String, email : String, password : String } -> Http.Request ( Me, AuthToken )
register params =
    let
        user =
            Encode.object
                [ ( "username", Encode.string params.username )
                , ( "email", Encode.string params.email )
                , ( "password", Encode.string params.password )
                ]

        body =
            Encode.object [ ( "user", user ) ]
                |> Http.jsonBody
    in
    Decode.field "user" decoderWithToken
        |> Http.post (Api.url [ "users" ]) body



-- SERIALIZATION


decoder : Decoder Me
decoder =
    Decode.succeed Internals
        |> required "username" Username.decoder
        |> required "bio" (Decode.nullable Decode.string)
        |> required "avatar" Avatar.decoder
        |> required "email" Decode.string
        |> Decode.map Me


decoderWithToken : Decoder ( Me, AuthToken )
decoderWithToken =
    Decode.succeed Tuple.pair
        |> custom decoder
        |> required "token" AuthToken.decoder
