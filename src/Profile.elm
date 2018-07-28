module Profile exposing (Profile, avatar, bio, decoder, fetch, follow, following, toggleFollow, username)

{-| A user's profile - potentially your own!

Contrast with Me, which is the currently signed-in user.

-}

import Api
import AuthToken exposing (AuthToken, withAuthorization)
import Avatar exposing (Avatar)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Username exposing (Username)



-- TYPES


type Profile
    = Profile Internals


type alias Internals =
    { username : Username
    , bio : Maybe String
    , avatar : Avatar
    , following : Bool
    }



-- INFO


username : Profile -> Username
username (Profile info) =
    info.username


bio : Profile -> Maybe String
bio (Profile info) =
    info.bio


avatar : Profile -> Avatar
avatar (Profile info) =
    info.avatar


following : Profile -> Bool
following (Profile info) =
    info.following



-- PROFILE


fetch : Username -> Maybe AuthToken -> Http.Request Profile
fetch uname maybeToken =
    Api.url [ "profiles", Username.toString uname ]
        |> HttpBuilder.get
        |> HttpBuilder.withExpect (Http.expectJson (Decode.field "profile" decoder))
        |> withAuthorization maybeToken
        |> HttpBuilder.toRequest



-- FOLLOWING


follow : Bool -> Profile -> Profile
follow isFollowing (Profile info) =
    Profile { info | following = isFollowing }


toggleFollow : Username -> Bool -> AuthToken -> Http.Request Profile
toggleFollow uname isFollowing authToken =
    if isFollowing then
        requestUnfollow uname authToken

    else
        requestFollow uname authToken


requestFollow : Username -> AuthToken -> Http.Request Profile
requestFollow =
    buildFollow HttpBuilder.post


requestUnfollow : Username -> AuthToken -> Http.Request Profile
requestUnfollow =
    buildFollow HttpBuilder.delete


buildFollow :
    (String -> RequestBuilder a)
    -> Username
    -> AuthToken
    -> Http.Request Profile
buildFollow builderFromUrl uname token =
    Api.url [ "profiles", Username.toString uname, "follow" ]
        |> builderFromUrl
        |> withAuthorization (Just token)
        |> withExpect (Http.expectJson (Decode.field "profile" decoder))
        |> HttpBuilder.toRequest



-- SERIALIZATION


decoder : Decoder Profile
decoder =
    Decode.succeed Internals
        |> required "username" Username.decoder
        |> required "bio" (Decode.nullable Decode.string)
        |> required "image" Avatar.decoder
        |> required "following" Decode.bool
        |> Decode.map Profile
