port module Session exposing (LoggedInUser, Session, attempt, authToken, changes, clear, fromValue, init, isLoggedIn, loggedInUser, loggedInUserDecoder, logout, me, store, timeZone, withTimeZone)

import AuthToken exposing (AuthToken)
import Avatar exposing (Avatar)
import Email exposing (Email)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, required)
import Json.Encode as Encode exposing (Value)
import Me exposing (Me)
import Profile exposing (Profile)
import Time
import Username exposing (Username)



-- TYPES


type Session
    = Session Internals


type alias Internals =
    { timeZone : Time.Zone
    , loggedInUser : Maybe LoggedInUser
    }


type alias LoggedInUser =
    { me : Me
    , email : Email
    , profile : Profile
    }



-- CREATE


init : Time.Zone -> Maybe LoggedInUser -> Session
init zone maybeUser =
    Session
        { loggedInUser = maybeUser
        , timeZone = zone
        }



-- TRANSFORM


withTimeZone : Time.Zone -> Session -> Session
withTimeZone zone (Session info) =
    Session { info | timeZone = zone }



-- INFO


isLoggedIn : Session -> Bool
isLoggedIn (Session info) =
    case info.loggedInUser of
        Just _ ->
            True

        Nothing ->
            False


me : Session -> Maybe Me
me (Session info) =
    Maybe.map .me info.loggedInUser


loggedInUser : Session -> Maybe LoggedInUser
loggedInUser (Session info) =
    info.loggedInUser


authToken : Session -> Maybe AuthToken
authToken session =
    me session
        |> Maybe.map Me.authToken


timeZone : Session -> Time.Zone
timeZone (Session info) =
    info.timeZone



-- MODIFY


clear : Session -> Session
clear (Session info) =
    Session { info | loggedInUser = Nothing }



-- ATTEMPT


attempt : String -> (AuthToken -> Cmd msg) -> Session -> Result String (Cmd msg)
attempt attemptedCmd toCmd (Session info) =
    case info.loggedInUser of
        Nothing ->
            Err ("Please sign in to " ++ attemptedCmd ++ ".")

        Just user ->
            Ok (toCmd (Me.authToken user.me))



-- STORE


store : LoggedInUser -> Cmd msg
store user =
    Encode.object
        [ ( "email", Email.encode user.email )
        , ( "username", Username.encode (Me.username user.me) )
        , ( "bio", Maybe.withDefault Encode.null (Maybe.map Encode.string (Profile.bio user.profile)) )
        , ( "image", Avatar.encode (Profile.avatar user.profile) )
        , ( "token", AuthToken.encode (Me.authToken user.me) )
        ]
        |> Encode.encode 0
        |> Just
        |> storeSession


logout : Cmd msg
logout =
    storeSession Nothing


port storeSession : Maybe String -> Cmd msg



-- CHANGES


changes : Time.Zone -> Sub Session
changes tz =
    onSessionChange (fromValue tz)


port onSessionChange : (Value -> msg) -> Sub msg


fromValue : Time.Zone -> Value -> Session
fromValue tz value =
    Session
        { loggedInUser = Result.toMaybe (Decode.decodeValue loggedInUserDecoder value)
        , timeZone = tz
        }


loggedInUserDecoder : Decoder LoggedInUser
loggedInUserDecoder =
    Decode.succeed LoggedInUser
        |> custom Me.decoder
        |> required "email" Email.decoder
        |> custom Profile.decoder
