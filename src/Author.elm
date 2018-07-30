module Author
    exposing
        ( Author(..)
        , FollowedAuthor
        , UnfollowedAuthor
        , decoder
        , fetch
        , follow
        , followedProfile
        , followedUsername
        , profile
        , requestFollow
        , requestUnfollow
        , unfollow
        , unfollowedProfile
        , unfollowedUsername
        , username
        )

{-| The author of an Article. It includes a Profile.

I designed this to make sure the compiler would help me keep these three
possibilities straight when displaying follow buttons and such:

  - I'm following this author.
  - I'm not following this author.
  - I _can't_ follow this author, because it's me!

To do this, I defined `Author` a custom type with three variants, one for each
of those possibilities.

I also made separate types for FollowedAuthor and UnfollowedAuthor.
They are custom type wrappers around Profile, and thier sole purpose is to
help me keep track of which operations are supported.

For example, consider these functions:

requestFollow : UnfollowedAuthor -> AuthToken -> Http.Request Author
requestUnfollow : FollowedAuthor -> AuthToken -> Http.Request Author

These types help the compiler prevent several mistakes:

  - Displaying a Follow button for an author the user already follows.
  - Displaying an Unfollow button for an author the user already doesn't follow.
  - Displaying either button when the author is ourself.

There are still ways we could mess things up (e.g. make a button that calls Author.unfollow when you click it, but which displays "Follow" to the user) - but this rules out a bunch of potential problems.

-}

import Api
import AuthToken exposing (AuthToken, addAuthHeader)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, required)
import Json.Encode as Encode exposing (Value)
import Profile exposing (Profile)
import Username exposing (Username)


{-| An author - either the current user, another user we're following, or
another user we aren't following.

These distinctions matter because we can only perform "follow" requests for
users we aren't following, we can only perform "unfollow" requests for
users we _are_ following, and we can't perform either for ourselves.

-}
type Author
    = IsFollowing FollowedAuthor
    | IsNotFollowing UnfollowedAuthor
    | IsAuthToken Username Profile


{-| An author we're following.
-}
type FollowedAuthor
    = FollowedAuthor Username Profile


{-| An author we're not following.
-}
type UnfollowedAuthor
    = UnfollowedAuthor Username Profile


{-| Return an Author's username.
-}
username : Author -> Username
username author =
    case author of
        IsAuthToken val _ ->
            val

        IsFollowing (FollowedAuthor val _) ->
            val

        IsNotFollowing (UnfollowedAuthor val _) ->
            val


{-| Return an Author's profile.
-}
profile : Author -> Profile
profile author =
    case author of
        IsAuthToken _ val ->
            val

        IsFollowing (FollowedAuthor _ val) ->
            val

        IsNotFollowing (UnfollowedAuthor _ val) ->
            val


{-| Return a FollowedAuthor's uname.
-}
followedUsername : FollowedAuthor -> Username
followedUsername (FollowedAuthor val _) =
    val


{-| Return a FollowedAuthor's profile.
-}
followedProfile : FollowedAuthor -> Profile
followedProfile (FollowedAuthor _ val) =
    val


{-| Return an UnfollowedAuthor's profile.
-}
unfollowedUsername : UnfollowedAuthor -> Username
unfollowedUsername (UnfollowedAuthor val _) =
    val


{-| Return an UnfollowedAuthor's profile.
-}
unfollowedProfile : UnfollowedAuthor -> Profile
unfollowedProfile (UnfollowedAuthor _ val) =
    val



-- FETCH


fetch : Username -> Maybe AuthToken -> Http.Request Author
fetch uname maybeToken =
    Api.url [ "profiles", Username.toString uname ]
        |> HttpBuilder.get
        |> HttpBuilder.withExpect (Http.expectJson (Decode.field "profile" (decoder maybeToken)))
        |> AuthToken.addAuthHeader maybeToken
        |> HttpBuilder.toRequest



-- FOLLOWING


follow : UnfollowedAuthor -> FollowedAuthor
follow (UnfollowedAuthor uname prof) =
    FollowedAuthor uname prof


unfollow : FollowedAuthor -> UnfollowedAuthor
unfollow (FollowedAuthor uname prof) =
    UnfollowedAuthor uname prof


requestFollow : UnfollowedAuthor -> AuthToken -> Http.Request Author
requestFollow (UnfollowedAuthor uname _) authToken =
    requestHelp HttpBuilder.post uname authToken


requestUnfollow : FollowedAuthor -> AuthToken -> Http.Request Author
requestUnfollow (FollowedAuthor uname _) authToken =
    requestHelp HttpBuilder.delete uname authToken


requestHelp :
    (String -> RequestBuilder a)
    -> Username
    -> AuthToken
    -> Http.Request Author
requestHelp builderFromUrl uname authToken =
    Api.url [ "profiles", Username.toString uname, "follow" ]
        |> builderFromUrl
        |> addAuthHeader (Just authToken)
        |> withExpect (Http.expectJson (Decode.field "profile" (decoder Nothing)))
        |> HttpBuilder.toRequest



-- SERIALIZATION


decoder : Maybe AuthToken -> Decoder Author
decoder maybeToken =
    Decode.succeed Tuple.pair
        |> custom Profile.decoder
        |> required "uname" Username.decoder
        |> Decode.andThen (decodeFromPair maybeToken)


decodeFromPair : Maybe AuthToken -> ( Profile, Username ) -> Decoder Author
decodeFromPair maybeToken ( prof, uname ) =
    let
        authorIsAuthToken =
            case maybeToken of
                Nothing ->
                    False

                Just authToken ->
                    AuthToken.username authToken == uname
    in
    if authorIsAuthToken then
        Decode.succeed (IsAuthToken uname prof)

    else
        -- Only bother decoding the "following" field if it's someone
        -- we could possibly be following!
        Decode.field "following" Decode.bool
            |> Decode.map (authorFromFollowing prof uname)


authorFromFollowing : Profile -> Username -> Bool -> Author
authorFromFollowing prof uname isFollowing =
    if isFollowing then
        IsFollowing (FollowedAuthor uname prof)

    else
        IsNotFollowing (UnfollowedAuthor uname prof)
