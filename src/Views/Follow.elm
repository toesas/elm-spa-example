module Views.Follow exposing (followButton, unfollowButton)

{-| The Follow button.

This API accepts a "toggle follow" message and the current state of whether
the user is already being followed. It's very lightweight!

It would be overkill to give something this simple its own Model, Msg, and
update. That would make it way more work to use than it needed to be,
and for no benefit.

-}

import Author exposing (FollowedAuthor, UnfollowedAuthor)
import Html exposing (Html, i, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Profile exposing (Profile)
import Username exposing (Username)


followButton : (UnfollowedAuthor -> msg) -> UnfollowedAuthor -> Html msg
followButton toMsg author =
    toggleFollowButton "Follow"
        [ "btn-outline-secondary" ]
        (toMsg author)
        (Author.unfollowedUsername author)


unfollowButton : (FollowedAuthor -> msg) -> FollowedAuthor -> Html msg
unfollowButton toMsg author =
    toggleFollowButton "Unfollow"
        [ "btn-secondary" ]
        (toMsg author)
        (Author.followedUsername author)


toggleFollowButton : String -> List String -> msg -> Username -> Html msg
toggleFollowButton txt extraClasses msgWhenClicked username =
    let
        classStr =
            "btn btn-sm " ++ String.join " " extraClasses ++ " action-btn"

        caption =
            " " ++ txt ++ " " ++ Username.toString username
    in
    Html.button [ class classStr, onClick msgWhenClicked ]
        [ i [ class "ion-plus-round" ] []
        , text caption
        ]
