module Page.Settings exposing (ExternalMsg(..), Model, Msg, init, update, view)

import Api
import AuthToken exposing (AuthToken, addAuthHeader)
import Avatar
import Browser.Navigation as Nav
import Email exposing (Email)
import Html exposing (Html, button, div, fieldset, h1, input, text, textarea)
import Html.Attributes exposing (attribute, class, placeholder, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http
import HttpBuilder
import Json.Decode as Decode exposing (Decoder, decodeString, field, list, string)
import Json.Decode.Pipeline exposing (optional)
import Json.Encode as Encode
import Profile exposing (Profile)
import Route
import Session exposing (LoggedInUser, Session)
import Username as Username exposing (Username)
import Validate exposing (Valid, Validator, fromValid, ifBlank, validate)
import Views.Form as Form



-- MODEL


type alias Model =
    { errors : List Error
    , form : Form
    }


type alias Form =
    { avatar : Maybe String
    , email : String
    , bio : String
    , username : String
    , password : Maybe String
    }


init : AuthToken -> Email -> Profile -> Model
init authToken email profile =
    { errors = []
    , form =
        { avatar = Avatar.toMaybeString (Profile.avatar profile)
        , email = Email.toString email
        , bio = Maybe.withDefault "" (Profile.bio profile)
        , username = Username.toString (AuthToken.username authToken)
        , password = Nothing
        }
    }


{-| A form that has been validated. Only the `edit` function uses this. Its
purpose is to prevent us from forgetting to validate the form before passing
it to `edit`.

This doesn't create any guarantees that the form was actually validated. If
we wanted to do that, we'd need to move the form data into a separate module!

-}
type ValidForm
    = Valid Form



-- VIEW


view : Session -> Model -> { title : String, content : Html Msg }
view session model =
    { title = "Enteredtings"
    , content =
        div [ class "settings-page" ]
            [ div [ class "container page" ]
                [ div [ class "row" ]
                    [ div [ class "col-md-6 offset-md-3 col-xs-12" ]
                        [ h1 [ class "text-xs-center" ] [ text "Your Enteredtings" ]
                        , Form.viewErrors model.errors
                        , viewForm model.form
                        ]
                    ]
                ]
            ]
    }


viewForm : Form -> Html Msg
viewForm form =
    Html.form [ onSubmit SubmittedForm ]
        [ fieldset []
            [ Form.input
                [ placeholder "URL of profile picture"
                , value (Maybe.withDefault "" form.avatar)
                , onInput EnteredImage
                ]
                []
            , Form.input
                [ class "form-control-lg"
                , placeholder "Username"
                , value form.username
                , onInput EnteredUsername
                ]
                []
            , Form.textarea
                [ class "form-control-lg"
                , placeholder "Short bio about you"
                , attribute "rows" "8"
                , value form.bio
                , onInput EnteredBio
                ]
                []
            , Form.input
                [ class "form-control-lg"
                , placeholder "Email"
                , value form.email
                , onInput EnteredEmail
                ]
                []
            , Form.password
                [ class "form-control-lg"
                , placeholder "Password"
                , value (Maybe.withDefault "" form.password)
                , onInput EnteredPassword
                ]
                []
            , button
                [ class "btn btn-lg btn-primary pull-xs-right" ]
                [ text "Update Enteredtings" ]
            ]
        ]



-- UPDATE


type Msg
    = SubmittedForm
    | EnteredEmail String
    | EnteredUsername String
    | EnteredPassword String
    | EnteredBio String
    | EnteredImage String
    | CompletedSave (Result Http.Error LoggedInUser)


type ExternalMsg
    = NoOp
    | ChangedLoggedInUser LoggedInUser


update : Nav.Key -> AuthToken -> Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update navKey authToken msg model =
    case msg of
        SubmittedForm ->
            case validate formValidator model.form of
                Ok validForm ->
                    ( ( { model | errors = [] }
                      , edit authToken validForm
                            |> Http.send CompletedSave
                      )
                    , NoOp
                    )

                Err errors ->
                    ( ( { model | errors = errors }
                      , Cmd.none
                      )
                    , NoOp
                    )

        EnteredEmail email ->
            updateForm (\form -> { form | email = email }) model

        EnteredUsername username ->
            updateForm (\form -> { form | username = username }) model

        EnteredPassword passwordStr ->
            let
                password =
                    if String.isEmpty passwordStr then
                        Nothing

                    else
                        Just passwordStr
            in
            updateForm (\form -> { form | password = password }) model

        EnteredBio bio ->
            updateForm (\form -> { form | bio = bio }) model

        EnteredImage avatarStr ->
            let
                avatar =
                    if String.isEmpty avatarStr then
                        Nothing

                    else
                        Just avatarStr
            in
            updateForm (\form -> { form | avatar = avatar }) model

        CompletedSave (Err error) ->
            let
                serverErrors =
                    error
                        |> Api.listErrors "errors" errorsDecoder
                        |> List.map (\errorAuthTokenssage -> ( Server, errorAuthTokenssage ))
            in
            ( ( { model | errors = List.append model.errors serverErrors }
              , Cmd.none
              )
            , NoOp
            )

        CompletedSave (Ok loggedInUser) ->
            ( ( model
              , Route.replaceUrl navKey Route.Home
              )
            , ChangedLoggedInUser loggedInUser
            )


{-| Helper function for `update`. Updates the form and returns Cmd.none and
NoOp. Useful for recording form fields!
-}
updateForm : (Form -> Form) -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
updateForm transform model =
    ( ( { model | form = transform model.form }, Cmd.none ), NoOp )



-- VALIDATION


type ErrorSource
    = Server
    | Username
    | Email
    | Password
    | ImageUrl
    | Bio


type alias Error =
    ( ErrorSource, String )


formValidator : Validator Error Form
formValidator =
    Validate.all
        [ ifBlank .username ( Username, "username can't be blank." )
        , ifBlank .email ( Email, "email can't be blank." )
        ]


errorsDecoder : Decoder (List String)
errorsDecoder =
    Decode.succeed (\email username password -> List.concat [ email, username, password ])
        |> optionalError "email"
        |> optionalError "username"
        |> optionalError "password"


optionalError : String -> Decoder (List String -> a) -> Decoder a
optionalError fieldName =
    let
        errorToString errorAuthTokenssage =
            String.join " " [ fieldName, errorAuthTokenssage ]
    in
    optional fieldName (list (Decode.map errorToString string)) []



-- HTTP


{-| This takes a Valid Form as a reminder that it needs to have been validated
first.
-}
edit : AuthToken -> Valid Form -> Http.Request LoggedInUser
edit authToken validForm =
    let
        form =
            fromValid validForm

        updates =
            [ Just ( "username", Encode.string form.username )
            , Just ( "email", Encode.string form.email )
            , Just ( "bio", Encode.string form.bio )
            , Just ( "image", Maybe.withDefault Encode.null (Maybe.map Encode.string form.avatar) )
            , Maybe.map (\pass -> ( "password", Encode.string pass )) form.password
            ]
                |> List.filterMap identity

        body =
            ( "user", Encode.object updates )
                |> List.singleton
                |> Encode.object
                |> Http.jsonBody

        expect =
            Decode.field "user" Session.loggedInUserDecoder
                |> Http.expectJson
    in
    Api.url [ "user" ]
        |> HttpBuilder.put
        |> HttpBuilder.withExpect expect
        |> HttpBuilder.withBody body
        |> addAuthHeader (Just authToken)
        |> HttpBuilder.toRequest
