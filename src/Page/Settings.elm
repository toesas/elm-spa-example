module Page.Settings exposing (ExternalMsg(..), Model, Msg, init, update, view)

import Api
import AuthToken exposing (AuthToken, withAuthorization)
import Avatar
import Browser.Navigation as Nav
import Html exposing (Html, button, div, fieldset, h1, input, text, textarea)
import Html.Attributes exposing (attribute, class, placeholder, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http
import HttpBuilder
import Json.Decode as Decode exposing (Decoder, decodeString, field, list, string)
import Json.Decode.Pipeline exposing (optional)
import Json.Encode as Encode
import Me exposing (Me)
import Route
import Session exposing (Session)
import Username as Username exposing (Username)
import Validate exposing (Validator, ifBlank, validate)
import Views.Form as Form



-- MODEL


type alias Model =
    { errors : List Error
    , avatar : Maybe String
    , email : String
    , bio : String
    , username : String
    , password : Maybe String
    }


init : Me -> Model
init me =
    { errors = []
    , avatar = Avatar.toMaybeString (Me.avatar me)
    , email = Me.email me
    , bio = Maybe.withDefault "" (Me.bio me)
    , username = Username.toString (Me.username me)
    , password = Nothing
    }


{-| A model that has been validated. Only the `edit` function uses this. Its
purpose is to prevent us from forgetting to validate the model before passing
it to `edit`.

This doesn't create any guarantees that the model was actually validated. If
we wanted to do that, we'd need to move the form data into a separate module!

-}
type ValidModel
    = Valid Model



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
                        , viewForm model
                        ]
                    ]
                ]
            ]
    }


viewForm : Model -> Html Msg
viewForm model =
    Html.form [ onSubmit SubmittedForm ]
        [ fieldset []
            [ Form.input
                [ placeholder "URL of profile picture"
                , value (Maybe.withDefault "" model.avatar)
                , onInput EnteredImage
                ]
                []
            , Form.input
                [ class "form-control-lg"
                , placeholder "Username"
                , value model.username
                , onInput EnteredUsername
                ]
                []
            , Form.textarea
                [ class "form-control-lg"
                , placeholder "Short bio about you"
                , attribute "rows" "8"
                , value model.bio
                , onInput EnteredBio
                ]
                []
            , Form.input
                [ class "form-control-lg"
                , placeholder "Email"
                , value model.email
                , onInput EnteredEmail
                ]
                []
            , Form.password
                [ class "form-control-lg"
                , placeholder "Password"
                , value (Maybe.withDefault "" model.password)
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
    | CompletedSave (Result Http.Error Me)


type ExternalMsg
    = NoOp
    | ChangedMe Me


update : Nav.Key -> AuthToken -> Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update navKey authToken msg model =
    case msg of
        SubmittedForm ->
            case validate modelValidator model of
                [] ->
                    ( edit authToken (Valid model)
                        |> Http.send CompletedSave
                        |> Tuple.pair { model | errors = [] }
                    , NoOp
                    )

                errors ->
                    ( ( { model | errors = errors }
                      , Cmd.none
                      )
                    , NoOp
                    )

        EnteredEmail email ->
            ( ( { model | email = email }
              , Cmd.none
              )
            , NoOp
            )

        EnteredUsername username ->
            ( ( { model | username = username }
              , Cmd.none
              )
            , NoOp
            )

        EnteredPassword passwordStr ->
            let
                password =
                    if String.isEmpty passwordStr then
                        Nothing

                    else
                        Just passwordStr
            in
            ( ( { model | password = password }
              , Cmd.none
              )
            , NoOp
            )

        EnteredBio bio ->
            ( ( { model | bio = bio }
              , Cmd.none
              )
            , NoOp
            )

        EnteredImage avatarStr ->
            let
                avatar =
                    if String.isEmpty avatarStr then
                        Nothing

                    else
                        Just avatarStr
            in
            ( ( { model | avatar = avatar }
              , Cmd.none
              )
            , NoOp
            )

        CompletedSave (Err error) ->
            let
                errorMessages =
                    case error of
                        Http.BadStatus response ->
                            response.body
                                |> decodeString (field "errors" errorsDecoder)
                                |> Result.withDefault []

                        _ ->
                            [ "unable to save changes" ]

                errors =
                    errorMessages
                        |> List.map (\errorMessage -> ( Form, errorMessage ))
            in
            ( ( { model | errors = errors }
              , Cmd.none
              )
            , NoOp
            )

        CompletedSave (Ok me) ->
            ( ( model
              , Cmd.batch [ Session.store me authToken, Route.replaceUrl navKey Route.Home ]
              )
            , ChangedMe me
            )



-- VALIDATION


type Field
    = Form
    | Username
    | Email
    | Password
    | ImageUrl
    | Bio


type alias Error =
    ( Field, String )


modelValidator : Validator Error Model
modelValidator =
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
        errorToString errorMessage =
            String.join " " [ fieldName, errorMessage ]
    in
    optional fieldName (list (Decode.map errorToString string)) []



-- HTTP


{-| This takes a ValidModel as a reminder that it needs to have been validated
first.
-}
edit : AuthToken -> ValidModel -> Http.Request Me
edit authToken (Valid params) =
    let
        updates =
            [ Just ( "username", Encode.string params.username )
            , Just ( "email", Encode.string params.email )
            , Just ( "bio", Encode.string params.bio )
            , Just ( "avatar", Maybe.withDefault Encode.null (Maybe.map Encode.string params.avatar) )
            , Maybe.map (\pass -> ( "password", Encode.string pass )) params.password
            ]
                |> List.filterMap identity

        body =
            ( "user", Encode.object updates )
                |> List.singleton
                |> Encode.object
                |> Http.jsonBody

        expect =
            Decode.field "user" Me.decoder
                |> Http.expectJson
    in
    Api.url [ "user" ]
        |> HttpBuilder.put
        |> HttpBuilder.withExpect expect
        |> HttpBuilder.withBody body
        |> withAuthorization (Just authToken)
        |> HttpBuilder.toRequest
