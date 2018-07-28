module Page.Login exposing (ExternalMsg(..), Model, Msg, initialModel, update, view)

{-| The login page.
-}

import Api
import AuthToken exposing (AuthToken)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, field, string)
import Json.Decode.Pipeline exposing (optional)
import Json.Encode as Encode
import Me exposing (Me)
import Route exposing (Route)
import Session exposing (Session)
import Validate exposing (Valid, Validator, fromValid, ifBlank, validate)
import Views.Form as Form



-- MODEL


type alias Model =
    { errors : List Error
    , form : Form
    }


type alias Form =
    { email : String
    , password : String
    }


initialModel : Model
initialModel =
    { errors = []
    , form =
        { email = ""
        , password = ""
        }
    }



-- VIEW


view : Session -> Model -> { title : String, content : Html Msg }
view session model =
    { title = "Login"
    , content =
        div [ class "auth-page" ]
            [ div [ class "container page" ]
                [ div [ class "row" ]
                    [ div [ class "col-md-6 offset-md-3 col-xs-12" ]
                        [ h1 [ class "text-xs-center" ] [ text "Sign in" ]
                        , p [ class "text-xs-center" ]
                            [ a [ Route.href Route.Register ]
                                [ text "Need an account?" ]
                            ]
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
        [ Form.input
            [ class "form-control-lg"
            , placeholder "Email"
            , onInput EnteredEmail
            , value form.email
            ]
            []
        , Form.password
            [ class "form-control-lg"
            , placeholder "Password"
            , onInput EnteredPassword
            , value form.password
            ]
            []
        , button [ class "btn btn-lg btn-primary pull-xs-right" ]
            [ text "Sign in" ]
        ]



-- UPDATE


type Msg
    = SubmittedForm
    | EnteredEmail String
    | EnteredPassword String
    | CompletedLogin (Result Http.Error ( Me, AuthToken ))


type ExternalMsg
    = NoOp
    | ChangedMeAndToken ( Me, AuthToken )


update : Nav.Key -> Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update navKey msg model =
    case msg of
        SubmittedForm ->
            case validate formValidator model.form of
                Ok validForm ->
                    ( ( { model | errors = [] }
                      , Http.send CompletedLogin (login validForm)
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

        EnteredPassword password ->
            updateForm (\form -> { form | password = password }) model

        CompletedLogin (Err error) ->
            let
                serverErrors =
                    error
                        |> Api.listErrors "errors" errorsDecoder
                        |> List.map (\errorMessage -> ( Server, errorMessage ))
            in
            ( ( { model | errors = List.append model.errors serverErrors }
              , Cmd.none
              )
            , NoOp
            )

        CompletedLogin (Ok (( me, authToken ) as pair)) ->
            ( ( model
              , Cmd.batch [ Session.store me authToken, Route.replaceUrl navKey Route.Home ]
              )
            , ChangedMeAndToken pair
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
    | Email
    | Password


{-| Recording validation errors on a per-field basis facilitates displaying
them inline next to the field where the error occurred.

I implemented it this way out of habit, then realized the spec called for
displaying all the errors at the top. I thought about simplifying it, but then
figured it'd be useful to show how I would normally model this data - assuming
the intended UX was to render errors per field.

(The other part of this is having a view function like this:

viewFormErrors : Field -> List Error -> Html msg

...and it filters the list of errors to render only the ones for the given
Field. This way you can call this:

viewFormErrors Email model.errors

...next to the `email` field, and call `viewFormErrors Password model.errors`
next to the `password` field, and so on.

-}
type alias Error =
    ( ErrorSource, String )


formValidator : Validator Error Form
formValidator =
    Validate.all
        [ ifBlank .email ( Email, "email can't be blank." )
        , ifBlank .password ( Password, "password can't be blank." )
        ]


errorsDecoder : Decoder (List String)
errorsDecoder =
    Decode.succeed (\emailOrPassword email username password -> List.concat [ emailOrPassword, email, username, password ])
        |> optionalError "email or password"
        |> optionalError "email"
        |> optionalError "username"
        |> optionalError "password"


optionalError : String -> Decoder (List String -> a) -> Decoder a
optionalError fieldName =
    let
        errorToString errorMessage =
            String.join " " [ fieldName, errorMessage ]
    in
    optional fieldName (Decode.list (Decode.map errorToString string)) []



-- HTTP


login : Valid Form -> Http.Request ( Me, AuthToken )
login validForm =
    let
        form =
            fromValid validForm

        user =
            Encode.object
                [ ( "email", Encode.string form.email )
                , ( "password", Encode.string form.password )
                ]

        body =
            Encode.object [ ( "user", user ) ]
                |> Http.jsonBody
    in
    Decode.field "user" Me.decoderWithToken
        |> Http.post (Api.url [ "users", "login" ]) body
