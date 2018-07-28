module Page.Register exposing (ExternalMsg(..), Model, Msg, initialModel, update, view)

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
    , username : String
    , password : String
    }


initialModel : Model
initialModel =
    { errors = []
    , form =
        { email = ""
        , username = ""
        , password = ""
        }
    }



-- VIEW


view : Session -> Model -> { title : String, content : Html Msg }
view session model =
    { title = "Register"
    , content =
        div [ class "auth-page" ]
            [ div [ class "container page" ]
                [ div [ class "row" ]
                    [ div [ class "col-md-6 offset-md-3 col-xs-12" ]
                        [ h1 [ class "text-xs-center" ] [ text "Sign up" ]
                        , p [ class "text-xs-center" ]
                            [ a [ Route.href Route.Login ]
                                [ text "Have an account?" ]
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
            , placeholder "Username"
            , onInput EnteredUsername
            , value form.username
            ]
            []
        , Form.input
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
            [ text "Sign up" ]
        ]



-- UPDATE


type Msg
    = SubmittedForm
    | EnteredEmail String
    | EnteredUsername String
    | EnteredPassword String
    | CompletedRegister (Result Http.Error ( Me, AuthToken ))


type ExternalMsg
    = NoOp
    | ChangedMeAndToken ( Me, AuthToken )


update : Nav.Key -> Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update navKey msg model =
    case msg of
        SubmittedForm ->
            ( case validate formValidator model.form of
                Ok validForm ->
                    ( { model | errors = [] }
                    , Http.send CompletedRegister (register validForm)
                    )

                Err errors ->
                    ( { model | errors = errors }
                    , Cmd.none
                    )
            , NoOp
            )

        EnteredUsername username ->
            updateForm (\form -> { form | username = username }) model

        EnteredEmail email ->
            updateForm (\form -> { form | email = email }) model

        EnteredPassword password ->
            updateForm (\form -> { form | password = password }) model

        CompletedRegister (Err error) ->
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

        CompletedRegister (Ok (( me, authToken ) as pair)) ->
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
    | Username
    | Email
    | Password


type alias Error =
    ( ErrorSource, String )


formValidator : Validator Error Form
formValidator =
    Validate.all
        [ ifBlank .username ( Username, "username can't be blank." )
        , ifBlank .email ( Email, "email can't be blank." )
        , Validate.fromErrors passwordLength
        ]


minPasswordChars : Int
minPasswordChars =
    6


passwordLength : Form -> List Error
passwordLength { password } =
    if String.length password < minPasswordChars then
        [ ( Password, "password must be at least " ++ String.fromInt minPasswordChars ++ " characters long." ) ]

    else
        []


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
    optional fieldName (Decode.list (Decode.map errorToString string)) []



-- HTTP


register : Valid Form -> Http.Request ( Me, AuthToken )
register validForm =
    let
        form =
            fromValid validForm

        user =
            Encode.object
                [ ( "username", Encode.string form.username )
                , ( "email", Encode.string form.email )
                , ( "password", Encode.string form.password )
                ]

        body =
            Encode.object [ ( "user", user ) ]
                |> Http.jsonBody
    in
    Decode.field "user" Me.decoderWithToken
        |> Http.post (Api.url [ "users" ]) body
