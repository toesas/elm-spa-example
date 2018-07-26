module Page.Register exposing (ExternalMsg(..), Model, Msg, initialModel, update, view)

import AuthToken exposing (AuthToken)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, field, string)
import Json.Decode.Pipeline exposing (optional)
import Me exposing (Me)
import Route exposing (Route)
import Session exposing (Session)
import Validate exposing (Validator, ifBlank, validate)
import Views.Form as Form



-- MODEL


type alias Model =
    { errors : List Error
    , email : String
    , username : String
    , password : String
    }


initialModel : Model
initialModel =
    { errors = []
    , email = ""
    , username = ""
    , password = ""
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
                        , viewForm
                        ]
                    ]
                ]
            ]
    }


viewForm : Html Msg
viewForm =
    Html.form [ onSubmit SubmittedForm ]
        [ Form.input
            [ class "form-control-lg"
            , placeholder "Username"
            , onInput EnteredUsername
            ]
            []
        , Form.input
            [ class "form-control-lg"
            , placeholder "Email"
            , onInput EnteredEmail
            ]
            []
        , Form.password
            [ class "form-control-lg"
            , placeholder "Password"
            , onInput EnteredPassword
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
            case validate modelValidator model of
                [] ->
                    ( ( { model | errors = [] }
                      , Http.send CompletedRegister (Me.register model)
                      )
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

        EnteredPassword password ->
            ( ( { model | password = password }
              , Cmd.none
              )
            , NoOp
            )

        CompletedRegister (Err error) ->
            let
                errorMessages =
                    case error of
                        Http.BadStatus response ->
                            response.body
                                |> decodeString (field "errors" errorsDecoder)
                                |> Result.withDefault []

                        _ ->
                            [ "unable to process registration" ]
            in
            ( ( { model | errors = List.map (\errorMessage -> ( Form, errorMessage )) errorMessages }
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



-- VALIDATION


type Field
    = Form
    | Username
    | Email
    | Password


type alias Error =
    ( Field, String )


modelValidator : Validator Error Model
modelValidator =
    Validate.all
        [ ifBlank .username ( Username, "username can't be blank." )
        , ifBlank .email ( Email, "email can't be blank." )
        , Validate.fromErrors passwordLength
        ]


minPasswordChars : Int
minPasswordChars =
    6


passwordLength : Model -> List Error
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
