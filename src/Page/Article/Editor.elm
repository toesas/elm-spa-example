module Page.Article.Editor exposing (Model, Msg, initEdit, initNew, update, view)

import Api
import Article exposing (Article, Full)
import Article.Body exposing (Body)
import Article.Slug as Slug exposing (Slug)
import AuthToken exposing (AuthToken, addAuthHeader)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (attribute, class, disabled, href, id, placeholder, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http
import HttpBuilder exposing (withBody, withExpect)
import Json.Decode as Decode
import Json.Encode as Encode
import Page.Errored exposing (PageLoadError, pageLoadError)
import Profile exposing (Profile)
import Route
import Session exposing (Session)
import Task exposing (Task)
import Validate exposing (Valid, Validator, fromValid, ifBlank, validate)
import Views.Form as Form
import Views.Page as Page



-- MODEL


type alias Model =
    { editingArticle : Maybe Slug
    , isSaving : Bool
    , errors : List Error
    , form : Form
    }


type alias Form =
    { title : String
    , body : String
    , description : String
    , tags : String
    }


initNew : Model
initNew =
    { errors = []
    , editingArticle = Nothing
    , isSaving = False
    , form =
        { title = ""
        , body = ""
        , description = ""
        , tags = ""
        }
    }


initEdit : Maybe AuthToken -> Slug -> Task PageLoadError Model
initEdit maybeToken slug =
    Article.fetch maybeToken slug
        |> Http.toTask
        |> Task.mapError (\_ -> pageLoadError Page.Other "Article is currently unavailable.")
        |> Task.map
            (\article ->
                let
                    meta =
                        Article.metadata article
                in
                { errors = []
                , editingArticle = Just slug
                , isSaving = False
                , form =
                    { title = meta.title
                    , body = Article.Body.toMarkdownString (Article.body article)
                    , description = meta.description
                    , tags = String.join " " meta.tags
                    }
                }
            )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Edit Article"
    , content =
        div [ class "editor-page" ]
            [ div [ class "container page" ]
                [ div [ class "row" ]
                    [ div [ class "col-md-10 offset-md-1 col-xs-12" ]
                        [ Form.viewErrors model.errors
                        , viewForm model.form
                            { isEditing = model.editingArticle /= Nothing
                            , isSaving = model.isSaving
                            }
                        ]
                    ]
                ]
            ]
    }


viewForm : Form -> { isEditing : Bool, isSaving : Bool } -> Html Msg
viewForm form { isEditing, isSaving } =
    let
        saveButtonText =
            if isEditing then
                "Update Article"

            else
                "Publish Article"
    in
    Html.form [ onSubmit ClickedSave ]
        [ fieldset []
            [ Form.input
                [ class "form-control-lg"
                , placeholder "Article Title"
                , onInput EnteredTitle
                , value form.title
                ]
                []
            , Form.input
                [ placeholder "What's this article about?"
                , onInput EnteredDescription
                , value form.description
                ]
                []
            , Form.textarea
                [ placeholder "Write your article (in markdown)"
                , attribute "rows" "8"
                , onInput EnteredBody
                , value form.body
                ]
                []
            , Form.input
                [ placeholder "Enter tags"
                , onInput EnteredTags
                , value form.tags
                ]
                []
            , button [ class "btn btn-lg pull-xs-right btn-primary", disabled isSaving ]
                [ text saveButtonText ]
            ]
        ]



-- UPDATE


type Msg
    = ClickedSave
    | EnteredBody String
    | EnteredDescription String
    | EnteredTags String
    | EnteredTitle String
    | CompletedCreate (Result Http.Error (Article Full))
    | CompletedEdit (Result Http.Error (Article Full))


update : AuthToken -> Nav.Key -> Msg -> Model -> ( Model, Cmd Msg )
update authToken navKey msg model =
    case msg of
        ClickedSave ->
            case validate formValidator model.form of
                Ok validForm ->
                    ( { model | errors = [], isSaving = True }
                    , case model.editingArticle of
                        Nothing ->
                            create validForm authToken
                                |> Http.send CompletedCreate

                        Just slug ->
                            edit slug validForm authToken
                                |> Http.send CompletedEdit
                    )

                Err errors ->
                    ( { model | errors = errors }, Cmd.none )

        EnteredTitle title ->
            updateForm (\form -> { form | title = title }) model

        EnteredDescription description ->
            updateForm (\form -> { form | description = description }) model

        EnteredTags tags ->
            updateForm (\form -> { form | tags = tags }) model

        EnteredBody body ->
            updateForm (\form -> { form | body = body }) model

        CompletedCreate (Ok article) ->
            Route.Article (Article.slug article)
                |> Route.replaceUrl navKey
                |> Tuple.pair model

        CompletedCreate (Err error) ->
            ( { model
                | errors = model.errors ++ [ ( Server, "Server error while attempting to publish article" ) ]
                , isSaving = False
              }
            , Cmd.none
            )

        CompletedEdit (Ok article) ->
            Route.Article (Article.slug article)
                |> Route.replaceUrl navKey
                |> Tuple.pair model

        CompletedEdit (Err error) ->
            ( { model
                | errors = model.errors ++ [ ( Server, "Server error while attempting to save article" ) ]
                , isSaving = False
              }
            , Cmd.none
            )


{-| Helper function for `update`. Updates the form and returns Cmd.none.
Useful for recording form fields!
-}
updateForm : (Form -> Form) -> Model -> ( Model, Cmd Msg )
updateForm transform model =
    ( { model | form = transform model.form }, Cmd.none )



-- VALIDATION


type ErrorSource
    = Server
    | Title
    | Body


type alias Error =
    ( ErrorSource, String )


formValidator : Validator Error Form
formValidator =
    Validate.all
        [ ifBlank .title ( Title, "title can't be blank." )
        , ifBlank .body ( Body, "body can't be blank." )
        ]



-- HTTP


create : Valid Form -> AuthToken -> Http.Request (Article Full)
create validForm authToken =
    let
        form =
            fromValid validForm

        expect =
            Article.fullDecoder (Just authToken)
                |> Decode.field "article"
                |> Http.expectJson

        article =
            Encode.object
                [ ( "title", Encode.string form.title )
                , ( "description", Encode.string form.description )
                , ( "body", Encode.string form.body )
                , ( "tagList", Encode.list Encode.string (tagsFromString form.tags) )
                ]

        jsonBody =
            Encode.object [ ( "article", article ) ]
                |> Http.jsonBody
    in
    Api.url [ "articles" ]
        |> HttpBuilder.post
        |> addAuthHeader (Just authToken)
        |> withBody jsonBody
        |> withExpect expect
        |> HttpBuilder.toRequest


tagsFromString : String -> List String
tagsFromString str =
    str
        |> String.split " "
        |> List.map String.trim
        |> List.filter (not << String.isEmpty)


edit : Slug -> Valid Form -> AuthToken -> Http.Request (Article Full)
edit articleSlug validForm authToken =
    let
        form =
            fromValid validForm

        expect =
            Article.fullDecoder (Just authToken)
                |> Decode.field "article"
                |> Http.expectJson

        article =
            Encode.object
                [ ( "title", Encode.string form.title )
                , ( "description", Encode.string form.description )
                , ( "body", Encode.string form.body )
                ]

        jsonBody =
            Encode.object [ ( "article", article ) ]
                |> Http.jsonBody
    in
    Article.url articleSlug []
        |> HttpBuilder.put
        |> addAuthHeader (Just authToken)
        |> withBody jsonBody
        |> withExpect expect
        |> HttpBuilder.toRequest
