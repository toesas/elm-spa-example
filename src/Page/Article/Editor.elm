module Page.Article.Editor exposing (Model, Msg, initEdit, initNew, update, view)

import Article exposing (Article, Full)
import Article.Body exposing (Body)
import Article.Slug as Slug exposing (Slug)
import AuthToken exposing (AuthToken)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (attribute, class, disabled, href, id, placeholder, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http
import Page.Errored exposing (PageLoadError, pageLoadError)
import Profile exposing (Profile)
import Route
import Session exposing (Session)
import Task exposing (Task)
import Validate exposing (Validator, ifBlank, validate)
import Views.Form as Form
import Views.Page as Page



-- MODEL


type alias Model =
    { errors : List Error
    , editingArticle : Maybe Slug
    , title : String
    , body : String
    , description : String
    , tags : List String
    , isSaving : Bool
    }


initNew : Model
initNew =
    { errors = []
    , editingArticle = Nothing
    , title = ""
    , body = ""
    , description = ""
    , tags = []
    , isSaving = False
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
                , title = meta.title
                , body = Article.Body.toMarkdownString (Article.body article)
                , description = meta.description
                , tags = meta.tags
                , isSaving = False
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
                        , viewForm model
                        ]
                    ]
                ]
            ]
    }


viewForm : Model -> Html Msg
viewForm model =
    let
        isEditing =
            model.editingArticle /= Nothing

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
                , value model.title
                ]
                []
            , Form.input
                [ placeholder "What's this article about?"
                , onInput EnteredDescription
                , value model.description
                ]
                []
            , Form.textarea
                [ placeholder "Write your article (in markdown)"
                , attribute "rows" "8"
                , onInput EnteredBody
                , value model.body
                ]
                []
            , Form.input
                [ placeholder "Enter tags"
                , onInput EnteredTags
                , value (String.join " " model.tags)
                ]
                []
            , button [ class "btn btn-lg pull-xs-right btn-primary", disabled model.isSaving ]
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
update token navKey msg model =
    case msg of
        ClickedSave ->
            case validate modelValidator model of
                [] ->
                    case model.editingArticle of
                        Nothing ->
                            token
                                |> Article.create model
                                |> Http.send CompletedCreate
                                |> Tuple.pair { model | errors = [], isSaving = True }

                        Just slug ->
                            token
                                |> Article.update slug model
                                |> Http.send CompletedEdit
                                |> Tuple.pair { model | errors = [], isSaving = True }

                errors ->
                    ( { model | errors = errors }, Cmd.none )

        EnteredTitle title ->
            ( { model | title = title }, Cmd.none )

        EnteredDescription description ->
            ( { model | description = description }, Cmd.none )

        EnteredTags tags ->
            ( { model | tags = tagsFromString tags }, Cmd.none )

        EnteredBody body ->
            ( { model | body = body }, Cmd.none )

        CompletedCreate (Ok article) ->
            Route.Article (Article.slug article)
                |> Route.replaceUrl navKey
                |> Tuple.pair model

        CompletedCreate (Err error) ->
            ( { model
                | errors = model.errors ++ [ ( Form, "Server error while attempting to publish article" ) ]
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
                | errors = model.errors ++ [ ( Form, "Server error while attempting to save article" ) ]
                , isSaving = False
              }
            , Cmd.none
            )



-- VALIDATION


type Field
    = Form
    | Title
    | Body


type alias Error =
    ( Field, String )


modelValidator : Validator Error Model
modelValidator =
    Validate.all
        [ ifBlank .title ( Title, "title can't be blank." )
        , ifBlank .body ( Body, "body can't be blank." )
        ]



-- INTERNAL


tagsFromString : String -> List String
tagsFromString str =
    str
        |> String.split " "
        |> List.map String.trim
        |> List.filter (not << String.isEmpty)
