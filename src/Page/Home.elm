module Page.Home exposing (Model, Msg, init, update, view)

{-| The homepage. You can get here via either the / or /#/ routes.
-}

import Article
import Article.FeedSources as FeedSources exposing (FeedSources, Source(..))
import Article.Tag as Tag exposing (Tag)
import AuthToken exposing (AuthToken)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder)
import Html.Events exposing (onClick)
import Http
import Me exposing (Me)
import Page.Errored exposing (PageLoadError, pageLoadError)
import Session exposing (Session)
import Task exposing (Task)
import Time
import Views.Article.Feed as Feed
import Views.Page as Page



-- MODEL


type alias Model =
    { tags : List Tag
    , feed : Feed.Model
    }


init : Maybe Me -> Task PageLoadError Model
init maybeMe =
    let
        feedSources =
            case maybeMe of
                Just _ ->
                    FeedSources.fromLists YourFeed [ GlobalFeed ]

                Nothing ->
                    FeedSources.fromLists GlobalFeed []

        loadTags =
            Tag.list
                |> Http.toTask

        loadSources =
            Feed.init maybeMe feedSources

        handleLoadError _ =
            pageLoadError Page.Home "Homepage is currently unavailable."
    in
    Task.map2 Model loadTags loadSources
        |> Task.mapError handleLoadError



-- VIEW


view : Session -> Model -> { title : String, content : Html Msg }
view session model =
    { title = "Conduit"
    , content =
        div [ class "home-page" ]
            [ viewBanner
            , div [ class "container page" ]
                [ div [ class "row" ]
                    [ div [ class "col-md-9" ] (viewFeed (Session.timeZone session) model.feed)
                    , div [ class "col-md-3" ]
                        [ div [ class "sidebar" ]
                            [ p [] [ text "Popular Tags" ]
                            , viewTags model.tags
                            ]
                        ]
                    ]
                ]
            ]
    }


viewBanner : Html msg
viewBanner =
    div [ class "banner" ]
        [ div [ class "container" ]
            [ h1 [ class "logo-font" ] [ text "conduit" ]
            , p [] [ text "A place to share your knowledge." ]
            ]
        ]


viewFeed : Time.Zone -> Feed.Model -> List (Html Msg)
viewFeed timeZone feed =
    div [ class "feed-toggle" ]
        [ Feed.viewFeedSources feed |> Html.map GotFeedMsg ]
        :: (Feed.viewArticles timeZone feed |> List.map (Html.map GotFeedMsg))


viewTags : List Tag -> Html Msg
viewTags tags =
    div [ class "tag-list" ] (List.map viewTag tags)


viewTag : Tag -> Html Msg
viewTag tagName =
    a
        [ class "tag-pill tag-default"
        , onClick (ClickedTag tagName)

        -- The RealWorld CSS requires an href to work properly.
        , href ""
        ]
        [ text (Tag.toString tagName) ]



-- UPDATE


type Msg
    = ClickedTag Tag
    | GotFeedMsg Feed.Msg


update : Maybe Me -> Msg -> Model -> ( Model, Cmd Msg )
update maybeMe msg model =
    case msg of
        GotFeedMsg subMsg ->
            let
                ( newFeed, subCmd ) =
                    Feed.update maybeMe subMsg model.feed
            in
            ( { model | feed = newFeed }, Cmd.map GotFeedMsg subCmd )

        ClickedTag tagName ->
            let
                subCmd =
                    Feed.selectTag maybeMe tagName
            in
            ( model, Cmd.map GotFeedMsg subCmd )
