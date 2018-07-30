module Page.Errored exposing (PageLoadError, pageLoadError, view)

{-| The page that renders when there was an error trying to load another page,
for example a Page Not Found error.

It includes a photo I took of a painting on a building in San Francisco,
of a giant walrus exploding the golden gate bridge with laser beams. Pew pew!

-}

import Html exposing (Html, div, h1, img, main_, p, text)
import Html.Attributes exposing (alt, class, id, tabindex)
import Session exposing (Session)
import Views.Page exposing (ActivePage)



-- MODEL --


type PageLoadError
    = PageLoadError Model


type alias Model =
    { activePage : ActivePage
    , errorAuthTokenssage : String
    }


pageLoadError : ActivePage -> String -> PageLoadError
pageLoadError activePage errorAuthTokenssage =
    PageLoadError { activePage = activePage, errorAuthTokenssage = errorAuthTokenssage }



-- VIEW --


view : Session -> PageLoadError -> { title : String, content : Html msg }
view session (PageLoadError model) =
    { title = "Error"
    , content =
        main_ [ id "content", class "container", tabindex -1 ]
            [ h1 [] [ text "Error Loading Page" ]
            , div [ class "row" ]
                [ p [] [ text model.errorAuthTokenssage ] ]
            ]
    }
