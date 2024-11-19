import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html exposing (..)
import Http exposing (..)
import Json.Dedcode exposing (..)


main = 
  Browser.element 
    { init = init
    , update = update 
    , subscriptions = subscriptions
    , view = view
    }
type Model = Failure 
           | Loading 
           | Success ServerResponse

type alias ServerResponse = 
      { parseError : Maybe String 
      , evaluation : Maybe Bool 
      }
init : () -> (Model, Cmd Msg)
init _ = 
  (Loading, getServerResponse)

type Msg = MorePlease 
         | GotResult (Result Http.Error ServerResponse)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
   case msg of 
    MorePlease -> 
     (Loading, getServerResponse)
     (GotResult result) -> 
       case result of 
        Ok r -> 
         (Success r, Cmd.none)
        (Err _) -> 
        (Failure, Cmd.none)


subscriptions : Model -> Sub Msg 
subscriptions model = 
   Sub.none 
view : Model -> Html Msg 
view model = 
  div [] 
  [h2 [] [text "Logic Calcluator"]
  ,viewServerResponse model 
  ]

viewServerResponse : Model -> Html Msg 
viewServerResponse model = 
   case model of 
    Failure -> 
      div [] 
      [ text "I could not get a server response"
      , button [onClick MorePlease] [text "try again"]
      ]
    Loading -> 
     text "Loading"
    Success serverResponse -> 
     case serverResponse.evaluation of 
      Nothing -> div []
                 [button [onClick MorePlease, style "display" "block"] [text "More Please!"]
                 , blockquote [] [text serverResponse.]
    ]
