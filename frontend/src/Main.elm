module Main exposing (..)
import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode exposing (..)
import Maybe exposing (withDefault)


main = 
  Browser.element 
    { init = init
    , update = update 
    , subscriptions = subscriptions
    , view = view
    }
type alias Model =  
           {failure : Bool  
           ,loading : Bool 
           ,textInput   : String 
           ,success : Maybe ServerResponse 
           } 
initialModel =  
             {failure = False
             ,loading = False 
             ,textInput   = ""
             ,success  = Nothing 
             }
type alias ServerResponse = 
      { parseError : String 
      , evaluation : String 
      }

init : () -> (Model, Cmd Msg)
init _ = 
  (initialModel, Cmd.none)

type Msg = Post 
         | GotResult (Result Http.Error ServerResponse)
         | TextInput String 

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
   case msg of 
    Post -> ({model | loading = True}, postRequest model.textInput)
    (GotResult result) -> case result of 
                            Ok r -> ({model | success = Just r }, Cmd.none)
                            (Err _) ->  ({model | failure = True }, Cmd.none)
    (TextInput s)      -> ({model | textInput = s}, Cmd.none)


subscriptions : Model -> Sub Msg 
subscriptions model = 
   Sub.none 

view : Model -> Html Msg 
view model = 
  div [] 
  [h2 [] [text "Logic Calcluator"]
  ,viewServerResponse model 
  ,ourTextArea model
  ]

ourTextArea : Model -> Html Msg 
ourTextArea model = textarea
              [ onInput TextInput
              ]
              [i [] [text "Enter here..."]
              , button [onClick Post] [text model.textInput]
              ]

viewServerResponse : Model -> Html Msg 
viewServerResponse model = 
   case model.failure of 
    True -> 
      div [] 
      [ text "I could not get a server response"
      ]
    False -> case model.loading of 
              True -> text "Loading"
              False -> case model.success of 
                        (Just serverResponse) -> case serverResponse.evaluation of 
                                                  "" ->  div []
                                                         [ blockquote [] [text serverResponse.parseError]
                                                         ]
                                                  t  ->  div []
                                                         [blockquote [] [text t]
                                                         ]

                        Nothing ->                       div []
                                                         [blockquote [] [text "Nothing yet"]
                                                         ]

postRequest : String -> Cmd Msg 
postRequest s = 
   Http.post 
     {  url     = "/upload"
     ,  body    = stringBody "text/plain" s
     ,  expect  = Http.expectJson GotResult resultDecoder
     }
resultDecoder : Decoder ServerResponse
resultDecoder = 
  map2 ServerResponse
   (field "parseError" string)
   (field "evaluation" string)