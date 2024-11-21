module Main exposing (..)
import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)
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
           ,textInput   : Request  
           ,success : Maybe ServerResponse 
           } 
initialModel =  
             {failure = False
             ,loading = False 
             ,textInput = {boolExpr = ""}
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
    (TextInput s)      -> ({model | textInput = {boolExpr = s }}, Cmd.none)


subscriptions : Model -> Sub Msg 
subscriptions model = 
   Sub.none 

view : Model -> Html Msg 
view model = 
  div [] 
  [h2 [] [text "Logic Calcluator"]
  ,viewServerResponse model 
  ]

ourTextArea : Model -> Html Msg 
ourTextArea model = 
              div 
              []
              [ textarea
                [ onInput TextInput]
                [i [] [text "Enter here..."]

                ]
              , br [] []
              , button [onClick Post] [text "Submit"]
              ]   
      
viewServerResponse : Model -> Html Msg 
viewServerResponse model = 
   case model.success of 
    Nothing -> div 
               []
               [case model.loading of 
                 True  -> text "Loading"
                 False -> text ""
               ,ourTextArea model  
               ]
    (Just s) -> case s.evaluation of 
                 "Nothing" -> div 
                              []
                              [ case model.loading of 
                                 True -> text "Loading"
                                 False -> text ""
                              ,ourTextArea model 
                              ,br [] []
                              ,text s.parseError 
                              ]
                 _         -> div 
                              []
                              [case model.loading of 
                                True  -> text "Loading"
                                False -> text ""
                              ,ourTextArea model
                              , br [] []
                              ,text s.evaluation
                              ]
postRequest : Request -> Cmd Msg 
postRequest request = 
   Http.post 
     {  url     = "/upload"
     ,  body    = jsonBody <| fromRequest<| request 
     ,  expect  = Http.expectJson GotResult resultDecoder
     }
type alias Request = 
          { boolExpr : String
          }

fromRequest : Request -> Encode.Value
fromRequest request =  
             Encode.object 
             [("booleanExpression", Encode.string request.boolExpr)]
 
    
resultDecoder : Decoder ServerResponse
resultDecoder = 
  map2 ServerResponse
   (field "parseError" Decode.string)
   (field "evaluation" Decode.string)