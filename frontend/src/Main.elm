module Main exposing (..)
import Browser exposing (..)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)
import Maybe exposing (withDefault)
import Time as Time exposing (..)
import Random as Random exposing (..)
import Array as Array exposing (..)
import Dict as D exposing (..)
import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)



oneToX : Array a -> Random.Generator Int 
oneToX a = Random.int 1 (Array.length a)



pathToMyImage : String -> (Html Msg)
pathToMyImage path = img
         [src path
         ,Attr.style "border-radius" "4px"
         ,Attr.style "border" "1px solid #ddd"
         ,Attr.style "height"  "80px"
         ,Attr.style "width"    "80px"
         ,Attr.style "margin" "0 auto"
         ,onMouseOver AdventureTimeSound
         ] 
         []
pathToMyAudio : String -> (Html Msg)
pathToMyAudio path = audio 
         [src path
         ,hidden True 
         ,autoplay True
         ,controls False
         ,default True 
         ]
         []
  
myAudio : Array (Html Msg)
myAudio = Array.fromList 
        [pathToMyAudio  "/audio/theme"
        ,pathToMyAudio "/audio/baby"
        ,pathToMyAudio "/audio/jake-fart"
        ]
myImages : Array (Html Msg)
myImages =  Array.fromList
            [
              pathToMyImage "/images/rootBeerAvatar.png"
            , pathToMyImage "/images/jakeTheDog.jpg"
            , pathToMyImage "/images/marceline.png"
            , pathToMyImage "/images/bubblegum.png"
            , pathToMyImage "/images/fin.png"
            ]
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
           ,switchImage : Maybe Int
           ,soundNumber : Maybe Int 
           } 
initialModel =  
             {failure = False
             ,loading = False 
             ,textInput = {boolExpr = ""}
             ,success  = Nothing 
             ,switchImage = Just 1
             ,soundNumber = Just 1 
             }
type alias ServerResponse = 
      { parseError   : String 
      , evaluation   : String 
      , gatesAndOuts : List (String,Bool)
      }

init : () -> (Model, Cmd Msg)
init _ = 
  (initialModel, Cmd.none)

type Msg = Post 
         | GotResult (Result Http.Error ServerResponse)
         | TextInput String 
         | Operator Char 
         | Erase String 
         | Symbol Char
         | NewImage 
         | ImageNumber Int
         | AdventureTimeSound
         | SoundNumber Int 

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
   case msg of 
    Post -> ({model | loading = True}, postRequest model.textInput)
    (GotResult result) -> case result of 
                            Ok r -> ({model | success = Just r , loading = False}, Cmd.none)
                            (Err _) ->  ({model | failure = True ,loading = False}, Cmd.none)
    (TextInput s)      -> ({model | textInput = {boolExpr = s }}, Cmd.none)
    (Operator c)       -> ({model | textInput = {boolExpr = model.textInput.boolExpr ++ (String.fromChar c)}}, Cmd.none)
    (Erase st)         -> case st of 
                           "backspace" -> ({model | textInput = {boolExpr = String.dropRight 1 model.textInput.boolExpr}}, Cmd.none)
                           "delete"    -> ({model | textInput = {boolExpr = ""}},Cmd.none)
                           _           -> (model, Cmd.none)
    (Symbol sy)            -> ({model | textInput = {boolExpr = model.textInput.boolExpr ++ (String.fromChar sy)}}, Cmd.none)
    (NewImage)           -> (model, Random.generate ImageNumber (oneToX myImages))
    (ImageNumber i)      -> ({model | switchImage = Just i}, Cmd.none)
    AdventureTimeSound   -> (model, Random.generate SoundNumber (oneToX myAudio))
    (SoundNumber i)      -> ({model | soundNumber = Just i}, Cmd.none)
        


subscriptions : Model -> Sub Msg 
subscriptions model = Time.every (15 * 1000) (\_ -> NewImage) 

myFooter : Html Msg 
myFooter = 
   Html.footer 
   [Attr.style "border" "solid"
   ,Attr.style "background-color" "red"
   ]
   [ Html.a 
    [Attr.href "https://github.com/alec-guy"]
    [Html.text "contact me here if you want to cringe"]
   ]
myHeader : Html Msg 
myHeader = 
    Html.header 
    [Attr.id "title"
    ,Attr.style "text-align" "center"
    ,Attr.style "color" "black"
    ] 
    [ h2 
       [ Attr.style "text-align" "center"
       ,Attr.style "color" "black"
       ] 
     [Html.text "Logic Calcluator"] 
    ]
view : Model -> Html Msg 
view model = 
  div []
  [ myHeader
  ,div 
    [Attr.id "div1"
    ,Attr.style "padding" "0 auto"
    ,Attr.style "max-width" "600px"
    ,Attr.style "margin" "0 auto" -- Center
    ,Attr.style "background-color" "#332"
    ] 
    [ viewServerResponse model 
    , myFooter
    ]
  
  
  , case model.switchImage of
     Nothing  -> Html.text ""
     (Just i) -> case Array.get (i - 1) myImages of 
                  Nothing  -> Html.text ""
                  (Just im) -> im
  , case model.soundNumber of 
     Nothing -> Html.text ""
     Just i  -> case Array.get (i - 1) myAudio of 
                 Nothing -> Html.text ""
                 (Just au) -> au
  ]


calculator : Model -> Html Msg 
calculator m = 
            div 
            [Attr.id "calculator"
            ,Attr.style "background-color" "blue"
            ,Attr.style "padding" "20px"
            ,Attr.style "border-raidus" "20px" --rounded corners
            ,Attr.style "box-shadow" "0 4px 8px rgba(0,0,0,0.2)"
            ]
            [
             div 
             [Attr.id "display"]
             [ourTextArea m]
            ,div 
             [Attr.id "button grid"
             ,Attr.style "display" "grid"
             ,Attr.style "grid-template-columns" "auto auto auto auto"
             ,Attr.style "gap" "10px"
             ]
             [button [onClick <| (Operator 'T')]   [Html.text "T"]
             ,button [onClick <| (Operator 'F')]   [Html.text "F"]
             ,button [onClick <| (Operator notChar)] [Html.text "\u{00AC}"]
             ,button [Attr.style "background-color" "yellow" , onClick <| (Operator and)] [Html.text "\u{2227}"]
             ,button [onClick <| (Operator or)] [Html.text "\u{2228}"] 
             ,button [onClick <| (Operator ifThen)] [Html.text "\u{2192}"] 
             ,button [onClick <| (Operator iff)] [Html.text "\u{2194}"]
             ,button [onClick <| (Operator xor)] [Html.text "\u{2295}"]
             ,button [onClick <| (Operator nand)] [Html.text "\u{22BC}"]
             ,button [onClick <| (Operator nor)]  [Html.text "\u{22BD}"]
             ,button [Attr.style "background-color" "red" , onClick <| (Symbol '(')] [Html.text "("]
             ,button [onClick <| (Symbol ')')] [Html.text ")"]
             ,button [onClick <| (Erase "backspace")] [Html.text "<- backspace"]
             ,button [Attr.style "background-color" "green" , onClick <| (Erase "delete")]    [Html.text "NUKE"]
             ,button [onClick <| (Symbol ' ')] [Html.text "space"]
             , button 
                [onClick Post
                ,Attr.style "background-color" "orange"
                ,Attr.style "color" "white"
                ,Attr.style "border" "none"
                ] 
                [Html.text "Enter ->"]
            ]
            ]

ifThen : Char 
ifThen = '\u{2192}'
iff : Char
iff = '\u{2194}'
notChar : Char
notChar = '\u{00AC}'
and : Char
and = '\u{2227}'
or : Char 
or = '\u{2228}'
xor : Char 
xor = '\u{2295}'
nand = '\u{22BC}'
nor = '\u{22BD}'





{- 
     S.svg
     [SA.viewBox  "0 0 200 150"
     ,SA.width "400px"
     ,SA.height "400px"
     ]

-}
-- first argument to end of makeGate 
fromSvg : List (Svg Msg) -> Html Msg 
fromSvg l = 
     S.svg
     [SA.viewBox  "0 0 200 150"
     ,SA.width "400px"
     ,SA.height "400px"
     ]
     l 

makeGate : (Int,Int) -> (String , Bool) -> List (Svg Msg)
makeGate (x, y) (gate1, gateOut1) =  
     [
      S.line 
      [SA.x1 <| String.fromInt <| 180 - x 
      ,SA.y1 <| String.fromInt <| 100 + y
      ,SA.x2 <| String.fromInt <| 200 - x 
      ,SA.y2 <| String.fromInt <| 100 + y 
      ,if gateOut1 then SA.stroke "green" else SA.stroke "red"
      ] 
      []
     , S.rect
      [SA.height "20"
      ,SA.width "20"
      ,SA.x <| String.fromInt <| 180 - x 
      ,SA.y <| String.fromInt <| 100 + y
      ,SA.fill <| if gateOut1 then "green" else "red"
      ]
      [S.text gate1]
     ]
gateDrawer : ((Int,Int) -> (String , Bool) -> List (Svg Msg)) 
           -> (List (String, Bool)) 
           -> (Int,Int)
           -> List (List (Svg Msg))
gateDrawer g l t = 
     case List.isEmpty l of 
      True -> [] 
      False -> (makeGate t (withDefault ("",True) (List.head l))) :: (gateDrawer g (List.drop 1 l) ((Tuple.first t)- 1,(Tuple.second t) + 1))
     


       
        
ourTextArea : Model -> Html Msg 
ourTextArea model = 
              div 
              [Attr.id "div2"
              ,Attr.style "background-color" "lightgray"
              ,Attr.style "padding" "16px"
              ]
              [ div
                [ onInput TextInput
                , Attr.title "Enter Expression Here"
                , Attr.style "width" "100%"
                , Attr.style "padding" "10px"
                , Attr.style "height" "100px"
                ]
                [i [Attr.style "color" "black"] 
                 [Html.text model.textInput.boolExpr
                 , br [] []
                 , br [] []
                 ,case model.success of 
                   Nothing -> Html.text "Error :("
                   (Just r) -> case r.evaluation of 
                                "Nothing" -> Html.text r.parseError 
                                _         -> div
                                              [] 
                                               [i [Attr.style "color" "red"]
                                                  [Html.text r.evaluation
                                                  ]
                                               , fromSvg <| List.concat <| gateDrawer makeGate r.gatesAndOuts (0,0) 
                                                 
                                               ]
                 ]
                ]
              ]   
fromBool  : Bool -> String 
fromBool b = case b of 
              True  -> "True"  
              False -> "False" 
viewServerResponse : Model -> Html Msg 
viewServerResponse model = 
   case model.success of 
    Nothing -> div 
               [Attr.style "color" "white"]
               [case model.loading of 
                 True  -> Html.text "Loading"
                 False -> Html.text ""
               ,calculator model
               ]
    (Just s) -> case s.evaluation of 
                 "Nothing" -> div 
                              [Attr.style "color" "white"]
                              [ case model.loading of 
                                 True -> Html.text "Loading"
                                 False -> Html.text ""
                              ,calculator model
                              ]
                 _         -> div 
                              [Attr.style "color" "white"]
                              [case model.loading of 
                                True  -> Html.text "Loading"
                                False -> Html.text ""
                              ,calculator model
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
  Decode.map3 ServerResponse
   (field "parseError" Decode.string)
   (field "evaluation" Decode.string)
   (field "gatesAndOuts" gatesAndOutsDecoder)
gateAndOutDecoder : Decoder (String,Bool)
gateAndOutDecoder = 
    Decode.map2 Tuple.pair 
      (Decode.index 0 Decode.string)
      (Decode.index 1 Decode.bool)
gatesAndOutsDecoder : Decoder (List (String,Bool))
gatesAndOutsDecoder = 
    Decode.list gateAndOutDecoder