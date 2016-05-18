module CoreoServer exposing (main)
{-| This is the top-level Elm module for a web server and interface created for
Brazilian dancer André Aguiar's multimedia choreography <name here>.

For usage details, check main.js.

@docs main 
-}

import Html as H exposing (Html)
import Html.Events as Events
import Html.App as App
import Time exposing (Time)

{-| main: Start the server and web app.
-}
main : Program Never
main = 
  App.program
     { init = init
     , view = view
     , update = update
     , subscriptions = subs
     }

type alias Model = 
  { currentWord : String 
  , votes : List (String, Int) 
  , newWords : List (String, Int)
  , updateFrequency : Time
  , remainingTime : Int
  }

type Msg
  = NewVote String
  | NewWordVote String
  | ShouldUpdate
  | Tick

initialFreq : Time
initialFreq = 60 * Time.second

initialOptions : List String
initialOptions =
  [ "Forte"
  , "Suave"
  , "Rápido"
  , "Lento"
  ]
  
init : (Model, Cmd Msg)
init = 
  let initialWord = case (List.head initialOptions) of
                  Just x  -> x
                  Nothing -> "ERRO"

      initialVotes = List.map (\x -> (x,0)) initialOptions

  in ( Model initialWord initialVotes [("Placeholder", 0)] initialFreq 60, Cmd.none )

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case message of
    NewVote str -> 
      let (list, item) = searchAndRemove str model.votes
      in case item of
           Just x ->
             ( { model | votes = insertSorted (incVotes x) list }, Cmd.none )
           Nothing ->
             ( model, Cmd.none )

    NewWordVote str ->
      let (list, item) = searchAndRemove str model.newWords
      in case item of
           Just x ->
             ( { model | votes = insertSorted (incVotes x) list }, Cmd.none )
           Nothing ->
             ( model, Cmd.none )

    ShouldUpdate ->
      let wordPair = 
            List.head (List.sortBy snd model.votes) `Maybe.andThen` (\mostVoted ->
            List.head (List.sortBy snd model.newWords) `Maybe.andThen` (\mostVotedNew ->
            Just (mostVoted, mostVotedNew)))

      in case wordPair of
           Just (mostVoted, mostVotedNew) ->
             ( { model | currentWord = fst mostVoted
                       , votes = (fst mostVotedNew, 0) :: (resetVotes model.votes)
                       , remainingTime = 60
               }
             , Cmd.none
             )

           Nothing -> 
             ({ model | remainingTime = 60 }, Cmd.none)

    Tick ->
     if model.remainingTime > 1 then
       ({ model | remainingTime = model.remainingTime - 1 }, Cmd.none)
     else ({ model | remainingTime = 60 }, Cmd.none)

view : Model -> Html Msg
view model = 
  H.div [] [ H.h1 [] 
               [ H.text ("Palavra Atual: " ++ model.currentWord) ]
           , H.h2 [] [ H.text "Votos atuais: " ]
           , H.div [] [ voteList model.votes ]
           , H.div [] [ H.text (toString model.newWords) ]
           , H.div [] [ H.text ("Tempo até a próxima atualização: " ++ 
                                  (toString model.remainingTime))
                      ]
           , H.button 
               [ Events.onClick ShouldUpdate ] 
               [ H.text "Atualizar agora" ]
           ]

subs : Model -> Sub Msg
subs model = 
  Sub.batch 
        [ Time.every model.updateFrequency (\_ -> ShouldUpdate)
        , Time.every Time.second (\_ -> Tick)
        ]

--helper functions
searchAndRemove : a -> List (a, comparable) -> (List (a, comparable), Maybe (a, comparable))
searchAndRemove x list = searchAndRemove' x list []

searchAndRemove' : a -> List (a, comparable) -> List (a, comparable) -> (List (a, comparable), Maybe (a, comparable))
searchAndRemove' x list accList =
  case list of
    (a, b) :: rest ->
      if a == x then ((List.reverse accList) ++ rest, Just (a, b))
      else searchAndRemove' x rest ((a,b)::accList)
    
    [] -> (List.reverse accList, Nothing)

insertSorted : (a, comparable) -> List (a, comparable) -> List (a, comparable)
insertSorted (a, b) list =
  case list of
    (x, y) :: rest ->
      if b > y then (a, b) :: (x, y) :: rest
      else (x, y) :: (insertSorted (a, b) rest)

    [] -> [(a, b)]

incVotes : (a, number) -> (a, number)
incVotes (a, n) = (a, n+1)

resetVotes : List (a, number) -> List (a, number)
resetVotes = List.map (\ (a, b) -> (a, 0))

--server data dummying helpers
voteList : List (String, Int) -> Html Msg
voteList pairList =
  let list =
    List.map listElem pairList
  in H.ul [] list

listElem : (String, Int) -> Html Msg
listElem (str, n) =
  H.li []
     [ H.text (str ++ ":" ++ (toString n))
     , H.button
         [ Events.onClick (NewVote str) ]
         [ H.text "Vote" ] 
     ]

        
    

