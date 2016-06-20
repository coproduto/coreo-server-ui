module CoreoServerUI exposing (main)
{-| This is the top-level Elm module for a web server and interface created for
Brazilian dancer André Aguiar's multimedia choreography <name here>.

For usage details, check main.js.

@docs main 
-}

import CoreoServerUI.VoteList as VoteList
import CoreoServerUI.NewWordList as NewWordList

import Phoenix.Socket
import Phoenix.Channel
import Phoenix.Push

import Http
import Task exposing (Task)

import Html as H exposing (Html)
import Html.Events as Events
import Html.App as App
import Time exposing (Time)

import Json.Decode as Decode

{-| main: Start the server and web app.
-}
main : Program Never
main = 
  App.program
     { init = init
     , view = view
     , update = update
     , subscriptions = subscriptions
     }

type alias Model = 
  { voteList : VoteList.Model
  , newWordList : NewWordList.Model
  , updateFrequency : Time
  , remainingTime : Int
  , isNWListLocked : Bool
  }

type Msg
  = VoteListMsg VoteList.Msg
  | NewWordMsg NewWordList.Msg
  | ToggleNWListLock
  | ToggleNWLockFail
  | ToggleNWLockSucceed
{-  | NewWordVote String
  | ShouldUpdate
  | Tick-}

wordsUrl : String
wordsUrl = "http://localhost:4000/api/v1/words/"

newWordsUrl : String
newWordsUrl = "http://localhost:4000/api/v1/new_words/"

adminUrl : String
adminUrl = "http://localhost:4000/api/v1/admin/"

socketUrl : String
socketUrl = "ws://localhost:4000/socket/websocket"

initialFreq : Time
initialFreq = 60 * Time.second
  
init : (Model, Cmd Msg)
init = 
  let (initialVoteList, voteListCmd) = VoteList.init wordsUrl
      (initialNWordList, nwListCmd)  = NewWordList.init newWordsUrl
  in ( Model initialVoteList initialNWordList initialFreq 60 False
     , Cmd.batch
         [ Cmd.map VoteListMsg voteListCmd
         , Cmd.map NewWordMsg nwListCmd
         ] 
     )

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case message of
    VoteListMsg msg ->
      let (newVoteList, voteListCmd) = VoteList.update msg model.voteList
      in ({ model | voteList = newVoteList }, Cmd.map VoteListMsg voteListCmd)

    NewWordMsg msg ->
      let (newNWList, nwListCmd) = NewWordList.update msg model.newWordList
      in ({ model | newWordList = newNWList }, Cmd.map NewWordMsg nwListCmd)

    ToggleNWListLock ->
      ( model
      , Task.perform (\_ -> ToggleNWLockFail) (\_ -> ToggleNWLockSucceed)
            (Http.post Decode.string 
               (adminUrl++"lock_new_words/") Http.empty)
      )

    ToggleNWLockFail ->
      ( Debug.log("could not toggle list lock") model
      , Cmd.none
      )

    ToggleNWLockSucceed ->
      ( { model | isNWListLocked = not model.isNWListLocked
        }
      , Cmd.none
      )

{-    NewWordVote str ->
      let (list, item) = searchAndRemove str model.newWords
      in case item of
           Just x ->
             ( { model | votes = insertSorted (incVotes x) list }, Cmd.none )
           Nothing ->
             ( model, Cmd.none )-}

{-    ShouldUpdate ->
      let wordPair = 
            List.head (List.sortBy snd model.votes) `Maybe.andThen` (\mostVoted ->
            List.head (List.sortBy snd model.newWords) `Maybe.andThen` (\mostVotedNew ->
            Just (mostVoted, mostVotedNew)))

      in case wordPair of
           Just (mostVoted, mostVotedNew) ->
             ( { model | currentWord = fst mostVoted
          p             , votes = (fst mostVotedNew, 0) :: (resetVotes model.votes)
                       , remainingTime = 60
               }
             , Cmd.none
             )

           Nothing -> 
             ({ model | remainingTime = 60 }, Cmd.none)

    Tick ->
     if model.remainingTime > 1 then
       ({ model | remainingTime = model.remainingTime - 1 }, Cmd.none)
     else ({ model | remainingTime = 60 }, Cmd.none)-}

view : Model -> Html Msg
view model = 
  H.div [] [ App.map VoteListMsg <| VoteList.view model.voteList
           , H.button
              [ Events.onClick ToggleNWListLock ]
              [ H.text (if model.isNWListLocked
                        then "Desbloquear criação de novas palavras"
                        else "Bloquear criação de novas palavras"
                       )
              ]
           , App.map NewWordMsg <| NewWordList.view model.newWordList
           , H.div [] [ H.text ("Tempo até a próxima atualização: " ++ 
                                  (toString model.remainingTime))
                      ]
{-           , H.button 
               [ Events.onClick ShouldUpdate ] 
               [ H.text "Atualizar agora" ]-}
           ]

subscriptions : Model -> Sub Msg
subscriptions model = 
  Sub.batch 
        [ Sub.map VoteListMsg <| VoteList.subscriptions model.voteList
        , Sub.map NewWordMsg  <| NewWordList.subscriptions model.newWordList
{-        , Time.every model.updateFrequency (\_ -> ShouldUpdate)
        , Time.every Time.second (\_ -> Tick)-}
        ]        
    

