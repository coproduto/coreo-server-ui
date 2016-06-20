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
import Json.Encode as Json

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
  , socket : Phoenix.Socket.Socket Msg
  , socketUrl : String
  , updatesChannel : Phoenix.Channel.Channel Msg
  }

type Msg
  = VoteListMsg VoteList.Msg
  | NewWordMsg NewWordList.Msg
  | ToggleNWListLock
  | ToggleNWLockFail
  | ToggleNWLockSucceed
  | FetchLists Json.Value
  | FetchNewWords Json.Value
  | FetchWords Json.Value
  | WordUpdate Json.Value
  | NewWordUpdate Json.Value
  | PhoenixMsg (Phoenix.Socket.Msg Msg)
  | Ping
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

      initSocket = Phoenix.Socket.init socketUrl
                 |> Phoenix.Socket.withDebug
                 |> Phoenix.Socket.on "update:word" "updates:lobby" WordUpdate
                 |> Phoenix.Socket.on "update:new_word" "updates:lobby" NewWordUpdate
                 |> Phoenix.Socket.on "update:invalidate_all" "updates:lobby" FetchLists
                 |> Phoenix.Socket.on "update:invalidate_new_words" "updates:lobby" FetchNewWords
                 |> Phoenix.Socket.on "update:invalidate_words" "updates:lobby" FetchWords
                 |> Phoenix.Socket.on "update:invalidate_words_votes" "updates:lobby" FetchWords
                 |> Phoenix.Socket.on "update:invalidate_new_words_votes" "updates:lobby" FetchNewWords

      channel = Phoenix.Channel.init "updates:lobby"
              |> Phoenix.Channel.withPayload (Json.string "")
              |> Phoenix.Channel.onJoin FetchLists
      
      (socket, phxCmd) = Phoenix.Socket.join channel initSocket

  in ( { voteList = initialVoteList 
       , newWordList = initialNWordList 
       , updateFrequency = initialFreq 
       , remainingTime = 60 
       , isNWListLocked = True
       , socket = socket
       , socketUrl = socketUrl
       , updatesChannel = channel
       }
     , Cmd.batch
         [ Cmd.map VoteListMsg voteListCmd
         , Cmd.map NewWordMsg nwListCmd
         , Cmd.map PhoenixMsg phxCmd
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

    FetchLists _ ->
      let (newVoteList, voteListCmd) = VoteList.update VoteList.FetchList model.voteList
          (newWordList, wordListCmd) = NewWordList.update NewWordList.FetchList model.newWordList
      in 
        ( { model | voteList = newVoteList
                  , newWordList = newWordList 
          }
        , Cmd.batch
            [ Cmd.map VoteListMsg voteListCmd
            , Cmd.map NewWordMsg wordListCmd
            ]
        )

    FetchNewWords _ ->
      let (newWordList, wordListCmd) = NewWordList.update NewWordList.FetchList model.newWordList
      in 
        ( { model | newWordList = newWordList 
          }
        , Cmd.batch
            [ Cmd.map NewWordMsg wordListCmd
            ]
        )

    FetchWords _ ->
      let (newVoteList, voteListCmd) = VoteList.update VoteList.FetchList model.voteList
      in 
        ( { model | voteList = newVoteList 
          }
        , Cmd.batch
            [ Cmd.map VoteListMsg voteListCmd
            ]
        ) 

    WordUpdate json ->
      let (newVoteList, voteListCmd) = VoteList.update (VoteList.WordUpdate json) model.voteList
      in 
        ( { model | voteList = newVoteList }, Cmd.map VoteListMsg voteListCmd )

    NewWordUpdate json ->
      let (newWordList, wordListCmd) = NewWordList.update (NewWordList.NewWordUpdate json) model.newWordList
      in
        ( { model | newWordList = newWordList }, Cmd.map NewWordMsg wordListCmd )

    PhoenixMsg msg ->
      let
        (phxSocket, phxCmd) = Phoenix.Socket.update msg model.socket
      in
        ( { model | socket = phxSocket }
        , Cmd.map PhoenixMsg phxCmd
        )

    Ping -> 
      let ping = Phoenix.Push.init "ping" "updates:lobby"
               |> Phoenix.Push.withPayload (Json.string "ping-response")
          (socket, phxCmd) = Phoenix.Socket.push ping model.socket
      in ( { model | socket = socket }
         , Cmd.map PhoenixMsg phxCmd
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
        , Phoenix.Socket.listen model.socket PhoenixMsg
        , Time.every (5 * Time.second) (\_ -> Ping)
        ]
    

