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
import Html.Attributes as Attr
import Html.Events as Events
import Html.App as App
import Time exposing (Time)

import Json.Decode as Decode exposing (Decoder, (:=))
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
  , isWListLocked : Bool
  , socket : Phoenix.Socket.Socket Msg
  , socketUrl : String
  , updatesChannel : Phoenix.Channel.Channel Msg
  , fieldContent : String
  }

type Msg
  = VoteListMsg VoteList.Msg
  | NewWordMsg NewWordList.Msg
  | ToggleNWListLock
  | ToggleNWLockFail
  | ToggleNWLockSucceed
  | ToggleWListLock
  | ToggleWLockFail
  | ToggleWLockSucceed
  | FetchLists Json.Value
  | FetchNewWords Json.Value
  | FetchWords Json.Value
  | WordUpdate Json.Value
  | NewWordUpdate Json.Value
  | PhoenixMsg (Phoenix.Socket.Msg Msg)
  | SetVideo String
  | SetVideoFail Http.Error
  | SetVideoSucceed String
  | LockStateFail Http.Error
  | LockStateSucceed Bool
  | WLockStateFail Http.Error
  | WLockStateSucceed Bool
  | NewContent String
  | Ping
{-  | NewWordVote String
  | ShouldUpdate
  | Tick-}

serverUrl : String
serverUrl = "http://localhost:4000/"

socketUrl : String
socketUrl = "ws://localhost:4000/socket/websocket"

wordsUrl : String
wordsUrl = serverUrl++"api/v1/words/"

newWordsUrl : String
newWordsUrl = serverUrl++"api/v1/new_words/"

adminUrl : String
adminUrl = serverUrl++"api/v1/admin/"


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

      lockCmd = Task.perform LockStateFail LockStateSucceed
                  ( Http.get decodeLockState (newWordsUrl++"lock_state/") )

      wLockCmd = Task.perform WLockStateFail WLockStateSucceed
                 ( Http.get decodeLockState (wordsUrl++"lock_state/") )

  in ( { voteList = initialVoteList 
       , newWordList = initialNWordList 
       , updateFrequency = initialFreq 
       , remainingTime = 60 
       , isNWListLocked = True
       , isWListLocked = True
       , socket = socket
       , socketUrl = socketUrl
       , updatesChannel = channel
       , fieldContent = ""
       }
     , Cmd.batch
         [ Cmd.map VoteListMsg voteListCmd
         , Cmd.map NewWordMsg nwListCmd
         , Cmd.map PhoenixMsg phxCmd
         , lockCmd
         , wLockCmd
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
            (Http.post (Decode.succeed "")
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

    ToggleWListLock ->
      ( model
      , Task.perform (\_ -> ToggleWLockFail) (\_ -> ToggleWLockSucceed)
            (Http.post (Decode.succeed "")
               (adminUrl++"lock_words/") Http.empty)
      )

    ToggleWLockFail ->
      ( Debug.log("could not toggle wlist lock") model
      , Cmd.none
      )

    ToggleWLockSucceed ->
      ( { model | isWListLocked = not model.isWListLocked
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

    NewContent str ->
      ({ model | fieldContent = str}, Cmd.none)

    SetVideo str ->
      let payload = Json.encode 0
                    <| Json.object
                         [ ("video"
                           , Json.object
                              [ ("code", (Json.string str)) ]
                           )
                         ]

          httpRequest = Http.send Http.defaultSettings
                        { verb = "POST"
                        , headers = [("Accept", "application/json")
                                    ,("Content-Type", "application/json")
                                    ]
                        , url = adminUrl++("set_video/")
                        , body = Http.string (Debug.log "payload" payload)
                        }
      in
        ( model
        , Task.perform SetVideoFail SetVideoSucceed
            (Http.fromJson (Decode.succeed "") httpRequest)
        )

    SetVideoFail err ->
      (Debug.log ("got err " ++ (toString err)) model
      , Cmd.none
      )

    SetVideoSucceed _ ->
      ( { model | fieldContent = "" }, Cmd.none )

    LockStateFail err ->
      (Debug.log ("lockstate: got err " ++ (toString err)) model
      , Cmd.none
      )

    LockStateSucceed value ->
      ( { model | isNWListLocked = value }, Cmd.none )

    WLockStateFail err ->
      (Debug.log ("wlockstate: got err " ++ (toString err)) model
      , Cmd.none
      )
      
    WLockStateSucceed value ->
      ( { model | isWListLocked = value }, Cmd.none )

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
  H.div [] [ videoForm model
           , H.button
              [ Events.onClick ToggleWListLock ]
              [ H.text (if model.isWListLocked
                        then "Iniciar votação"
                        else "Encerrar votação"
                       )
              ]
           , App.map VoteListMsg <| VoteList.view model.voteList
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
    

videoForm : Model -> Html Msg
videoForm model =
  H.form
     [ Attr.class "form-inline" ]
     [ H.div
         [ Attr.class "form-group" ]
         [ H.input
             [ Attr.placeholder "Insira o código do YouTube"
             , Events.onInput NewContent
             , Attr.value model.fieldContent
             ] []
         , H.button
             [ Attr.type' "button"
             , Attr.class "btn btn-secondary"
             , Events.onClick (SetVideo model.fieldContent)
             ]
             [ H.text "Confirmar vídeo" ]
         ]
     ]

--json decoders

decodeLockState : Decoder Bool
decodeLockState =
  "data" := ( "state" := Decode.bool )
