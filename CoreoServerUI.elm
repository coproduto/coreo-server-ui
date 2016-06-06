module CoreoServerUI exposing (main)
{-| This is the top-level Elm module for a web server and interface created for
Brazilian dancer André Aguiar's multimedia choreography <name here>.

For usage details, check main.js.

@docs main 
-}

import CoreoServerUI.VoteList as VoteList

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
     , subscriptions = subscriptions
     }

type alias Model = 
  { voteList : VoteList.Model
  , updateFrequency : Time
  , remainingTime : Int
  }

type Msg
  = VoteListMsg VoteList.Msg
{-  | NewWordVote String
  | ShouldUpdate
  | Tick-}

url : String
url = "http://localhost:4000/api/v1/words/"

initialFreq : Time
initialFreq = 60 * Time.second
  
init : (Model, Cmd Msg)
init = 
  let (initialVoteList, voteListCmd) = VoteList.init url
  in ( Model initialVoteList initialFreq 60
     , Cmd.batch
         [ Cmd.map VoteListMsg voteListCmd
         ] 
     )

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case message of
    VoteListMsg msg ->
      let (newVoteList, voteListCmd) = VoteList.update msg model.voteList
      in ({ model | voteList = newVoteList }, Cmd.map VoteListMsg voteListCmd)

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
--           , H.div [] [ H.text (toString model.newWords) ]
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
{-        , Time.every model.updateFrequency (\_ -> ShouldUpdate)
        , Time.every Time.second (\_ -> Tick)-}
        ]        
    

