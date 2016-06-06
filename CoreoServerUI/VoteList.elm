module CoreoServerUI.VoteList exposing (Model, Msg, update, view, init, subscriptions)

import Html as H exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events

import Http
import Task exposing (Task)

import Result exposing (Result)
import Json.Decode as Decode exposing (Decoder,(:=))

import Debug

type alias Votes =
  { id : Int
  , name: String
  , votes: Int 
  }

type alias Model =
  { currentWord : String
  , votes : List Votes
  , url : String
  }

type Msg 
  = NewVote Int
  | UndoVote Int
  | UpdateListFail Http.Error
  | UpdateListSucceed (List Votes)
  | ResetList

{- código da lista aqui -}
{- ler sobre como channels funcionam pra fazer as atualizações virem por websocket -}
init : String -> (Model, Cmd Msg)
init url =
  ( Model "" [] url
  , Task.perform UpdateListFail UpdateListSucceed (Http.get decodeVoteList url)
  )

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case message of
    NewVote id ->
      ({ model | votes = dispatchAction increment id model.votes
                           |> (List.sortBy (\a -> negate a.votes))
       }
      , Cmd.none
      )

    UndoVote id ->
      ({ model | votes = dispatchAction decrement id model.votes
                           |> (List.sortBy (\a -> negate a.votes))
       }
      , Cmd.none)

    UpdateListFail _ ->
      ( model
      , Cmd.none
      )

    UpdateListSucceed vList ->
      ( { model | votes = vList 
                            |> (List.sortBy (\a -> negate a.votes))
        }
      , Cmd.none
      )

    ResetList -> --change this one to reset on the server too
      ( { model | votes = resetVoteList model.votes }
      , Cmd.none
      )

view : Model -> Html Msg
view model =
  H.div 
     []
     [ H.h2 [] [ H.text ("Palavra atual: " ++ model.currentWord) ]
     , H.h3 [] [ H.text "Votos atuais: " ]
     , H.div
        []
        [ voteList model.votes ]
     , H.button
        [ Events.onClick ResetList ]
        [ H.text "Reiniciar votos" ]
     ]

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none


--helper function
voteList : List Votes -> Html Msg
voteList vList =
  let list =
    List.map listElem vList
  in H.ul [] list

listElem : Votes -> Html Msg
listElem vote =
  H.li []
     [ H.text (vote.name ++ ":" ++ (toString vote.votes))
     , H.button
         [ Events.onClick (NewVote vote.id) ]
         [ H.text "Vote" ] 
     ]

dispatchAction : (Int -> Int) -> Int -> List Votes -> List Votes
dispatchAction action target list =
  case list of
    (vote :: rest) ->
      if vote.id == target then { vote | votes = action (vote.votes) } :: rest
      else vote :: (dispatchAction action target rest)

    [] -> []

increment x = x + 1

decrement x = x - 1

resetVoteList : List Votes -> List Votes
resetVoteList = List.map (\vote -> { vote | votes = 0 })

--decoders for JSON data

decodeVoteList : Decoder (List Votes)
decodeVoteList = 
  let vList = decodeVote |> Decode.list
  in ("data" := vList)

decodeVote : Decoder Votes
decodeVote =
 Decode.object3 Votes 
   ("id"    := Decode.int)
   ("name"  := Decode.string) 
   ("votes" := Decode.int) 
         
decodeVoteResponse : Decoder Votes
decodeVoteResponse =
  ("data" := decodeVote)
