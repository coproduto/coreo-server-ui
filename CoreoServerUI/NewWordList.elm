module CoreoServerUI.NewWordList exposing (Model, Msg(FetchList, NewWordUpdate), update, view, init, subscriptions)

import Html as H exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events

import Http
import Task exposing (Task)

import Result exposing (Result)
import Json.Decode as Decode exposing (Decoder,(:=))
import Json.Encode as Json

import Debug

type alias Votes =
  { id : Int
  , name: String
  , votes: Int 
  }

type alias Model =
  { votes : List Votes
  , url : String
  }

type Msg 
  = FetchList
  | UpdateListFail Http.Error
  | UpdateListSucceed (List Votes)
  | ResetList
  | NewWordUpdate Json.Value

init : String -> (Model, Cmd Msg)
init url =
  ( Model [] url
  , Task.perform UpdateListFail UpdateListSucceed (Http.get decodeVoteList url)
  )

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case message of
    FetchList ->
      ( model
      , Task.perform UpdateListFail UpdateListSucceed 
          (Http.get decodeVoteList model.url)
      )

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

    NewWordUpdate json ->
      let data = Decode.decodeValue decodeVote json
      in case data of
           Ok vote ->
             ( { model | votes = dispatchAction 
                                  (always vote.votes)
                                   vote.id
                                   model.votes
               }
             , Cmd.none
             )
                   
           Err err ->
             ( (Debug.log("got err " ++ err) model)
             , Cmd.none
             )


view : Model -> Html Msg
view model =
  H.div 
     []
     [ H.h3 [] [ H.text "Votos para nova palavra: " ]
     , H.div
        []
        [ voteList model.votes ]
     , H.button
        [ Events.onClick ResetList ]
        [ H.text "Reiniciar votos de nova palavra" ]
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
