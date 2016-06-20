module CoreoServerUI.VoteList exposing (Model, Msg(FetchList, WordUpdate), update, view, init, subscriptions)

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
  { currentWord : String
  , votes : List Votes
  , url : String
  }

type Msg 
  = FetchList
  | UpdateListFail Http.Error
  | UpdateListSucceed (List Votes)
  | ResetList
  | RemoveWord Int
  | RemoveFail Http.Error
  | RemoveSucceed ()
  | WordUpdate Json.Value
  | NoOp

{- código da lista aqui -}
{- ler sobre como channels funcionam pra fazer as atualizações virem por websocket -}
init : String -> (Model, Cmd Msg)
init url =
  ( Model "" [] url
  , Cmd.none
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
      ( Debug.log "Could not get vList" model
      , Cmd.none
      )

    UpdateListSucceed vList ->
      let sorted = vList |> (List.sortBy (\a -> negate a.votes))
      in
        ( { model | votes = sorted
          , currentWord = mostVoted sorted
          }
        , Cmd.none
        )
        
    ResetList -> --change this one to reset on the server too
      ( { model | votes = resetVoteList model.votes 
                , currentWord = "" }
      , Cmd.none
      )

    RemoveWord id ->
      let httpRequest = Http.send Http.defaultSettings
                        { verb = "DELETE"
                        , headers = [("Accept", "application/json")
                                    ,("Content-Type", "application/json")
                                    ]
                        , url = model.url ++ (toString id)
                        , body = Http.empty
                        }

      in 
        ( model
        , Task.perform RemoveFail RemoveSucceed
            (Http.fromJson decodeEmptyResponse httpRequest)
        )
        
    RemoveFail err ->
      ( Debug.log ("got err" ++ (toString err)) model
      , Cmd.none 
      )

    RemoveSucceed _ ->
      ( model, Cmd.none )

    WordUpdate json ->
      let data = Decode.decodeValue decodeVote json
      in case data of
           Ok newVote ->
             ( { model | votes = dispatchAction (always newVote.votes) newVote.id model.votes }
             , Cmd.none
             )
           Err err ->
             ( (Debug.log ("got err " ++ err) model)
             , Cmd.none
             )

    NoOp ->
      (model, Cmd.none)


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
        [ Events.onClick (RemoveWord vote.id) ]
        [ H.text "Remover" ]
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

mostVoted : List Votes -> String
mostVoted list = Maybe.withDefault (Votes 0 "" 0) (List.head list) |> .name

--decoders for JSON data

decodeEmptyResponse : Decoder ()
decodeEmptyResponse = Decode.succeed ()

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
