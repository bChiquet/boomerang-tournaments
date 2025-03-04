module Main exposing (..)

import Browser
import Html exposing (Html, div, text, button, input)
import Html.Events exposing (onClick, onInput)
import Debug exposing (log)



type alias Model =
  { tournaments : List Tournament
  , action : Action
  }

type Action
  = ViewingTournaments
  | CreatingTournament TournamentCreationForm
  | ViewingTournament String ThrowerCreationForm

type alias Tournament = 
  { name : String
  , throwers : List String
  }

type alias TournamentCreationForm =
  { tournamentName : String
  }

type alias ThrowerCreationForm =
  { name : String
  }

------- Init
initialModel =
  { tournaments = [] 
  , action = ViewingTournaments
  }

initProgram : () -> (Model, Cmd msg)
initProgram _ = (initialModel, Cmd.none)

------- Logic
type Msg
  = NewTournament
  | BackToTournaments
  | TournamentCreated TournamentCreationForm
  | TournamentSelected String 
  | Typing Field String
  | ThrowerAdded

type Field = TournamentName | ThrowerName

update : Msg -> Model -> (Model, Cmd msg)
update msg model = case msg of
  NewTournament ->
    ( { model | action = CreatingTournament { tournamentName = "" }}
    , Cmd.none
    )
  BackToTournaments ->
    ( { model | action = ViewingTournaments }
    , Cmd.none
    )
  TournamentCreated data -> 
    ( { model | action = ViewingTournaments
              , tournaments =
                  { name = data.tournamentName
                  , throwers = []
                  } :: model.tournaments
      }
    , Cmd.none
    )
  TournamentSelected tournamentName ->
    ( { model | action = ViewingTournament tournamentName { name="" } }

    , Cmd.none
    )
  Typing _ data -> case model.action of
    (CreatingTournament form) ->
      ({ model | action = CreatingTournament { tournamentName = data }}
      , Cmd.none
      )
    _ -> 
      (model, Cmd.none)
  ThrowerAdded -> (model, Cmd.none)


-------- View

view : Model -> Html Msg
view model = case model.action of 
  ViewingTournaments -> div []
    [ newTournamentButton
    , tournamentList model
    ]
  CreatingTournament form -> creationTournamentCreationForm model form
  ViewingTournament name form ->
    showTournamentDetails model name form

creationTournamentCreationForm model form = 
    div []
      [ input [onInput (\name -> Typing TournamentName name)] []
      , button [onClick BackToTournaments] [text "cancel"]
      , button
          [onClick (TournamentCreated form)]
          [text "create"]
      ]
    

newTournamentButton = button [onClick NewTournament] [text "New Tournament"]

tournamentList model =
  List.map showTournament model.tournaments
  |> div []

showTournament : Tournament -> Html Msg
showTournament tournament =
  div [onClick (TournamentSelected tournament.name)] [text tournament.name]

showTournamentDetails : Model -> String -> ThrowerCreationForm -> Html Msg
showTournamentDetails model tname form =
  div []
    ([text tname] ++
    [ input [onInput (\name -> Typing TournamentName name)] []
    , button [onClick ThrowerAdded] [text "add thrower"]
    ])

---------- Program
main : Program () Model Msg
main = Browser.element
  { init = initProgram
  , view = view
  , update = update
  , subscriptions = \_ -> Sub.none
  }
