module Main exposing (..)

import Browser
import Html exposing (Html, div, text, button, input)
import Html.Events exposing (onClick, onInput)
import Debug exposing (log)



type alias Model =
  { tournaments : List Tournament
  , action : Action
  }

type Action = ViewingTournaments | CreatingTournament TournamentCreationForm

type alias Tournament = 
  { name : String
  , throwers : List String
  , status : TournamentStatus
  }

type TournamentStatus = Created | Published

type alias TournamentCreationForm =
  { tournamentName : String
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
  | Typing Field String

type Field = TournamentName

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
                  , status = Created
                  , throwers = []
                  } :: model.tournaments
      }
    , Cmd.none
    )
  Typing _ data -> case model.action of
    (CreatingTournament form) ->
      ({ model | action = CreatingTournament { tournamentName = data }}
      , Cmd.none
      )
    _ -> 
      (model, Cmd.none)

-------- View

view : Model -> Html Msg
view model = case model.action of 
  ViewingTournaments -> div []
    [ newTournamentButton
    , tournamentList model
    ]
  CreatingTournament form -> creationTournamentCreationForm model form

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

showTournament tournament = text tournament.name



---------- Program
main : Program () Model Msg
main = Browser.element
  { init = initProgram
  , view = view
  , update = update
  , subscriptions = \_ -> Sub.none
  }
