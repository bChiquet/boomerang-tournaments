module MainLogic exposing (..)
import TournamentCreation
import Types exposing (..)

------- Init
initialModel : { tournaments : List a, action : Action }
initialModel =
  { tournaments = [] 
  , action = ViewingAllTournaments
  }

initProgram : () -> (Model, Cmd msg)
initProgram _ = (initialModel, Cmd.none)

------- Logic
type Msg
  = TournamentCreation TournamentCreation.Msg
  | BackToTournaments
  | TournamentSelected TournamentName 
  | Typing Field String
  | ThrowerAdded TournamentName ThrowerCreationForm

  
type Field = TournamentName | ThrowerName


update : Msg -> Model -> (Model, Cmd msg)
update msg model = case msg of
  TournamentCreation tmsg -> 
    (TournamentCreation.update tmsg model)
  BackToTournaments ->
    ( { model | action = ViewingAllTournaments }
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
    (ViewingTournament t form) ->
      ({ model | action = ViewingTournament t { name = data }}
      , Cmd.none)
    _ -> 
      (model, Cmd.none)
  ThrowerAdded tname form ->
    ({ model | action = ViewingTournament tname { name = ""}
             , tournaments = List.map (addThrowerToTournament tname form.name) model.tournaments }
    , Cmd.none)

addThrowerToTournament : TournamentName -> Thrower -> Tournament -> Tournament
addThrowerToTournament tournamentname thrower tournament =
  if tournament.name == tournamentname
  then { tournament | throwers = thrower :: tournament.throwers}
  else tournament

