module Types exposing (..)

type alias Model =
  { tournaments : List Tournament
  , action : Action
  }

type Action
  = ViewingAllTournaments
  | CreatingTournament TournamentCreationForm
  | ViewingTournament String ThrowerCreationForm

type alias TournamentName = String
type alias Tournament = 
  { name : TournamentName
  , throwers : List Thrower
  }

type alias Thrower = String

type alias TournamentCreationForm =
  { tournamentName : String
  }

type alias ThrowerCreationForm =
  { name : String
  }
