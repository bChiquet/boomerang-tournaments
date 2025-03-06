module TournamentCreation exposing
    ( Msg(..)
    , update
    )
import Types exposing (..)

type Msg
  = NewTournament
  | TournamentCreated TournamentCreationForm


update : Msg -> Model -> (Model, Cmd msg)
update msg model = case msg of
  NewTournament ->
    ( { model | action = CreatingTournament { tournamentName = "" }}
    , Cmd.none
    )
  TournamentCreated data -> 
    ( { model | action = ViewingAllTournaments
              , tournaments =
                  { name = data.tournamentName
                  , throwers = []
                  } :: model.tournaments
      }
    , Cmd.none
    )
