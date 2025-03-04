module Main exposing (..)

import Browser
import Html exposing (Html, div, text, button, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (style, value, placeholder)
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
  , throwers : List Thrower
  }

type alias Thrower = String

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
  | ThrowerAdded String ThrowerCreationForm

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
    (ViewingTournament t form) ->
      ({ model | action = ViewingTournament t { name = data }}
      , Cmd.none)
    _ -> 
      (model, Cmd.none)
  ThrowerAdded tname form ->
    ({ model | action = ViewingTournament tname { name = ""}
             , tournaments = List.map (theFunction tname form.name) model.tournaments }
    , Cmd.none)


theFunction tournamentname thrower tournament =
  if tournament.name == tournamentname
  then { tournament | throwers = thrower :: tournament.throwers}
  else tournament

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
      [ homeButton 
      , input [onInput (\name -> Typing TournamentName name)] []
      , button
          [onClick (TournamentCreated form)]
          [text "create"]
      ]
    

homeButton = button [onClick BackToTournaments] [text "Back to home"]
newTournamentButton = button [onClick NewTournament] [text "New Tournament"]

tournamentList model =
  List.map showTournament model.tournaments
  |> div [ style "display" "flex"
         , style "flex-direction" "column"
         ]

showTournament : Tournament -> Html Msg
showTournament tournament =
  div [onClick (TournamentSelected tournament.name)] [text tournament.name]

showTournamentDetails : Model -> String -> ThrowerCreationForm -> Html Msg
showTournamentDetails model tname form =
  div []
    ([text tname] ++
    [ homeButton
    , div [] <| List.map showThrower (thatOtherFunction tname model)
    , input [ onInput (\name -> Typing TournamentName name)
            , placeholder "Add a thrower..."
            , value form.name]
            []
    , button [onClick (ThrowerAdded tname form)] [text "add thrower"]
    ])

showThrower = text

thatOtherFunction tournamentName model =
  model.tournaments
    |> List.filter (\t -> t.name == tournamentName)
    |> List.head
    |> Maybe.map .throwers
    |> Maybe.withDefault []



---------- Program
main : Program () Model Msg
main = Browser.element
  { init = initProgram
  , view = view
  , update = update
  , subscriptions = \_ -> Sub.none
  }
