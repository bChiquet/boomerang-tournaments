module View exposing (..)
import TournamentCreation exposing (..)
import Types exposing (..)
import MainLogic exposing (..)
import Html exposing (Html, div, text, button, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (style, value, placeholder)



-------- View

view : Model -> Html MainLogic.Msg
view model = case model.action of 
  ViewingAllTournaments -> div []
    [ newTournamentButton
    , tournamentList model
    ]
  CreatingTournament form -> creationTournamentCreationForm model form
  ViewingTournament name form ->
    showTournamentDetails model name form

creationTournamentCreationForm : a -> TournamentCreationForm -> Html MainLogic.Msg
creationTournamentCreationForm model form = 
    div []
      [ homeButton 
      , input [onInput (\name -> Typing TournamentName name)] []
      , button
          [onClick (TournamentCreation (TournamentCreated form))]
          [text "create"]
      ]
    

homeButton : Html MainLogic.Msg
homeButton = button [onClick BackToTournaments] [text "Back to home"]
newTournamentButton : Html MainLogic.Msg
newTournamentButton = button [onClick (TournamentCreation NewTournament)] [text "New Tournament"]

tournamentList : Model -> Html MainLogic.Msg
tournamentList model =
  List.map showTournament model.tournaments
  |> div [ style "display" "flex"
         , style "flex-direction" "column"
         ]

showTournament : Tournament -> Html MainLogic.Msg
showTournament tournament =
  div [style "color" "blue"
    , style "height"  ""
    , onClick (TournamentSelected tournament.name)] [text tournament.name]

showTournamentDetails : Model -> String -> ThrowerCreationForm -> Html MainLogic.Msg
showTournamentDetails model tname form =
  div []
    [ text tname 
    , homeButton
    , showThrowersTable tname model
    , input [ onInput (\name -> Typing TournamentName name)
            , placeholder "Add a thrower..."
            , value form.name]
            []
    , button [onClick (ThrowerAdded tname form)] [text "add thrower"]
    ]

showThrowersTable : TournamentName -> Model -> Html msg
showThrowersTable tname model =
  div [ style "display" "flex"
         , style "flex-direction" "column"
         ] <| List.map showThrower (throwersInTournament tname model)

showThrower : Thrower -> Html msg
showThrower thrower = 
  div [] [text thrower]

throwersInTournament : TournamentName -> Model -> List Thrower
throwersInTournament tournamentName model =
  model.tournaments
    |> List.filter (\t -> t.name == tournamentName)
    |> List.head
    |> Maybe.map .throwers
    |> Maybe.withDefault []

