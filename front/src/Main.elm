module Main exposing (..)


import Browser
import Types exposing (..)
import View exposing (view)
import MainLogic


---------- Program
main : Program () Model MainLogic.Msg
main = Browser.element
  { init = MainLogic.initProgram
  , view = view
  , update = MainLogic.update
  , subscriptions = \_ -> Sub.none
  }
