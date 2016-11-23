module Msg exposing (..)

import Material


-- LOCAL IMPORTS

import MazeGenerate as MG exposing (..)
import Model exposing (..)
import Keyboard.Extra as Keyboard
import Time exposing (Time)
import Window exposing (Size)


type Msg
    = Error String
    | PlayMode Mode
    | GameWon
    | GameLost
    | MazeDifficulty Difficulty
    | MazeSizePending Int
    | Mdl (Material.Msg Msg)
    | NewMaze
    | DeleteMaze Int
    | GoToNextMaze
    | GoToPreviousMaze
    | GoToMaze Int
    | KeyboardExtraMsg Keyboard.Msg
    | MazeGenerate MG.Msg
    | Tick Time
    | WindowResize Size
