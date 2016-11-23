module Model
    exposing
        ( Model
        , Maze
        , Mode(..)
        , Difficulty(..)
        , createMaze
        )

import Keyboard.Extra as Keyboard
import List.Zipper as Zipper exposing (Zipper)
import Material
import Matrix exposing (Matrix)
import Time exposing (Time)
import Window exposing (Size)


-- LOCAL IMPORTS

import MazeGenerate as MG


{-| Mazes are stored in a zip list.
-}
type alias Model =
    { mazes : Zipper Maze
    , mazeMode : Mode
    , mazeDifficulty : Difficulty
    , mazeSizePending : Int
    , timeLeft : Time
    , mazeGenerate : MG.Model
    , mdl : Material.Model
    , keyboardModel : Keyboard.Model
    , nextId : Int
    , won : Int
    , lost : Int
    , points : Int
    , windowSize : Size
    }


type alias Maze =
    { cells : Matrix MG.Cell
    , mazeSize : Int
    , center : ( Int, Int )
    , goal : ( Int, Int )
    , id : Int
    , percComplete : Int
    , timesWon : Int
    }


type Mode
    = Editing
    | Playing
    | Viewing


type Difficulty
    = Easy
    | Medium
    | Hard


createMaze : Int -> Int -> Maze
createMaze mazeSize id =
    let
        ( cx, cy ) =
            ( mazeSize - 2, mazeSize - 2 )
    in
        { cells = initialCells mazeSize
        , mazeSize = mazeSize
        , center = ( cx, cy )
        , goal = ( 0, 0 )
        , id = id
        , percComplete = 0
        , timesWon = 0
        }


initialCells : Int -> Matrix MG.Cell
initialCells size =
    Matrix.repeat size size (MG.Cell False False False False)
