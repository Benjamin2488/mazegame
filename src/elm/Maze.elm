module Maze exposing (..)

import Array
import Html.App as App
import Keyboard.Extra as Keyboard
import List.Zipper as Zipper exposing (Zipper)
import Material
import Matrix
import Task
import Time exposing (Time)
import Window


-- LOCAL IMPORTS

import MazeGenerate as MG
import Model
    exposing
        ( Model
        , Maze
        , Mode(..)
        , Difficulty(..)
        , createMaze
        )
import Msg exposing (Msg(..))
import View as V


-- MODEL


easyTime : Time
easyTime =
    120


mediumTime : Time
mediumTime =
    90


hardTime : Time
hardTime =
    60


{-| The number of blocks the maze is horizontally and
    vertically.
-}
mazeSize : Int
mazeSize =
    20


init : ( Model, Cmd Msg )
init =
    let
        ( keyboardModel, keyboardCmd ) =
            Keyboard.init

        ( mazeGenerateModel, mazeGenerateCmd ) =
            MG.update MG.Init MG.emptyModel

        -- TODO: get rid of temporary hard-code here.
        size =
            { width = 800, height = 800 }
    in
        ( { mazes = Zipper.singleton <| createMaze mazeSize 1
          , mazeMode = Viewing
          , mazeDifficulty = Easy
          , mazeSizePending = 20
          , timeLeft = easyTime
          , mazeGenerate = mazeGenerateModel
          , mdl = Material.model
          , keyboardModel = keyboardModel
          , nextId = 2
          , won = 0
          , lost = 0
          , points = 0
          , windowSize = size
          }
        , Cmd.batch
            [ Cmd.map KeyboardExtraMsg keyboardCmd
            , Task.perform Error WindowResize Window.size
            , Cmd.map (\m -> MazeGenerate m) mazeGenerateCmd
            ]
        )



-- UPDATE


calcPoints : Int -> Int -> Difficulty -> Int
calcPoints mazeSize secondsElapsed difficulty =
    let
        points =
            case compare mazeSize 20 of
                LT ->
                    case difficulty of
                        Easy ->
                            200 - secondsElapsed

                        Medium ->
                            300 - secondsElapsed

                        Hard ->
                            400 - secondsElapsed

                EQ ->
                    case difficulty of
                        Easy ->
                            800 - secondsElapsed

                        Medium ->
                            1000 - secondsElapsed

                        Hard ->
                            1200 - secondsElapsed

                GT ->
                    case difficulty of
                        Easy ->
                            1800 - secondsElapsed

                        Medium ->
                            2200 - secondsElapsed

                        Hard ->
                            2600 - secondsElapsed
    in
        points


resetCenterPlusWon : Model -> Bool -> Model
resetCenterPlusWon model didWin =
    let
        currentMaze =
            Zipper.current model.mazes

        updatedMaze =
            { currentMaze
                | center = ( currentMaze.mazeSize - 2, currentMaze.mazeSize - 2 )
                , timesWon =
                    if didWin then
                        currentMaze.timesWon + 1
                    else
                        currentMaze.timesWon
            }

        newMazes =
            Zipper.update (always updatedMaze) model.mazes
    in
        { model | mazes = newMazes }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    --case Debug.log "update" msg of
    case msg of
        Error e ->
            model ! []

        Mdl mdlMsg ->
            Material.update mdlMsg model

        PlayMode mode ->
            let
                timeLeft =
                    if mode == Playing then
                        case model.mazeDifficulty of
                            Easy ->
                                easyTime

                            Medium ->
                                mediumTime

                            Hard ->
                                hardTime
                    else
                        model.timeLeft
            in
                { model | mazeMode = mode, timeLeft = timeLeft } ! []

        GameWon ->
            -- TODO: toast the results.
            let
                currentMaze =
                    Zipper.current model.mazes

                -- We don't reward user playing the same maze that was already won
                -- with the same number of points. The more they win that maze, the
                -- less reward that there is for it.
                points =
                    calcPoints currentMaze.mazeSize (round model.timeLeft) model.mazeDifficulty
                        |> flip (//) (currentMaze.timesWon + 1)

                -- Reset center and times won for the next game.
                newModel =
                    resetCenterPlusWon model True
            in
                { newModel
                    | mazeMode = Viewing
                    , won = model.won + 1
                    , points = model.points + points
                }
                    ! []

        GameLost ->
            -- TODO: toast the results.
            let
                -- Reset center and times won for the next game.
                newModel =
                    resetCenterPlusWon model False
            in
                { newModel
                    | mazeMode = Viewing
                    , lost = model.lost + 1
                }
                    ! []

        MazeDifficulty diff ->
            { model | mazeDifficulty = diff } ! []

        MazeSizePending size ->
            let
                ( mgModel, mgCmd ) =
                    MG.update MG.MazeGenerationStop model.mazeGenerate

                newMaze =
                    createMaze size model.nextId

                newMazes =
                    Zipper.update (always newMaze) model.mazes
            in
                { model
                    | mazeSizePending = size
                    , mazes = newMazes
                    , nextId = model.nextId + 1
                    , mazeGenerate = mgModel
                }
                    ! [ Cmd.map (\m -> MazeGenerate m) mgCmd ]

        NewMaze ->
            -- Inserts the new maze after the current maze and makes it current.
            let
                newMaze =
                    createMaze 40 model.nextId

                newMazes =
                    case
                        Zipper.after model.mazes
                            |> (::) newMaze
                            |> always
                            |> flip Zipper.updateAfter model.mazes
                            |> Zipper.next
                    of
                        Just m ->
                            m

                        Nothing ->
                            -- This should not be possible.
                            model.mazes

                newModel =
                    { model | mazes = newMazes, nextId = model.nextId + 1 }
            in
                newModel ! []

        DeleteMaze idx ->
            -- We can delete any maze except that we have to leave at least
            -- one empty maze.
            let
                ( before, after ) =
                    ( Zipper.before model.mazes, Zipper.after model.mazes )

                newMazes =
                    case ( List.isEmpty before, List.isEmpty after ) of
                        ( _, False ) ->
                            Zipper.singleton (Maybe.withDefault (createMaze mazeSize (model.nextId)) (List.head after))
                                |> Zipper.updateBefore (always before)
                                |> Zipper.updateAfter (always (List.drop 1 after))

                        ( False, True ) ->
                            Zipper.singleton (Maybe.withDefault (createMaze mazeSize (model.nextId)) (List.reverse before |> List.head))
                                |> Zipper.updateBefore (always (List.take ((List.length before) - 1) before))
                                |> Zipper.updateAfter (always [])

                        ( True, True ) ->
                            Zipper.singleton (createMaze mazeSize (model.nextId))
            in
                { model | mazes = newMazes, nextId = model.nextId + 1 } ! []

        GoToPreviousMaze ->
            let
                newMazes =
                    case Zipper.previous model.mazes of
                        Just m ->
                            m

                        Nothing ->
                            model.mazes
            in
                { model | mazes = newMazes } ! []

        GoToNextMaze ->
            let
                newMazes =
                    case Zipper.next model.mazes of
                        Just m ->
                            m

                        Nothing ->
                            model.mazes
            in
                { model | mazes = newMazes } ! []

        GoToMaze idx ->
            let
                mazesList =
                    Zipper.toList model.mazes

                before =
                    List.take idx mazesList

                curr =
                    mazesList
                        |> Array.fromList
                        |> Array.get idx

                after =
                    List.drop (idx + 1) mazesList

                newMazes =
                    case curr of
                        Just c ->
                            Zipper.singleton c
                                |> Zipper.updateBefore (always before)
                                |> Zipper.updateAfter (always after)

                        Nothing ->
                            model.mazes
            in
                { model | mazes = newMazes } ! []

        KeyboardExtraMsg keyMsg ->
            let
                ( keyboardModel, keyboardCmd ) =
                    Keyboard.update keyMsg model.keyboardModel

                arrows =
                    Keyboard.arrows keyboardModel

                isarrows =
                    .x arrows /= 0 || .y arrows /= 0

                currentmaze =
                    Zipper.current model.mazes

                center =
                    currentmaze.center

                ( goalx, goaly ) =
                    ( fst currentmaze.goal, snd currentmaze.goal )

                ms =
                    currentmaze.mazeSize

                centery =
                    snd center

                centerx =
                    fst center

                cell =
                    case Matrix.get centerx centery currentmaze.cells of
                        Just c ->
                            c

                        Nothing ->
                            { northLink = False, eastLink = False, southLink = False, westLink = False }

                newx =
                    case arrows.x of
                        -1 ->
                            if centerx > 0 && cell.westLink then
                                centerx - 1
                            else
                                centerx

                        1 ->
                            if centerx == ms - 1 then
                                centerx
                            else if cell.eastLink then
                                centerx + 1
                            else
                                centerx

                        _ ->
                            centerx

                newy =
                    case arrows.y of
                        -1 ->
                            -- down arrow
                            if centery < ms - 1 && cell.southLink then
                                centery + 1
                            else
                                centery

                        1 ->
                            -- up arrow
                            if centery == 0 then
                                centery
                            else if cell.northLink then
                                centery - 1
                            else
                                centery

                        _ ->
                            centery

                updatedMazes =
                    Zipper.update (\m -> { m | center = ( newx, newy ) }) model.mazes

                newCmd =
                    if goalx == centerx && goaly == centery && model.mazeMode == Playing then
                        Task.perform (always GameWon) (always GameWon) <| Task.succeed True
                    else
                        Cmd.none
            in
                if model.mazeMode == Playing then
                    ( { model
                        | keyboardModel = keyboardModel
                        , mazes = updatedMazes
                      }
                    , Cmd.batch
                        [ Cmd.map KeyboardExtraMsg keyboardCmd
                        , newCmd
                        ]
                    )
                else
                    model ! [ newCmd ]

        MazeGenerate mgMsg ->
            let
                ( mgModel, mgCmd ) =
                    MG.update mgMsg model.mazeGenerate

                currMazeId =
                    Zipper.current model.mazes
                        |> .id

                mgMazeId =
                    Maybe.withDefault -1 mgModel.mazeId

                newMazes =
                    if mgMazeId == currMazeId then
                        Zipper.current model.mazes
                            |> (\m ->
                                    Zipper.update
                                        (always
                                            { m
                                                | cells = mgModel.cells
                                                , percComplete = mgModel.percComplete
                                            }
                                        )
                                        model.mazes
                               )
                    else
                        model.mazes
            in
                ( { model
                    | mazeGenerate = mgModel
                    , mazes = newMazes
                  }
                , Cmd.map (\m -> MazeGenerate m) mgCmd
                )

        Tick time ->
            let
                timeLeft =
                    case model.mazeMode == Playing of
                        True ->
                            max 0 (model.timeLeft - 1)

                        False ->
                            model.timeLeft

                ( newModel, newCmd ) =
                    if timeLeft == 0 && model.mazeMode == Playing then
                        update GameLost model
                    else
                        ( model, Cmd.none )
            in
                { newModel | timeLeft = timeLeft } ! [ newCmd ]

        WindowResize size ->
            { model | windowSize = size } ! []



-- MAIN


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map KeyboardExtraMsg Keyboard.subscriptions
        , Time.every Time.second Tick
        , Window.resizes WindowResize
        ]


main : Program Never
main =
    App.program
        { init = init
        , update = update
        , view = V.view
        , subscriptions = subscriptions
        }
