module Maze exposing (..)

import Array
import Html.App as App
import List.Zipper as Zipper exposing (Zipper)
import Material
import Keyboard.Extra as Keyboard
import Time exposing (Time)
import Matrix


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
    in
        ( { mazes = Zipper.singleton <| createMaze mazeSize 1
          , mazeMode = Viewing
          , mazeDifficulty = Easy
          , mazeSizePending = 20
          , timeLeft = easyTime
          , mazeGenerate = MG.emptyModel
          , mdl = Material.model
          , keyboardModel = keyboardModel
          , nextId = 2
          }
        , Cmd.map KeyboardExtraMsg keyboardCmd
        )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    --case Debug.log "update" msg of
    case msg of
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
                    ! [ Cmd.none ]

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

        SetTitle title ->
            let
                newMazes =
                    Zipper.current model.mazes
                        |> (\m -> Zipper.update (always { m | title = title }) model.mazes)
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
            in
                if model.mazeMode == Playing then
                    ( { model
                        | keyboardModel = keyboardModel
                        , mazes = updatedMazes
                      }
                    , Cmd.map KeyboardExtraMsg keyboardCmd
                    )
                else
                    model ! []

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
            in
                { model | timeLeft = timeLeft } ! []



-- MAIN


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map KeyboardExtraMsg Keyboard.subscriptions
        , Time.every Time.second Tick
        ]


main : Program Never
main =
    App.program
        { init = init
        , update = update
        , view = V.view
        , subscriptions = subscriptions
        }
