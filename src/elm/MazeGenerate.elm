module MazeGenerate
    exposing
        ( Msg(..)
        , MazeStatus(..)
        , Direction(..)
        , Model
        , Cell
        , emptyModel
        , update
        , initModel
          -- The functions below are exposed for testing purposes only.
        , advanceByCell
        , generateBinaryTreeStep
        , link
        )

import Matrix exposing (Matrix)
import Process
import Random
import Task
import Time


-- LOCAL IMPORTS


type Direction
    = North
    | East
    | South
    | West


type alias Model =
    { cells : Matrix Cell
    , mazeSize : Int
    , currCol : Int
    , currRow : Int
    , status : MazeStatus
    , mazeId : Maybe Int
    , percComplete : Int
    , seed : Random.Seed
    }


type MazeStatus
    = Empty
    | InProcess
    | Complete
    | Stopped


emptyModel : Model
emptyModel =
    { cells = Matrix.empty
    , mazeSize = 10
    , currCol = 0
    , currRow = 0
    , status = Empty
    , mazeId = Nothing
    , percComplete = 0
    , seed = Random.initialSeed 0
    }


type alias Cell =
    { northLink : Bool
    , eastLink : Bool
    , southLink : Bool
    , westLink : Bool
    }


type Msg
    = Init
    | Error String
    | BinaryTreeInit Int Int
    | BinaryTreeUpdate Bool
    | BinaryTreeDoRandom
    | BinaryTreeComplete
    | MazeGenerationStop
    | NewSeed Time.Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Init ->
            emptyModel ! [ Time.now |> Task.perform (always Error "") NewSeed ]

        Error e ->
            model ! []

        NewSeed time ->
            { model | seed = (Random.initialSeed <| round time) } ! []

        BinaryTreeInit size mazeId ->
            let
                -- Initialize the cells to the proper size but only if we are not
                -- already processing another maze and have not processed this
                -- one yet.
                ( initializedModel, newCmd ) =
                    if
                        model.status
                            == Empty
                            || model.status
                            == Complete
                            || model.status
                            == Stopped
                    then
                        ( initModel
                            { emptyModel
                                | mazeSize = size
                                , status = InProcess
                                , mazeId = Just mazeId
                                , percComplete = 0
                            }
                        , Random.generate BinaryTreeUpdate Random.bool
                        )
                    else
                        ( model, Cmd.none )
            in
                initializedModel ! [ newCmd ]

        BinaryTreeUpdate bool ->
            let
                -- Get a number of links done without coming up for air.
                ( batchModel, _ ) =
                    if model.status == Stopped then
                        ( model, Cmd.none )
                    else
                        generateBinaryTreeBatch model

                -- This will do one more link but return a Cmd that
                -- allows TEA to respond to other events.
                ( newModel, newCmd ) =
                    if model.status == Stopped then
                        ( model, Cmd.none )
                    else
                        generateBinaryTreeStep bool batchModel

                percComplete =
                    if model.status /= Stopped then
                        newModel.mazeSize
                            * newModel.mazeSize
                            |> toFloat
                            |> (/) ((toFloat (newModel.currRow * newModel.mazeSize)) + (toFloat newModel.currCol))
                            |> (*) 100.0
                            |> round
                    else
                        0
            in
                { newModel | percComplete = percComplete } ! [ newCmd ]

        BinaryTreeDoRandom ->
            {- : Generate a random Bool for the BinaryTree algorithm. It is possible
               to directly generate the Bool using BinaryTreeUpdate as the target
               but it will lock up the UI.
            -}
            ( model, Random.generate BinaryTreeUpdate Random.bool )

        BinaryTreeComplete ->
            { model | percComplete = 100 } ! []

        MazeGenerationStop ->
            let
                newStatus =
                    if model.status == InProcess then
                        Stopped
                    else
                        model.status
            in
                { model | status = newStatus } ! []


{-| Initialize the cells field of the model with unlinked cells according to
    the mazeSize field.
-}
initModel : Model -> Model
initModel model =
    let
        matrix =
            Matrix.repeat model.mazeSize model.mazeSize (Cell False False False False)
    in
        { model | cells = matrix }


{-| Do four links at once without entering the TEA cycle.
This allows for faster maze generation. This needs
to be interspersed with allowances for the TEA cycle in
order to allow the UI to be somewhat responsive during
maze generation.
-}
generateBinaryTreeBatch : Model -> ( Model, Cmd Msg )
generateBinaryTreeBatch model =
    let
        ( boolList, newSeed ) =
            Random.step (Random.list 5 Random.bool) model.seed

        ( b1, b2, b3, b4, b5 ) =
            ( Maybe.withDefault False <| List.head boolList
            , Maybe.withDefault False <| List.head <| List.drop 1 boolList
            , Maybe.withDefault False <| List.head <| List.drop 2 boolList
            , Maybe.withDefault False <| List.head <| List.drop 3 boolList
            , Maybe.withDefault False <| List.head <| List.drop 4 boolList
            )

        ( model1, isComplete1 ) =
            generateBinaryTreeLink b1 model

        ( model2, isComplete2 ) =
            if not isComplete1 then
                generateBinaryTreeLink b2 model1
            else
                ( model1, True )

        ( model3, isComplete3 ) =
            if not isComplete2 then
                generateBinaryTreeLink b3 model2
            else
                ( model2, True )

        ( model4, isComplete4 ) =
            if not isComplete3 then
                generateBinaryTreeLink b4 model3
            else
                ( model3, True )

        ( model5, isComplete5 ) =
            if not isComplete4 then
                generateBinaryTreeLink b5 model4
            else
                ( model4, True )
    in
        { model5 | seed = newSeed } ! []


generateBinaryTreeLink : Bool -> Model -> ( Model, Bool )
generateBinaryTreeLink bool model =
    let
        ( col, row ) =
            ( model.currCol, model.currRow )

        cell =
            Matrix.get col row model.cells

        ( model1, isCompleted ) =
            case cell of
                Just c ->
                    if row /= (model.mazeSize - 1) then
                        -- Not the last row.
                        if col == (model.mazeSize - 1) then
                            -- Last column of row, only go south.
                            link col row c South model
                                |> advanceByCell
                        else
                            -- Otherwise, randomly choose a direction.
                            link col
                                row
                                c
                                (if bool then
                                    East
                                 else
                                    South
                                )
                                model
                                |> advanceByCell
                    else if col == (model.mazeSize - 1) then
                        -- Last cell of last row.
                        ( model, True )
                    else
                        -- Last row but not last cell, only go east.
                        link col row c East model
                            |> advanceByCell

                Nothing ->
                    -- If we get here, something is wrong.
                    ( model, True )

        newModel =
            if isCompleted then
                { model1 | status = Complete }
            else
                model1
    in
        ( newModel, isCompleted )


{-| Generate a single step of the maze using the Binary Tree algorithm. Returns the
    updated model as well as a Cmd that leads to the next step.
-}
generateBinaryTreeStep : Bool -> Model -> ( Model, Cmd Msg )
generateBinaryTreeStep bool model =
    let
        ( col, row ) =
            ( model.currCol, model.currRow )

        cell =
            Matrix.get col row model.cells

        ( model1, isCompleted ) =
            case cell of
                Just c ->
                    if row /= (model.mazeSize - 1) then
                        -- Not the last row.
                        if col == (model.mazeSize - 1) then
                            -- Last column of row, only go south.
                            link col row c South model
                                |> advanceByCell
                        else
                            -- Otherwise, randomly choose a direction.
                            link col
                                row
                                c
                                (if bool then
                                    East
                                 else
                                    South
                                )
                                model
                                |> advanceByCell
                    else if col == (model.mazeSize - 1) then
                        -- Last cell of last row.
                        ( model, True )
                    else
                        -- Last row but not last cell, only go east.
                        link col row c East model
                            |> advanceByCell

                Nothing ->
                    -- If we get here, something is wrong.
                    ( model, True )

        newModel =
            if isCompleted then
                { model1 | status = Complete }
            else
                model1

        newCmd =
            if isCompleted then
                Task.perform (always BinaryTreeComplete) (always BinaryTreeComplete) (Task.succeed True)
            else
                {- : Do this to skip the sleep altogether. This is about three
                   times faster but it ties up the UI.
                -}
                --Random.generate BinaryTreeUpdate Random.bool
                {- : Here we sleep to avoid locking up the screen. The BinaryTreeDoRandom case in
                   update will perform the actual random Bool generation.
                -}
                Task.perform (always BinaryTreeDoRandom) (always BinaryTreeDoRandom) (Process.sleep 0)
    in
        newModel ! [ newCmd ]


{-| Advance the currCol and currRow fields of the model by cell moving by
    cell within row then by row ascending. Return True in the second element
    of the tuple to signify completion.
-}
advanceByCell : Model -> ( Model, Bool )
advanceByCell model =
    let
        ( col, row ) =
            ( model.currCol, model.currRow )

        ( newModel, isCompleted ) =
            if row /= (model.mazeSize - 1) then
                if col == (model.mazeSize - 1) then
                    ( { model | currCol = 0, currRow = row + 1 }, False )
                else
                    ( { model | currCol = col + 1 }, False )
            else if col /= (model.mazeSize - 1) then
                ( { model | currCol = col + 1 }, False )
            else
                ( model, True )
    in
        ( newModel, isCompleted )


{-| "Link" two cells by setting the link field on their facing walls to True.
    Linked cells do not have a wall between them.
-}
link : Int -> Int -> Cell -> Direction -> Model -> Model
link col row cell direction model =
    let
        -- Set the directional link in the cell passed and the corresponding cell
        -- if there is one (cells on a border may not have one).
        ( updatedCell, linkedCell, linkedCol, linkedRow ) =
            case direction of
                North ->
                    let
                        ( linkedCol, linkedRow ) =
                            ( col, row - 1 )
                    in
                        ( { cell | northLink = True }
                        , case Matrix.get linkedCol linkedRow model.cells of
                            Nothing ->
                                Nothing

                            Just c ->
                                Just { c | southLink = True }
                        , linkedCol
                        , linkedRow
                        )

                East ->
                    let
                        ( linkedCol, linkedRow ) =
                            ( col + 1, row )
                    in
                        ( { cell | eastLink = True }
                        , case Matrix.get linkedCol linkedRow model.cells of
                            Nothing ->
                                Nothing

                            Just c ->
                                Just { c | westLink = True }
                        , linkedCol
                        , linkedRow
                        )

                South ->
                    let
                        ( linkedCol, linkedRow ) =
                            ( col, row + 1 )
                    in
                        ( { cell | southLink = True }
                        , case Matrix.get linkedCol linkedRow model.cells of
                            Nothing ->
                                Nothing

                            Just c ->
                                Just { c | northLink = True }
                        , linkedCol
                        , linkedRow
                        )

                West ->
                    let
                        ( linkedCol, linkedRow ) =
                            ( col - 1, row )
                    in
                        ( { cell | westLink = True }
                        , case Matrix.get linkedCol linkedRow model.cells of
                            Nothing ->
                                Nothing

                            Just c ->
                                Just { c | eastLink = True }
                        , linkedCol
                        , linkedRow
                        )

        -- Create a new set of cells with the updated cell and possibly the linked cell.
        newCells =
            case linkedCell of
                Just c2 ->
                    Matrix.update col
                        row
                        (\c ->
                            { c
                                | northLink = updatedCell.northLink
                                , eastLink = updatedCell.eastLink
                                , southLink = updatedCell.southLink
                                , westLink = updatedCell.westLink
                            }
                        )
                        model.cells
                        |> Matrix.update linkedCol
                            linkedRow
                            (\c ->
                                { c
                                    | northLink = c2.northLink
                                    , eastLink = c2.eastLink
                                    , southLink = c2.southLink
                                    , westLink = c2.westLink
                                }
                            )

                Nothing ->
                    Matrix.update col
                        row
                        (\c ->
                            { c
                                | northLink = updatedCell.northLink
                                , eastLink = updatedCell.eastLink
                                , southLink = updatedCell.southLink
                                , westLink = updatedCell.westLink
                            }
                        )
                        model.cells
    in
        { model | cells = newCells }
