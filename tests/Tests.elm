module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import Array


-- Project specific imports.

import Material
import Matrix
import TestData exposing (blockSize, gameWindowSize, displayWindowSize)


-- LOCAL IMPORTS

import Maze as M
import MazeGenerate as MG
import Model exposing (Mode(..), Difficulty(..))


binaryTreeEast : Bool
binaryTreeEast =
    True


binaryTreeSouth : Bool
binaryTreeSouth =
    False


all : Test
all =
    describe "MazeGenerate module"
        [ modelTests
        , advanceByCellTests
        , generateBinaryTreeTests
        , linkTests
        , updateTests
        , calcPointsTests
        ]


calcPointsTests : Test
calcPointsTests =
    describe "calcPoints"
        [ test "10x10, 0 seconds, Easy"
            <| \() ->
                let
                    points =
                        M.calcPoints 10 0 Easy
                in
                    Expect.equal points 200
        , test "10x10, 23 seconds, Easy"
            <| \() ->
                let
                    points =
                        M.calcPoints 10 23 Easy
                in
                    Expect.equal points 177
        , test "10x10, 119 seconds, Easy"
            <| \() ->
                let
                    points =
                        M.calcPoints 10 119 Easy
                in
                    Expect.equal points 81
        , test "10x10, 0 seconds, Medium"
            <| \() ->
                let
                    points =
                        M.calcPoints 10 0 Medium
                in
                    Expect.equal points 300
        , test "10x10, 55 seconds, Medium"
            <| \() ->
                let
                    points =
                        M.calcPoints 10 55 Medium
                in
                    Expect.equal points 245
        , test "10x10, 89 seconds, Medium"
            <| \() ->
                let
                    points =
                        M.calcPoints 10 89 Medium
                in
                    Expect.equal points 211
        , test "10x10, 0 seconds, Hard"
            <| \() ->
                let
                    points =
                        M.calcPoints 10 0 Hard
                in
                    Expect.equal points 400
        , test "20x20, 0 seconds, Easy"
            <| \() ->
                let
                    points =
                        M.calcPoints 20 0 Easy
                in
                    Expect.equal points 800
        , test "20x20, 45 seconds, Easy"
            <| \() ->
                let
                    points =
                        M.calcPoints 20 45 Easy
                in
                    Expect.equal points 755
        , test "20x20, 119 seconds, Easy"
            <| \() ->
                let
                    points =
                        M.calcPoints 20 119 Easy
                in
                    Expect.equal points 681
        , test "20x20, 0 seconds, Medium"
            <| \() ->
                let
                    points =
                        M.calcPoints 20 0 Medium
                in
                    Expect.equal points 1000
        , test "20x20, 88 seconds, Medium"
            <| \() ->
                let
                    points =
                        M.calcPoints 20 88 Medium
                in
                    Expect.equal points 912
        , test "20x20, 119 seconds, Medium"
            <| \() ->
                let
                    points =
                        M.calcPoints 20 119 Medium
                in
                    Expect.equal points 881
        , test "20x20, 0 seconds, Hard"
            <| \() ->
                let
                    points =
                        M.calcPoints 20 0 Hard
                in
                    Expect.equal points 1200
        , test "20x20, 66 seconds, Hard"
            <| \() ->
                let
                    points =
                        M.calcPoints 20 66 Hard
                in
                    Expect.equal points 1134
        , test "20x20, 119 seconds, Hard"
            <| \() ->
                let
                    points =
                        M.calcPoints 20 119 Hard
                in
                    Expect.equal points 1081
        , test "40x40, 0 seconds, Easy"
            <| \() ->
                let
                    points =
                        M.calcPoints 40 0 Easy
                in
                    Expect.equal points 1800
        , test "40x40, 33 seconds, Easy"
            <| \() ->
                let
                    points =
                        M.calcPoints 40 33 Easy
                in
                    Expect.equal points 1767
        , test "40x40, 119 seconds, Easy"
            <| \() ->
                let
                    points =
                        M.calcPoints 40 119 Easy
                in
                    Expect.equal points 1681
        , test "40x40, 0 seconds, Medium"
            <| \() ->
                let
                    points =
                        M.calcPoints 40 0 Medium
                in
                    Expect.equal points 2200
        , test "40x40, 57 seconds, Medium"
            <| \() ->
                let
                    points =
                        M.calcPoints 40 57 Medium
                in
                    Expect.equal points 2143
        , test "40x40, 119 seconds, Medium"
            <| \() ->
                let
                    points =
                        M.calcPoints 40 119 Medium
                in
                    Expect.equal points 2081
        , test "40x40, 0 seconds, Hard"
            <| \() ->
                let
                    points =
                        M.calcPoints 40 0 Hard
                in
                    Expect.equal points 2600
        , test "40x40, 38 seconds, Hard"
            <| \() ->
                let
                    points =
                        M.calcPoints 40 38 Hard
                in
                    Expect.equal points 2562
        , test "40x40, 119 seconds, Hard"
            <| \() ->
                let
                    points =
                        M.calcPoints 40 119 Hard
                in
                    Expect.equal points 2481
        ]


linkTests : Test
linkTests =
    describe "link"
        [ test "North link: top row"
            <| \() ->
                let
                    -- Initialize the model.
                    ( col, row, model ) =
                        ( 1
                        , 0
                        , MG.emptyModel
                            |> (\m -> { m | mazeSize = 5 })
                            |> MG.initModel
                        )

                    -- Do the link.
                    newModel =
                        case Matrix.get col row model.cells of
                            Just c ->
                                MG.link col row c MG.North model

                            Nothing ->
                                model

                    -- Query the links.
                    ( hasNorthLink, hasSouthLink ) =
                        ( case Matrix.get col row newModel.cells of
                            Just c ->
                                c.northLink

                            Nothing ->
                                False
                        , case Matrix.get col (row - 1) newModel.cells of
                            Just c ->
                                c.southLink

                            Nothing ->
                                False
                        )
                in
                    Expect.equal (hasNorthLink && not hasSouthLink) True
        , test "North link: middle row"
            <| \() ->
                let
                    -- Initialize the model.
                    ( col, row, model ) =
                        ( 1
                        , 3
                        , MG.emptyModel
                            |> (\m -> { m | mazeSize = 5 })
                            |> MG.initModel
                        )

                    -- Do the link.
                    newModel =
                        case Matrix.get col row model.cells of
                            Just c ->
                                MG.link col row c MG.North model

                            Nothing ->
                                model

                    -- Query the links.
                    ( hasNorthLink, hasSouthLink ) =
                        ( case Matrix.get col row newModel.cells of
                            Just c ->
                                c.northLink

                            Nothing ->
                                False
                        , case Matrix.get col (row - 1) newModel.cells of
                            Just c ->
                                c.southLink

                            Nothing ->
                                False
                        )
                in
                    Expect.equal (hasNorthLink && hasSouthLink) True
        , test "North link: bottom row"
            <| \() ->
                let
                    -- Initialize the model.
                    ( col, row, model ) =
                        ( 1
                        , 4
                        , MG.emptyModel
                            |> (\m -> { m | mazeSize = 5 })
                            |> MG.initModel
                        )

                    -- Do the link.
                    newModel =
                        case Matrix.get col row model.cells of
                            Just c ->
                                MG.link col row c MG.North model

                            Nothing ->
                                model

                    -- Query the links.
                    ( hasNorthLink, hasSouthLink ) =
                        ( case Matrix.get col row newModel.cells of
                            Just c ->
                                c.northLink

                            Nothing ->
                                False
                        , case Matrix.get col (row - 1) newModel.cells of
                            Just c ->
                                c.southLink

                            Nothing ->
                                False
                        )
                in
                    Expect.equal (hasNorthLink && hasSouthLink) True
        , test "South link: Top row"
            <| \() ->
                let
                    -- Initialize the model.
                    ( col, row, model ) =
                        ( 1
                        , 0
                        , MG.emptyModel
                            |> (\m -> { m | mazeSize = 5 })
                            |> MG.initModel
                        )

                    -- Do the link.
                    newModel =
                        case Matrix.get col row model.cells of
                            Just c ->
                                MG.link col row c MG.South model

                            Nothing ->
                                model

                    -- Query the links.
                    ( hasSouthLink, hasNorthLink ) =
                        ( case Matrix.get col row newModel.cells of
                            Just c ->
                                c.southLink

                            Nothing ->
                                False
                        , case Matrix.get col (row + 1) newModel.cells of
                            Just c ->
                                c.northLink

                            Nothing ->
                                False
                        )
                in
                    Expect.equal (hasNorthLink && hasSouthLink) True
        , test "South link: Middle row"
            <| \() ->
                let
                    -- Initialize the model.
                    ( col, row, model ) =
                        ( 1
                        , 3
                        , MG.emptyModel
                            |> (\m -> { m | mazeSize = 5 })
                            |> MG.initModel
                        )

                    -- Do the link.
                    newModel =
                        case Matrix.get col row model.cells of
                            Just c ->
                                MG.link col row c MG.South model

                            Nothing ->
                                model

                    -- Query the links.
                    ( hasSouthLink, hasNorthLink ) =
                        ( case Matrix.get col row newModel.cells of
                            Just c ->
                                c.southLink

                            Nothing ->
                                False
                        , case Matrix.get col (row + 1) newModel.cells of
                            Just c ->
                                c.northLink

                            Nothing ->
                                False
                        )
                in
                    Expect.equal (hasNorthLink && hasSouthLink) True
        , test "South link: Bottom row"
            <| \() ->
                let
                    -- Initialize the model.
                    ( col, row, model ) =
                        ( 1
                        , 4
                        , MG.emptyModel
                            |> (\m -> { m | mazeSize = 5 })
                            |> MG.initModel
                        )

                    -- Do the link.
                    newModel =
                        case Matrix.get col row model.cells of
                            Just c ->
                                MG.link col row c MG.South model

                            Nothing ->
                                model

                    -- Query the links.
                    ( hasSouthLink, hasNorthLink ) =
                        ( case Matrix.get col row newModel.cells of
                            Just c ->
                                c.southLink

                            Nothing ->
                                False
                        , case Matrix.get col (row + 1) newModel.cells of
                            Just c ->
                                c.northLink

                            Nothing ->
                                False
                        )
                in
                    Expect.equal (hasSouthLink && not hasNorthLink) True
        , test "East link: Left column"
            <| \() ->
                let
                    -- Initialize the model.
                    ( col, row, model ) =
                        ( 0
                        , 2
                        , MG.emptyModel
                            |> (\m -> { m | mazeSize = 5 })
                            |> MG.initModel
                        )

                    -- Do the link.
                    newModel =
                        case Matrix.get col row model.cells of
                            Just c ->
                                MG.link col row c MG.East model

                            Nothing ->
                                model

                    -- Query the links.
                    ( hasEastLink, hasWestLink ) =
                        ( case Matrix.get col row newModel.cells of
                            Just c ->
                                c.eastLink

                            Nothing ->
                                False
                        , case Matrix.get (col + 1) row newModel.cells of
                            Just c ->
                                c.westLink

                            Nothing ->
                                False
                        )
                in
                    Expect.equal (hasEastLink && hasWestLink) True
        , test "East link: middle column"
            <| \() ->
                let
                    -- Initialize the model.
                    ( col, row, model ) =
                        ( 2
                        , 2
                        , MG.emptyModel
                            |> (\m -> { m | mazeSize = 5 })
                            |> MG.initModel
                        )

                    -- Do the link.
                    newModel =
                        case Matrix.get col row model.cells of
                            Just c ->
                                MG.link col row c MG.East model

                            Nothing ->
                                model

                    -- Query the links.
                    ( hasEastLink, hasWestLink ) =
                        ( case Matrix.get col row newModel.cells of
                            Just c ->
                                c.eastLink

                            Nothing ->
                                False
                        , case Matrix.get (col + 1) row newModel.cells of
                            Just c ->
                                c.westLink

                            Nothing ->
                                False
                        )
                in
                    Expect.equal (hasEastLink && hasWestLink) True
        , test "East link: right column"
            <| \() ->
                let
                    -- Initialize the model.
                    ( col, row, model ) =
                        ( 4
                        , 2
                        , MG.emptyModel
                            |> (\m -> { m | mazeSize = 5 })
                            |> MG.initModel
                        )

                    -- Do the link.
                    newModel =
                        case Matrix.get col row model.cells of
                            Just c ->
                                MG.link col row c MG.East model

                            Nothing ->
                                model

                    -- Query the links.
                    ( hasEastLink, hasWestLink ) =
                        ( case Matrix.get col row newModel.cells of
                            Just c ->
                                c.eastLink

                            Nothing ->
                                False
                        , case Matrix.get (col + 1) row newModel.cells of
                            Just c ->
                                c.westLink

                            Nothing ->
                                False
                        )
                in
                    Expect.equal (hasEastLink && not hasWestLink) True
        , test "West link: left column"
            <| \() ->
                let
                    -- Initialize the model.
                    ( col, row, model ) =
                        ( 0
                        , 2
                        , MG.emptyModel
                            |> (\m -> { m | mazeSize = 5 })
                            |> MG.initModel
                        )

                    -- Do the link.
                    newModel =
                        case Matrix.get col row model.cells of
                            Just c ->
                                MG.link col row c MG.West model

                            Nothing ->
                                model

                    -- Query the links.
                    ( hasWestLink, hasEastLink ) =
                        ( case Matrix.get col row newModel.cells of
                            Just c ->
                                c.westLink

                            Nothing ->
                                False
                        , case Matrix.get (col - 1) row newModel.cells of
                            Just c ->
                                c.eastLink

                            Nothing ->
                                False
                        )
                in
                    Expect.equal (hasWestLink && not hasEastLink) True
        , test "West link: middle column"
            <| \() ->
                let
                    -- Initialize the model.
                    ( col, row, model ) =
                        ( 2
                        , 2
                        , MG.emptyModel
                            |> (\m -> { m | mazeSize = 5 })
                            |> MG.initModel
                        )

                    -- Do the link.
                    newModel =
                        case Matrix.get col row model.cells of
                            Just c ->
                                MG.link col row c MG.West model

                            Nothing ->
                                model

                    -- Query the links.
                    ( hasWestLink, hasEastLink ) =
                        ( case Matrix.get col row newModel.cells of
                            Just c ->
                                c.westLink

                            Nothing ->
                                False
                        , case Matrix.get (col - 1) row newModel.cells of
                            Just c ->
                                c.eastLink

                            Nothing ->
                                False
                        )
                in
                    Expect.equal (hasWestLink && hasEastLink) True
        , test "West link: right column"
            <| \() ->
                let
                    -- Initialize the model.
                    ( col, row, model ) =
                        ( 4
                        , 2
                        , MG.emptyModel
                            |> (\m -> { m | mazeSize = 5 })
                            |> MG.initModel
                        )

                    -- Do the link.
                    newModel =
                        case Matrix.get col row model.cells of
                            Just c ->
                                MG.link col row c MG.West model

                            Nothing ->
                                model

                    -- Query the links.
                    ( hasWestLink, hasEastLink ) =
                        ( case Matrix.get col row newModel.cells of
                            Just c ->
                                c.westLink

                            Nothing ->
                                False
                        , case Matrix.get (col - 1) row newModel.cells of
                            Just c ->
                                c.eastLink

                            Nothing ->
                                False
                        )
                in
                    Expect.equal (hasWestLink && hasEastLink) True
        ]


generateBinaryTreeTests : Test
generateBinaryTreeTests =
    describe "generateBinaryTreeStep"
        [ test "Middle cell can link south"
            <| \() ->
                let
                    ( ms, cCol, cRow ) =
                        ( 5, 2, 2 )

                    initialModel =
                        MG.emptyModel
                            |> (\m -> { m | mazeSize = ms, currCol = cCol, currRow = cRow })
                            |> MG.initModel

                    ( model, cmd ) =
                        MG.generateBinaryTreeStep binaryTreeSouth initialModel

                    isLinkSouth =
                        case
                            model.cells
                                |> Matrix.get cCol cRow
                        of
                            Just c ->
                                c.southLink

                            Nothing ->
                                False
                in
                    Expect.equal isLinkSouth True
        , test "Middle cell can link east"
            <| \() ->
                let
                    ( ms, cCol, cRow ) =
                        ( 5, 2, 2 )

                    initialModel =
                        MG.emptyModel
                            |> (\m -> { m | mazeSize = ms, currCol = cCol, currRow = cRow })
                            |> MG.initModel

                    ( model, cmd ) =
                        MG.generateBinaryTreeStep binaryTreeEast initialModel

                    isLinkEast =
                        case
                            model.cells
                                |> Matrix.get cCol cRow
                        of
                            Just c ->
                                c.eastLink

                            Nothing ->
                                False
                in
                    Expect.equal isLinkEast True
        , test "Last col in row should link south"
            <| \() ->
                let
                    ( ms, cCol, cRow ) =
                        ( 5, 4, 1 )

                    initialModel =
                        MG.emptyModel
                            |> (\m -> { m | mazeSize = ms, currCol = cCol, currRow = cRow })
                            |> MG.initModel

                    ( model, cmd ) =
                        MG.generateBinaryTreeStep binaryTreeEast initialModel

                    isLinkSouth =
                        case
                            model.cells
                                |> Matrix.get cCol cRow
                        of
                            Just c ->
                                c.southLink

                            Nothing ->
                                False
                in
                    Expect.equal isLinkSouth True
        , test "A middle cell in last row should link east"
            <| \() ->
                let
                    ( ms, cCol, cRow ) =
                        ( 5, 3, 4 )

                    initialModel =
                        MG.emptyModel
                            |> (\m -> { m | mazeSize = ms, currCol = cCol, currRow = cRow })
                            |> MG.initModel

                    ( model, cmd ) =
                        MG.generateBinaryTreeStep binaryTreeSouth initialModel

                    isLinkEast =
                        case
                            model.cells
                                |> Matrix.get cCol cRow
                        of
                            Just c ->
                                c.eastLink

                            Nothing ->
                                False
                in
                    Expect.equal isLinkEast True
        , test "Last cell of last row should not link east or south"
            <| \() ->
                let
                    ( ms, cCol, cRow ) =
                        ( 5, 4, 4 )

                    initialModel =
                        MG.emptyModel
                            |> (\m -> { m | mazeSize = ms, currCol = cCol, currRow = cRow })
                            |> MG.initModel

                    ( model, cmd ) =
                        MG.generateBinaryTreeStep binaryTreeSouth initialModel

                    ( isEastLink, isSouthLink ) =
                        case
                            model.cells
                                |> Matrix.get cCol cRow
                        of
                            Just c ->
                                ( c.eastLink, c.southLink )

                            Nothing ->
                                ( True, True )
                in
                    Expect.equal (isEastLink == False && isSouthLink == False) True
        ]


updateTests : Test
updateTests =
    describe "update"
        [ test "BinaryTreeInit creates maze of proper size and id"
            <| \() ->
                let
                    ( ms, id ) =
                        ( 12, 100 )

                    ( mod, _ ) =
                        MG.update (MG.BinaryTreeInit ms id) MG.emptyModel
                in
                    Expect.equal (mod.mazeSize == ms && mod.mazeId == (Just id)) True
        , test "BinaryTreeInit creates Cmd msg"
            <| \() ->
                let
                    ( ms, id ) =
                        ( 14, 200 )

                    ( _, cmd ) =
                        MG.update (MG.BinaryTreeInit ms id) MG.emptyModel
                in
                    Expect.equal (cmd /= Cmd.none) True
        , test "BinaryTreeInit does nothing when status /= Empty or Complete"
            <| \() ->
                let
                    ( ms, id ) =
                        ( 15, 300 )

                    ( mod, _ ) =
                        MG.emptyModel
                            |> (\m -> { m | status = MG.InProcess })
                            |> MG.update (MG.BinaryTreeInit ms id)
                in
                    Expect.equal (mod.mazeSize /= ms && mod.mazeId /= (Just id)) True
        ]


advanceByCellTests : Test
advanceByCellTests =
    describe "advanceByCell"
        [ test "Advances cell by one when not at end of row or last row and returns not compete"
            <| \() ->
                let
                    ( ms, id, currCol, currRow ) =
                        ( 14, 100, 1, 1 )

                    ( model, isComplete ) =
                        MG.emptyModel
                            |> (\m -> { m | mazeSize = ms, currCol = currCol, currRow = currRow })
                            |> MG.initModel
                            |> MG.advanceByCell
                in
                    Expect.equal
                        (model.currCol
                            == (currCol + 1)
                            && model.currRow
                            == currRow
                            && isComplete
                            == False
                        )
                        True
        , test "Advances cell to next row when not at last row nor at end of column and returns not compete"
            <| \() ->
                let
                    ( ms, id, currCol, currRow ) =
                        ( 15, 100, 14, 0 )

                    ( model, isComplete ) =
                        MG.emptyModel
                            |> (\m -> { m | mazeSize = ms, currCol = currCol, currRow = currRow })
                            |> MG.initModel
                            |> MG.advanceByCell
                in
                    Expect.equal (model.currCol == 0 && model.currRow == (currRow + 1) && isComplete == False)
                        True
        , test "Advances cell when on last row but not last cell and returns not complete"
            <| \() ->
                let
                    ( ms, id, currCol, currRow ) =
                        ( 15, 100, 12, 14 )

                    ( model, isComplete ) =
                        MG.emptyModel
                            |> (\m -> { m | mazeSize = ms, currCol = currCol, currRow = currRow })
                            |> MG.initModel
                            |> MG.advanceByCell
                in
                    Expect.equal (model.currCol == (currCol + 1) && model.currRow == currRow && isComplete == False)
                        True
        , test "Does not advance cell when at last cell in last row and returns complete"
            <| \() ->
                let
                    ( ms, id, currCol, currRow ) =
                        ( 15, 100, 14, 14 )

                    ( model, isComplete ) =
                        MG.emptyModel
                            |> (\m -> { m | mazeSize = ms, currCol = currCol, currRow = currRow })
                            |> MG.initModel
                            |> MG.advanceByCell
                in
                    Expect.equal (model.currCol == currCol && model.currRow == currRow && isComplete == True)
                        True
        ]


modelTests : Test
modelTests =
    describe "emptyModel and initModel"
        [ test "emptyModel should have proper fields"
            <| \() ->
                let
                    model =
                        MG.emptyModel

                    ( isEmpty, currPos, validStatus, unassignedId, validPerc ) =
                        ( Matrix.height model.cells == 0 && Matrix.width model.cells == 0
                        , model.currCol == 0 && model.currRow == 0
                        , model.status == MG.Empty
                        , model.mazeId == Nothing
                        , model.percComplete == 0
                        )
                in
                    Expect.equal (isEmpty && currPos && validStatus && unassignedId && validPerc) True
        , test "Create a maze with initModel of a certain size with proper number of cells"
            <| \() ->
                let
                    ms =
                        6

                    model =
                        MG.emptyModel
                            |> (\m -> { m | mazeSize = ms })
                            |> MG.initModel
                in
                    Expect.equal
                        ((model.mazeSize == ms)
                            && (Matrix.width model.cells == ms)
                            && (Matrix.height model.cells == ms)
                        )
                        True
        , test "Create a maze with initModel and all cells should have links set to False"
            <| \() ->
                let
                    ms =
                        7

                    numLinkedCells =
                        MG.emptyModel
                            |> (\m -> { m | mazeSize = ms })
                            |> MG.initModel
                            |> .cells
                            |> Matrix.filter
                                (\c ->
                                    c.northLink
                                        == True
                                        || c.eastLink
                                        == True
                                        || c.southLink
                                        == True
                                        || c.westLink
                                        == True
                                )
                            |> Array.length
                in
                    Expect.equal numLinkedCells 0
        ]
