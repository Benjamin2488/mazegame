module View exposing (..)

import Array
import Html exposing (Html, div, p, text)
import Html.Events as Html
import List
import List.Zipper as Zipper exposing (Zipper)
import Matrix
import Material
import Material.Button as Button
import Material.Card as Card
import Material.Chip as Chip
import Material.Color as Color
import Material.Grid as Grid exposing (grid, cell, size, Device(..))
import Material.Layout as Layout
import Material.Options as Options
import Material.Toggles as Toggles
import Material.Typography as Typo
import Svg as S
import Svg.Attributes as S


-- LOCAL IMPORTS

import MazeGenerate as MG
import Model
    exposing
        ( Model
        , Maze
        , Mode(..)
        , Difficulty(..)
        )
import Msg exposing (..)


--import Util as U


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


intToPx : Int -> String
intToPx int =
    int |> toString |> flip (++) "px"


fourIntToString : Int -> Int -> Int -> Int -> String
fourIntToString a b c d =
    ((toString a) ++ " " ++ (toString b) ++ " " ++ (toString c) ++ " " ++ (toString d))


type alias Mdl =
    Material.Model


{-| MDL contexts.
-}
viewViewingContext : Int
viewViewingContext =
    1000


viewEditingContext : Int
viewEditingContext =
    2000


viewPlayingContext : Int
viewPlayingContext =
    3000


mazeMetaInfoContext : Int
mazeMetaInfoContext =
    4000


view : Model -> Html Msg
view model =
    Layout.render Mdl
        model.mdl
        [ Layout.fixedHeader
        ]
        { header = headerSmall "A Maze Game" model
        , drawer = []
        , tabs = ( [], [] )
        , main = [ viewMain model ]
        }


viewMain : Model -> Html Msg
viewMain model =
    let
        view =
            case model.mazeMode of
                Viewing ->
                    viewViewing model

                Editing ->
                    viewEditing model

                Playing ->
                    viewPlaying model
    in
        view


viewPlaying : Model -> Html Msg
viewPlaying model =
    let
        ( mazesList, currMazeIdx ) =
            ( Zipper.toList model.mazes
            , Zipper.before model.mazes
                |> List.length
            )
    in
        grid
            [ Color.background <| Color.accentContrast
            ]
            [ cell
                -- Maze on the left.
                [ size Desktop 10
                , size Tablet 7
                , size Phone 4
                ]
                <| [ viewMaze model ]
            , cell
                -- Done Playing button.
                [ size Desktop 2
                , size Tablet 1
                , size Phone 4
                ]
                [ grid []
                    [ cell
                        [ size Desktop 12
                        , size Tablet 8
                        , size Phone 4
                        ]
                        [ Card.view []
                            [ Card.text []
                                [ Options.styled div
                                    [ Typo.subhead
                                    , Color.text Color.accent
                                    , Color.background Color.accentContrast
                                    ]
                                    [ text "Find the" ]
                                , Options.styled div
                                    [ Typo.subhead
                                    , Color.text Color.accent
                                    , Color.background Color.accentContrast
                                    ]
                                    [ text "green cell" ]
                                ]
                            ]
                        ]
                    , cell
                        [ size Desktop 12
                        , size Tablet 8
                        , size Phone 4
                        ]
                        [ Card.view []
                            [ Card.text []
                                [ Options.styled div
                                    [ Typo.display1
                                    ]
                                    [ round model.timeLeft
                                        |> toString
                                        |> text
                                    ]
                                ]
                            , Card.text []
                                [ Options.styled div
                                    [ Typo.subhead
                                    ]
                                    [ text "Seconds left" ]
                                ]
                            ]
                        ]
                    , cell
                        [ size Desktop 12
                        , size Tablet 8
                        , size Phone 4
                        ]
                        [ Button.render Mdl
                            [ viewPlayingContext, 0 ]
                            model.mdl
                            [ Button.ripple
                            , Button.colored
                            , Button.onClick <| PlayMode Viewing
                            ]
                            [ text "Done Playing" ]
                        ]
                    ]
                ]
            ]


viewEditing : Model -> Html Msg
viewEditing model =
    let
        currentMaze =
            Zipper.current model.mazes

        isGenerating =
            model.mazeGenerate.status == MG.InProcess
    in
        grid
            [ Color.background <| Color.accentContrast
            ]
            [ cell
                -- Maze on the left.
                [ size Desktop 8
                , size Tablet 6
                , size Phone 4
                ]
                <| [ viewMaze model ]
            , cell
                -- Maze meta data on the right.
                [ size Desktop 4
                , size Tablet 2
                , size Phone 4
                ]
                [ grid
                    [ Color.background Color.accent
                    ]
                    [ cell
                        [ size Desktop 12
                        , size Tablet 8
                        , size Phone 4
                        ]
                        [ Options.styled p
                            [ Typo.display1
                            ]
                            [ text "Instructions" ]
                        ]
                    , cell
                        [ size Desktop 12
                        , size Tablet 8
                        , size Phone 4
                        ]
                        [ Options.styled p
                            [ Typo.subhead
                            , Color.text Color.accentContrast
                            ]
                            [ text "1. Choose the size of maze you want with the radio buttons." ]
                        ]
                    , cell
                        [ size Desktop 12
                        , size Tablet 8
                        , size Phone 4
                        ]
                        [ Options.styled p
                            [ Typo.subhead
                            , Color.text Color.accentContrast
                            ]
                            [ text "2. Click the Generate The Maze button to create your custom maze." ]
                        ]
                    , cell
                        [ size Desktop 12
                        , size Tablet 8
                        , size Phone 4
                        ]
                        [ Options.styled p
                            [ Typo.subhead
                            , Color.text Color.accentContrast
                            ]
                            [ text "4. Click the Done Editing button when you are satisfied." ]
                        ]
                    , cell
                        -- The size of the maze to generate.
                        [ size Desktop 12
                        , size Tablet 8
                        , size Phone 4
                        ]
                        [ Toggles.radio Mdl
                            [ viewEditingContext, 10 ]
                            model.mdl
                            [ Toggles.value <| currentMaze.mazeSize <= 10
                            , Toggles.group "mazeSizePending"
                            , Toggles.ripple
                            , Toggles.onClick <| MazeSizePending 10
                            , Options.css "padding-right" "10px"
                            ]
                            [ text "10 x 10" ]
                        , Toggles.radio Mdl
                            [ viewEditingContext, 11 ]
                            model.mdl
                            [ Toggles.value <| currentMaze.mazeSize > 10 && currentMaze.mazeSize <= 20
                            , Toggles.group "mazeSizePending"
                            , Toggles.ripple
                            , Toggles.onClick <| MazeSizePending 20
                            , Options.css "padding-right" "10px"
                            ]
                            [ text "20 x 20" ]
                        , Toggles.radio Mdl
                            [ viewEditingContext, 12 ]
                            model.mdl
                            [ Toggles.value <| currentMaze.mazeSize > 20
                            , Toggles.group "mazeSizePending"
                            , Toggles.ripple
                            , Toggles.onClick <| MazeSizePending 40
                            , Options.css "padding-right" "10px"
                            ]
                            [ text "40 x 40" ]
                        ]
                    , cell
                        -- Generate maze button and percent complete.
                        [ size Desktop 12
                        , size Tablet 8
                        , size Phone 4
                        ]
                        [ Button.render Mdl
                            [ viewEditingContext, 0 ]
                            model.mdl
                            [ Button.ripple
                            , Button.colored
                            , Button.onClick <| MazeGenerate (MG.BinaryTreeInit currentMaze.mazeSize currentMaze.id)
                            ]
                            [ text "Generate the maze" ]
                        , Chip.span []
                            [ Chip.content []
                                [ text <| (toString currentMaze.percComplete) ++ "%" ]
                            ]
                        ]
                    , cell
                        -- Done editing button.
                        [ size Desktop 12
                        , size Tablet 8
                        , size Phone 4
                        ]
                        [ Button.render Mdl
                            [ viewEditingContext, 1 ]
                            model.mdl
                            [ Button.ripple
                            , Button.colored
                            , Button.onClick <| PlayMode Viewing
                            , if isGenerating then
                                Button.disabled
                              else
                                Options.nop
                            ]
                            [ text "Done Editing" ]
                        ]
                    ]
                ]
            ]


viewViewing : Model -> Html Msg
viewViewing model =
    let
        ( mazesList, currMazeIdx ) =
            ( Zipper.toList model.mazes
            , Zipper.before model.mazes
                |> List.length
            )

        mazesListView =
            List.indexedMap (\idx maze -> mazeMetaInfo model.mdl idx maze (idx == currMazeIdx) model.mazeDifficulty) mazesList
    in
        grid
            [ Color.background <| Color.accentContrast
            ]
            [ cell
                [ size Desktop 6
                , size Tablet 8
                , size Phone 4
                ]
                [ grid []
                    [ cell
                        -- Instructions above the maze on the left.
                        [ size Desktop 12
                        , size Tablet 8
                        , size Phone 4
                        ]
                        [ Card.view []
                            [ Card.text
                                [ Color.text Color.primaryContrast
                                , Color.background Color.primary
                                , Options.css "font-size" "24px"
                                , Options.css "line-height" "1.1em"
                                ]
                                [ text """
                                Goal: Build your maze then use arrow keys to move blue circle to green square.
                                """
                                ]
                            , Card.text
                                [ Color.text Color.primaryContrast
                                , Color.background Color.primary
                                , Options.css "font-size" "16"
                                , Options.css "line-height" "1.1em"
                                ]
                                [ text """
                                Get as many points as you can. Larger mazes, quicker times,
                                and higher difficulty levels earn more points. Replaying the
                                same maze that you already won will earn less points the more
                                that you do it.
                                """
                                ]
                            ]
                        ]
                    , cell
                        -- Maze on the left.
                        [ size Desktop 12
                        , size Tablet 8
                        , size Phone 4
                        ]
                        [ viewMaze model ]
                    , cell
                        -- Play mode options on the left.
                        [ size Desktop 12
                        , size Tablet 8
                        , size Phone 4
                        ]
                        [ Card.view []
                            [ Card.title []
                                [ Card.head
                                    [ Color.text Color.accent
                                    , Color.background Color.accentContrast
                                    ]
                                    [ text "Choose your difficulty" ]
                                ]
                            , Card.text
                                [ Color.text Color.accent
                                , Color.background Color.accentContrast
                                ]
                                [ Toggles.radio Mdl
                                    [ viewViewingContext, 1 ]
                                    model.mdl
                                    [ Toggles.value <| model.mazeDifficulty == Easy
                                    , Toggles.group "PlayMode"
                                    , Toggles.ripple
                                    , Toggles.onClick <| MazeDifficulty Easy
                                    , Options.css "padding-right" "10px"
                                    ]
                                    [ text "Easy" ]
                                , Toggles.radio Mdl
                                    [ viewViewingContext, 2 ]
                                    model.mdl
                                    [ Toggles.value <| model.mazeDifficulty == Medium
                                    , Toggles.group "PlayMode"
                                    , Toggles.ripple
                                    , Toggles.onClick <| MazeDifficulty Medium
                                    , Options.css "padding-right" "10px"
                                    ]
                                    [ text "Medium" ]
                                , Toggles.radio Mdl
                                    [ viewViewingContext, 3 ]
                                    model.mdl
                                    [ Toggles.value <| model.mazeDifficulty == Hard
                                    , Toggles.group "PlayMode"
                                    , Toggles.ripple
                                    , Toggles.onClick <| MazeDifficulty Hard
                                    , Options.css "padding-right" "10px"
                                    ]
                                    [ text "Hard" ]
                                ]
                            , Card.text
                                [ Color.text Color.primary
                                , Color.background Color.primaryContrast
                                ]
                                [ Options.styled p
                                    [ Typo.display1 ]
                                    [ text <| "Games Won: " ++ (toString model.won) ]
                                , Options.styled p
                                    [ Typo.display1 ]
                                    [ text <| "Games Lost: " ++ (toString model.lost) ]
                                , Options.styled p
                                    [ Typo.display1 ]
                                    [ text <| "Total Points: " ++ (toString model.points) ]
                                ]
                            ]
                        ]
                    ]
                ]
            , cell
                -- List of Mazes on the right.
                [ size Desktop 6
                , size Tablet 8
                , size Phone 4
                ]
                [ grid
                    [ Color.background <| Color.accent
                    ]
                    -- Title of the list of mazes.
                    [ cell
                        [ size Desktop 12
                        , size Tablet 8
                        , size Phone 4
                        ]
                        [ Options.styled p
                            [ Typo.display1
                            ]
                            [ text "Instructions" ]
                        ]
                    , cell
                        [ size Desktop 12
                        , size Tablet 8
                        , size Phone 4
                        ]
                        [ Options.styled p
                            [ Typo.subhead
                            , Color.text Color.accentContrast
                            ]
                            [ text "1. You need to build your maze before you can use it. Click the Edit button now." ]
                        ]
                    , cell
                        [ size Desktop 12
                        , size Tablet 8
                        , size Phone 4
                        ]
                        [ Options.styled p
                            [ Typo.subhead
                            , Color.text Color.accentContrast
                            ]
                            [ text "After you have built your maze, choose your difficulty level on the left then click the Play button." ]
                        ]
                    , cell
                        [ size Desktop 12
                        , size Tablet 8
                        , size Phone 4
                        ]
                        [ Options.styled p
                            [ Typo.subhead
                            , Color.text Color.accentContrast
                            ]
                            [ text """
                            Optional: You can have more than one maze. Create another maze with the New Maze button.
                            Then edit the maze to build it. Finally play it.
                            """
                            ]
                        ]
                    , cell
                        -- List of mazes.
                        [ size Desktop 12
                        , size Tablet 8
                        , size Phone 4
                        ]
                        [ grid [] mazesListView ]
                    , cell
                        -- New maze button.
                        [ size Desktop 12
                        , size Tablet 8
                        , size Phone 4
                        ]
                        [ Button.render Mdl
                            [ viewViewingContext, 0 ]
                            model.mdl
                            [ Button.ripple
                            , Button.colored
                            , Button.onClick <| NewMaze
                            ]
                            [ text "New Maze" ]
                        ]
                    ]
                ]
            ]


mazeMetaInfo : Material.Model -> Int -> Maze -> Bool -> Difficulty -> Grid.Cell Msg
mazeMetaInfo mdl idx maze isCurrentMaze mazeDifficulty =
    let
        makeMsg val =
            (toString val) ++ " x " ++ (toString val) ++ " blocks"

        gws =
            makeMsg maze.mazeSize

        vps =
            -- TODO: fix hard-code.
            makeMsg 60

        isMazeReady =
            maze.percComplete == 100

        backgroundColor =
            if isCurrentMaze then
                Color.primaryContrast
            else
                Color.accentContrast

        textColor =
            if isCurrentMaze then
                Color.primary
            else
                Color.accent
    in
        cell
            [ size Desktop 12
            , size Tablet 8
            , size Phone 4
            ]
            [ Card.view
                [ Options.css "width" "100%"
                , Color.background backgroundColor
                , Color.text textColor
                , Card.border
                , Options.attribute <| Html.onClick <| GoToMaze idx
                ]
                [ Card.text [ Color.text textColor ]
                    [ text ("Size: " ++ gws) ]
                , Card.text []
                    [ Button.render Mdl
                        [ mazeMetaInfoContext + 400 + idx ]
                        mdl
                        [ Button.ripple
                        , Button.colored
                        , Button.onClick <| PlayMode Playing
                        , Options.disabled <| not isMazeReady
                        ]
                        [ text "Play" ]
                    , Button.render Mdl
                        [ mazeMetaInfoContext + 500 + idx ]
                        mdl
                        [ Button.ripple
                        , Button.colored
                        , Button.onClick <| PlayMode Editing
                        ]
                        [ text "Edit" ]
                    , Button.render Mdl
                        [ mazeMetaInfoContext + 600 + idx ]
                        mdl
                        [ Button.ripple
                        , Button.colored
                        , Options.css "float" "right"
                        , Button.onClick <| DeleteMaze idx
                        ]
                        [ text "Delete" ]
                    ]
                ]
            ]


headerSmall : String -> Model -> List (Html a)
headerSmall title model =
    let
        contents =
            [ Layout.row []
                [ Layout.title []
                    [ Options.styled p [ Typo.headline ] [ text title ]
                    ]
                ]
            ]
    in
        contents


viewMaze : Model -> Html Msg
viewMaze model =
    let
        maze =
            Zipper.current model.mazes

        winSize =
            min model.windowSize.width model.windowSize.height

        viewingWH =
            winSize
                // 2
                |> flip (-) (winSize // 20)

        editingWH =
            winSize
                // 4
                |> (*) 3
                |> flip (-) (winSize // 10)

        playingWH =
            winSize
                // 8
                |> (*) 7
                |> flip (-) (winSize // 20)

        ( widthHeight, blockSize ) =
            case model.mazeMode of
                Viewing ->
                    ( viewingWH
                    , viewingWH // maze.mazeSize
                    )

                Editing ->
                    ( editingWH
                    , editingWH // maze.mazeSize
                    )

                Playing ->
                    ( playingWH
                    , playingWH // maze.mazeSize
                    )

        ( viewportSize, x, y ) =
            ( case ( model.mazeMode, model.mazeDifficulty ) of
                ( Playing, Hard ) ->
                    round <| (toFloat maze.mazeSize) / 4

                ( Playing, Medium ) ->
                    round <| (toFloat maze.mazeSize) / 2

                ( Playing, Easy ) ->
                    maze.mazeSize

                _ ->
                    maze.mazeSize
            , fst maze.center
            , snd maze.center
            )

        displayWH =
            viewportSize * blockSize

        ( vbx, vby ) =
            if model.mazeMode /= Playing then
                -- Show the whole maze unless in playing mode.
                ( 0, 0 )
            else
                ( max 0 ((x * blockSize) - (displayWH // 2))
                , max 0 ((y * blockSize) - (displayWH // 2))
                )
    in
        S.svg
            [ S.width (intToPx widthHeight)
            , S.height (intToPx widthHeight)
            , S.viewBox <| (fourIntToString vbx vby displayWH displayWH)
            ]
            [ background model blockSize
            ]


background : Model -> Int -> Html Msg
background model blockSize =
    let
        currentMaze =
            Zipper.current model.mazes

        -- TODO: 10 is a hard-coded blockSize. Set according to available space.
        wh =
            currentMaze.mazeSize * blockSize

        doLine x1 x2 y1 y2 =
            [ S.line
                [ S.x1 (intToPx x1)
                , S.y1 (intToPx y1)
                , S.x2 (intToPx x2)
                , S.y2 (intToPx y2)
                , S.stroke "black"
                , S.strokeWidth "5px"
                ]
                []
            ]

        ( topx1, topy1, topx2, topy2 ) =
            ( 0, 0, wh, 0 )

        ( rightx1, righty1, rightx2, righty2 ) =
            ( wh, 0, wh, wh )

        ( bottomx1, bottomy1, bottomx2, bottomy2 ) =
            ( wh, wh, 0, wh )

        ( leftx1, lefty1, leftx2, lefty2 ) =
            ( 0, wh, 0, 0 )

        list =
            (drawCells currentMaze model.mazeMode blockSize)
                ++ (doLine topx1 topy1 topx2 topy2)
                ++ (doLine rightx1 righty1 rightx2 righty2)
                ++ (doLine bottomx1 bottomy1 bottomx2 bottomy2)
                ++ (doLine leftx1 lefty1 leftx2 lefty2)
    in
        S.g [] list


drawCells : Maze -> Mode -> Int -> List (S.Svg Msg)
drawCells maze mode blockSize =
    let
        ( cells, centerX, centerY, goalX, goalY ) =
            ( Matrix.toIndexedArray maze.cells
                |> Array.toList
            , fst maze.center
            , snd maze.center
            , fst maze.goal
            , snd maze.goal
            )
    in
        List.map
            (\c ->
                let
                    col =
                        fst c
                            |> fst

                    row =
                        fst c
                            |> snd

                    cell =
                        snd c
                in
                    drawCell col
                        row
                        cell.northLink
                        cell.eastLink
                        cell.southLink
                        cell.westLink
                        (centerX == col && centerY == row)
                        (goalX == col && goalY == row)
                        mode
                        blockSize
            )
            cells


drawCell : Int -> Int -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Mode -> Int -> S.Svg Msg
drawCell col row north east south west isCenter isGoal mode blockSize =
    let
        -- Translate for blockSize and 0 based Svg system.
        xs =
            col * blockSize

        ys =
            row * blockSize

        fillColor =
            "white"

        strokeColor =
            "black"

        goalSize =
            (toFloat blockSize) * 0.75 |> round

        goalOffset =
            (toFloat blockSize)
                |> flip (-) (toFloat goalSize)
                |> flip (/) 2
                |> round

        {- : Generate lines but only if needed. -}
        topLine =
            if north then
                []
            else
                [ S.line
                    [ S.x1 (intToPx xs)
                    , S.y1 (intToPx ys)
                    , S.x2 (intToPx (xs + blockSize))
                    , S.y2 (intToPx ys)
                    , S.stroke strokeColor
                    ]
                    []
                ]

        rightLine =
            if east then
                []
            else
                [ S.line
                    [ S.x1 (intToPx (xs + blockSize))
                    , S.y1 (intToPx ys)
                    , S.x2 (intToPx (xs + blockSize))
                    , S.y2 (intToPx (ys + blockSize))
                    , S.stroke strokeColor
                    ]
                    []
                ]

        bottomLine =
            if south then
                []
            else
                [ S.line
                    [ S.x1 (intToPx (xs + blockSize))
                    , S.y1 (intToPx (ys + blockSize))
                    , S.x2 (intToPx xs)
                    , S.y2 (intToPx (ys + blockSize))
                    , S.stroke strokeColor
                    ]
                    []
                ]

        leftLine =
            if west then
                []
            else
                [ S.line
                    [ S.x1 (intToPx xs)
                    , S.y1 (intToPx ys)
                    , S.x2 (intToPx xs)
                    , S.y2 (intToPx (ys + blockSize))
                    , S.stroke strokeColor
                    ]
                    []
                ]

        goal =
            if isGoal then
                [ S.rect
                    [ S.width <| intToPx goalSize
                    , S.height <| intToPx goalSize
                    , S.stroke fillColor
                    , S.fill "green"
                    , S.x (intToPx (goalOffset))
                    , S.y (intToPx (goalOffset))
                    ]
                    []
                ]
            else
                []

        center =
            if isCenter then
                [ S.circle
                    [ S.stroke fillColor
                    , S.fill "lightblue"
                    , S.cx <| intToPx (xs + (blockSize // 2))
                    , S.cy <| intToPx (ys + (blockSize // 2))
                    , S.r <| intToPx (blockSize // 2)
                    ]
                    []
                ]
            else
                []

        cellLines =
            center
                ++ goal
                ++ topLine
                ++ rightLine
                ++ bottomLine
                ++ leftLine
    in
        S.g []
            cellLines
