module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyDown, onKeyUp)
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Platform.Sub
import Random


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.batch [ onKeyDown (isFlagKey KeyDown) ]
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Playing (Array.repeat config.height (Array.repeat config.width initTile)) False
    , Cmd.batch (List.repeat config.mines layMine)
    )


initTile : Tile
initTile =
    Tile Hidden False


layMine : Cmd Msg
layMine =
    Random.generate LayMine
        (Random.pair (Random.int 0 (config.width - 1)) (Random.int 0 (config.height - 1)))


config =
    { width = 8
    , height = 8
    , mines = 10
    }


type alias Model =
    { status : GameStatus
    , board : Board
    , flagging : Bool
    }


type GameStatus
    = Playing
    | Lost
    | Won


type Msg
    = LayMine ( Int, Int )
    | ClickedTile ( Int, Int )
    | KeyDown Bool
    | NewGame


isFlagKey : (Bool -> Msg) -> Decode.Decoder Msg
isFlagKey msg =
    Decode.map (msg << (\k -> k == "F" || k == "f")) (Decode.field "key" Decode.string)



-- BOARD


type alias Board =
    Array (Array Tile)


type alias Tile =
    { status : TileStatus
    , mine : Bool
    }


type TileStatus
    = Hidden
    | Flagged
    | Revealed Int


neighborIndexes : Int -> Int -> List ( Int, Int )
neighborIndexes x y =
    [ ( x - 1, y - 1 )
    , ( x, y - 1 )
    , ( x + 1, y - 1 )
    , ( x - 1, y )
    , ( x + 1, y )
    , ( x - 1, y + 1 )
    , ( x, y + 1 )
    , ( x + 1, y + 1 )
    ]


getTile : Board -> Int -> Int -> Maybe Tile
getTile board x y =
    Array.get y board
        |> Maybe.andThen (\row -> Array.get x row)


updateTile : Board -> Int -> Int -> Tile -> Board
updateTile board x y tile =
    Array.get y board
        |> Maybe.withDefault (Array.repeat config.width initTile)
        |> (\row -> Array.set x tile row)
        |> (\updatedRow -> Array.set y updatedRow board)


getNeighbors : Board -> Int -> Int -> List Tile
getNeighbors board x y =
    neighborIndexes x y
        |> List.filterMap (\( x_, y_ ) -> getTile board x_ y_)


countNeighbors : Board -> Int -> Int -> Int
countNeighbors board x y =
    getNeighbors board x y
        |> List.filter (\tile -> tile.mine == True)
        |> List.length


revealNeighbors : Board -> Int -> Int -> Board
revealNeighbors board x y =
    neighborIndexes x y
        |> List.filterMap (\( x_, y_ ) -> getTile board x_ y_ |> Maybe.map (\tile -> ( x_, y_, tile )))
        |> List.filter (\( x_, y_, tile ) -> tile.status == Hidden)
        |> List.foldl
            (\( x_, y_, tile ) updatedBoard ->
                let
                    updatedTile =
                        { tile | status = Revealed (countNeighbors board x_ y_) }
                in
                case updatedTile.status of
                    Revealed 0 ->
                        updateTile updatedBoard x_ y_ updatedTile
                            |> (\b -> revealNeighbors b x_ y_)

                    _ ->
                        updateTile updatedBoard x_ y_ updatedTile
            )
            board



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LayMine ( x, y ) ->
            let
                tile =
                    getTile model.board x y
                        |> Maybe.withDefault initTile
            in
            if tile.mine then
                ( model, layMine )

            else
                let
                    board =
                        updateTile model.board x y { tile | mine = True }
                in
                ( { model | board = board }, Cmd.none )

        ClickedTile ( x, y ) ->
            let
                tile =
                    getTile model.board x y
                        |> Maybe.withDefault initTile

                updatedStatus =
                    case model.flagging of
                        True ->
                            case tile.status of
                                Hidden ->
                                    Flagged

                                Flagged ->
                                    Hidden

                                _ ->
                                    tile.status

                        False ->
                            case tile.status of
                                Hidden ->
                                    Revealed (countNeighbors model.board x y)

                                _ ->
                                    tile.status

                updatedBoard =
                    case updatedStatus of
                        Revealed 0 ->
                            updateTile (revealNeighbors model.board x y) x y { tile | status = updatedStatus }

                        _ ->
                            updateTile model.board x y { tile | status = updatedStatus }
            in
            ( { model | board = updatedBoard, status = updateGameStatus updatedBoard, flagging = False }, Cmd.none )

        KeyDown flagKey ->
            if flagKey then
                ( { model | flagging = not model.flagging }, Cmd.none )

            else
                ( model, Cmd.none )

        NewGame ->
            init ()


updateGameStatus : Board -> GameStatus
updateGameStatus board =
    let
        bombRevealed =
            board
                |> Array.foldr (\row acc -> Array.append acc row) Array.empty
                |> Array.filter .mine
                |> Array.filter
                    (\tile ->
                        case tile.status of
                            Revealed _ ->
                                True

                            _ ->
                                False
                    )
                |> (Array.isEmpty >> not)

        allRevealed =
            board
                |> Array.foldr (\row acc -> Array.append acc row) Array.empty
                |> Array.filter (.mine >> not)
                |> Array.filter
                    (\tile ->
                        case tile.status of
                            Hidden ->
                                True

                            _ ->
                                False
                    )
                |> Array.isEmpty
    in
    if bombRevealed then
        Lost

    else if allRevealed then
        Won

    else
        Playing



-- VIEW


view : Model -> Html Msg
view model =
    let
        board =
            viewBoard model.board
    in
    div []
        [ div [] [ text "Minesweeper" ]
        , div [] <|
            case model.status of
                Lost ->
                    [ text "You Lost!" ]

                Won ->
                    [ text "You Won!" ]

                _ ->
                    [ text "Playing" ]
        , button [ onClick NewGame ] [ text "New Game" ]
        , if model.flagging then
            div [ style "cursor" "help" ] [ board ]

          else
            div [] [ board ]
        , div [] [ text "Press f to flag a square" ]
        ]


viewBoard : Board -> Html Msg
viewBoard board =
    div [] <|
        List.indexedMap
            (\y row ->
                div [] <|
                    List.indexedMap
                        (\x tile ->
                            div
                                [ style "border" "solid 1px black"
                                , style "display" "inline-block"
                                , style "vertical-align" "bottom"
                                , style "width" "3rem"
                                , style "height" "3rem"
                                , style "font-size" "2rem"
                                , style "text-align" "center"
                                , onClick (ClickedTile ( x, y ))
                                ]
                            <|
                                case ( tile.status, tile.mine ) of
                                    ( Hidden, _ ) ->
                                        [ text " " ]

                                    ( Flagged, _ ) ->
                                        [ text "F" ]

                                    ( Revealed count, False ) ->
                                        [ text (String.fromInt count) ]

                                    ( Revealed count, True ) ->
                                        [ text "*" ]
                        )
                        (Array.toList row)
            )
            (Array.toList board)
