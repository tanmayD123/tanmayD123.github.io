import Html exposing (..)
import Html as Html
import Html.Events exposing (..)
import Keyboard
import Time exposing (..)
import Element exposing (..)
import Collage exposing (..)
import Color exposing (..)
import Text
import Char
import Random exposing (..)

segemD = 11
circleRad = 10
(width, height) = (900, 900)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type Direction
  = Up
  | Down
  | Left
  | Right

type alias Position = (Float, Float)

pos : Float -> Float -> Position
pos = (,)

type alias Snake =
  { head: Position
  , tail: List Position
  , direction: Direction }

type alias Cherry = Maybe Position

type alias Score = Int

type Model
  = NotStarted
  | Started Snake Cherry Score

type Msg
  = Tick Time
  | KeyPress Keyboard.KeyCode
  | Spawn (Float, Position)

randPos = Random.pair (Random.float 0 1) (Random.float 0 1)

generator: Random.Generator (Float, Position)
generator = Random.pair (Random.float 0 1) randPos

iSnake : Snake
iSnake =
  let head = (0, 0)
      tail = [1 .. 8] |> List.map (\n -> pos (-n*segemD) 0)
  in { head=head, tail=tail, direction=Right }

init : (Model, Cmd Msg)
init = (NotStarted, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  case model of
    NotStarted ->
      Keyboard.presses KeyPress

    Started _ _ _ ->
      Sub.batch

        [ Keyboard.presses KeyPress
        , Time.every (Time.inMilliseconds 50) Tick
        ]

view : Model -> Html Msg
view model =
  let bg = rect (toFloat width) (toFloat height) |> filled black
      content =
        case model of
          NotStarted ->
            [txt "press SPACE to start"]

          Started snake cherry score ->
            let head = rect segemD segemD |> filled white |> move snake.head
                tail =
                  snake.tail
                  |> List.map (\pos ->
                    rect segemD segemD |> filled white |> move pos)
                scoreLbl = txt (toString score)
            in case cherry of
                Nothing -> scoreLbl::head::tail
                Just pos ->
                  (circle circleRad |> filled red |> move pos)::scoreLbl::head::tail
  in collage width height (bg::content)
     |> Element.toHtml

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case model of
    NotStarted ->
      case msg of
        KeyPress 32 ->
          (Started iSnake Nothing 0, Cmd.none)

        _ ->
          (model, Cmd.none)

    Started snake cherry score ->
      case msg of
        KeyPress keyCode ->
          let newDir = newDirec keyCode snake.direction
              newSnake = { snake | direction=newDir }
          in (Started newSnake cherry score, Cmd.none)

        Spawn (chance, (randX, randY)) ->
          if chance <= 0.2 then
            let newCherry = spawnCherry randX randY
            in (Started snake newCherry score, Cmd.none)
          else
            (model, Cmd.none)

        Tick _ ->
          let newHead = newSegm1 snake.head snake.direction
              ateCherry =
                case cherry of
                  Just pos -> isOverlap newHead pos
                  Nothing -> False
              newTail =
                if ateCherry then
                  snake.head::snake.tail
                else
                  snake.head::(List.take (List.length snake.tail-1) snake.tail)
              newSnake = { snake | head=newHead, tail=newTail }
              (newCherry, newScore) =
                if ateCherry then
                  (Nothing, score+1)
                else
                  (cherry, score)
              gameOver = gameOvrr newHead newTail
          in if gameOver then
              (NotStarted, Cmd.none)
             else if newCherry == Nothing then
              (Started newSnake newCherry newScore, Random.generate Spawn generator)
             else
              (Started newSnake newCherry newScore, Cmd.none)

txt : String -> Form
txt msg =
  msg
  |> Text.fromString
  |> Text.color red
  |> Text.monospace
  |> Element.centered
  |> Collage.toForm

newDirec : Char.KeyCode -> Direction -> Direction
newDirec keyCode currentDir =
  let (changeableDirs, newDir) =
    case Char.fromCode keyCode of
      'a' -> ([ Up, Down ], Left)
      'w' -> ([ Left, Right ], Up)
      'd' -> ([ Up, Down ], Right)
      's' -> ([ Left, Right ], Down)
      _  -> ([], currentDir)
  in if List.any ((==) currentDir) changeableDirs then newDir else currentDir

newSegm1 : Position -> Direction -> Position
newSegm1 (x, y) direction =
  case direction of
    Up    -> pos x (y+segemD)
    Down  -> pos x (y-segemD)
    Left  -> pos (x-segemD) y
    Right -> pos (x+segemD) y

gameOvrr : Position -> List Position -> Bool
gameOvrr newHead newTail =
  List.any ((==) newHead) newTail
  || fst newHead > (width / 2)
  || snd newHead > (height / 2)
  || fst newHead < (-width / 2)
  || snd newHead < (-height / 2)

spawnCherry : Float -> Float -> Cherry
spawnCherry randW randH =
  let x = randW * width - width / 2
      y = randH * height - height / 2
  in pos x y |> Just

isOverlap : Position -> Position -> Bool
isOverlap (snakeX, snakeY) (cherryX, cherryY) =
  let (xd, yd) = (cherryX - snakeX, cherryY - snakeY)
      distance = sqrt(xd * xd + yd * yd)
  in distance <= (circleRad * 2)
