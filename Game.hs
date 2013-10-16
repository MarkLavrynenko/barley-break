module Game where

import Prelude hiding (Either(..))
import Data.Array
import System.Random
import Control.Monad
import Control.Applicative

type Pos 	= (Int, Int)
type Label	= Int
newtype Vec = Vec (Int, Int)
type Board = Array Pos Label
data Move = Up | Down | Left | Right
data Query = Quit | NewGame Int | Play Move
-- core data type to represent game board
data Game = Game {
		emptyField	:: 	Pos,
		gameBoard	:: Board } deriving Eq

-- called at start, return initial board
initGame :: Game
initGame = Game (3, 3) $ listArray ((0, 0), (3,3)) $ [0..15]
		
instance Show Game where
	show (Game _ board) = "\n" ++ space ++ line ++
		(foldr (\a b -> a ++ space ++ line ++ b) "\n" $ map column [0 .. 3]) where
				post id = showLabel $ board ! id
				showLabel n = cell $ show $ case n of
						15	-> 0
						n	-> n+1
				cell "0" = "    "
				cell [x] = ' ':' ': x :' ':[]
				cell [a,b] = ' ': a : b :' ':[]
				line = "+----+----+----+----+\n"
				nums = ((space ++ "|") ++ ) . foldr (\a b -> a ++ "|" ++ b) "\n".
					map post
				column i = nums $ map (\x -> (i, x)) [0 .. 3]
				space = "\t"

-- return Board updated to new state according to the Move
move :: Move -> Game -> Game
move m (Game id board)
	| within id' = Game id' $ board // updates
	| otherwise  = Game id board where
				id' = shift (orient m) id
				updates = [(id, board ! id'), (id', emptyLabel)]

-- check whether cell is inside the board
within :: Pos -> Bool
within (a, b) = p a && p b where
				p x = x >= 0 && x <= 3	

-- move cell along Vec orientation
shift :: Vec -> Pos -> Pos
shift (Vec (va, vb)) (pa, pb) = (va + pa, pb + vb)

-- Convert from orientation to vector
orient :: Move -> Vec
orient m = Vec $ case m of 
	Up		-> (-1, 0)
	Down	-> (1 , 0)
	Left	-> (0 ,-1)
	Right	-> (0 , 1)

-- label of the unique empty cell
emptyLabel :: Label
emptyLabel = 15

-- compare current board with initial
isGameOver :: Game -> Bool
isGameOver = (== initGame)

-- generate random board to play
shuffle :: Int -> IO Game
shuffle n = (iterate (shufflel =<<) $ pure initGame) !! n

-- make random move to generate random board
shufflel :: Game -> IO Game
shufflel g = flip move g <$> (randomElem $ nextMoves g)

-- select random item from the list
randomElem :: [a] -> IO a
randomElem xs = (xs !!) <$> randomRIO (0, length xs - 1)

-- list of available moves
nextMoves :: Game -> [Move]
nextMoves g = filter (within . moveEmptyTo . orient) allMoves where
			moveEmptyTo v = shift v (emptyField g)
			allMoves = [Up, Down, Left, Right]