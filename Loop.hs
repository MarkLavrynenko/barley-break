module Loop where

import Prelude hiding (Either(..))
import Data.Char (isDigit)
import Game

-- main functions
-- start game
play :: IO ()
play = greetings >> setup >>= gameLoop

-- main game loop
gameLoop :: Game -> IO()
gameLoop game
	| isGameOver game	= showResults game >> setup >>= gameLoop
	| otherwise			= showGame game >> askForMove >>= reactOnMove game
	
setup :: IO Game
setup = putStrLn "Start new game?" >>
	putStrLn "Set difficalty level: " >>
	getLine >>= maybe setup shuffle . readInt

-- queries from user
-- quit game or start new game or make move
reactOnMove :: Game ->  Query -> IO ()
reactOnMove game query = case query of
	Quit		-> quit
	NewGame n	-> gameLoop =<< shuffle n
	Play m		-> gameLoop $ move m game
	
-- read move from console and validate it
askForMove :: IO Query
askForMove = showAsk >>
	getLine >>= maybe askAgain return . parseQuery
	where askAgain = wrongMove >> askForMove
	
-- interprete query string 
parseQuery :: String -> Maybe Query
parseQuery x = case x of
	"up"	-> Just $ Play Up
	"u"		-> Just $ Play Up
	"down"	-> Just $ Play Down
	"d"		-> Just $ Play Down
	"left"	-> Just $ Play Left
	"l"		-> Just $ Play Left
	"right"	-> Just $ Play Right
	"r"		-> Just $ Play Right
	"quit"	-> Just $ Quit
	"q"		-> Just $ Quit
	
	'n':'e':'w':' ':n	-> Just . NewGame =<< readInt n
	'n':' ':n			-> Just . NewGame =<< readInt n
	_					-> Nothing

-- read string and convert it to integer
readInt :: String -> Maybe Int
readInt n
	| all isDigit n = Just $ read n
	| otherwise 	= Nothing

-- answers to user
-- greet lovely user
greetings :: IO ()
greetings = putStrLn "Hello! This is barley-break game" >>
	showGame initGame >> remindMoves

-- show result when game is finished
showResults :: Game -> IO ()
showResults g = showGame g >> putStrLn "Game over"
	
showGame :: Game -> IO ()
showGame = putStrLn . show

wrongMove :: IO ()
wrongMove = putStrLn "Can't parse move" >> remindMoves

-- ask user to enter next move
showAsk :: IO ()
showAsk = putStrLn "Your move: "

-- list of available moves
remindMoves :: IO()
remindMoves = mapM_ putStrLn talk
	where talk = ["Your can move:",
					"   left or l       > move left",
					"   right or r      > move right",
					"   up or u         > move up",
					"   down or d       > mode doown",
					"Or do another actions:",
					"   new {integer}   > state new game, integer - difficulty",
					"   quit or q       > exit the game"
				]
-- finally quit the game
quit :: IO ()
quit = putStrLn "Bye bye lovely user, have a nice day" >> return ()