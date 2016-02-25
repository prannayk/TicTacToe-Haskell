import System.IO
import System.Process

infixr 5 .+
infixr 5 ..+
infixr 5 .*
infixr 5 ..*
infixr 5 ./
infixr 5 ../

data Cell = O | X | Empty | Q | W | E | A | S | D | Z | C | V deriving (Show,Eq)
type Row a = [a]
type GameState = Row (Row Cell)
type GamePlay = (Cell,Cell,GameState)

data Tree a = EmptyTree | Node a (Row (Tree a)) deriving (Eq, Show)
type GameTree = Tree GamePlay

(.+) :: Cell -> Cell -> Cell
Empty .+ ys = ys
ys .+ Empty = ys
X .+ _ = X
O .+ _ = O

inv :: Cell -> Cell
inv O = X
inv X = O

initiliazeGames :: [GameState]
initiliazeGames = [[[X,Empty,Empty],[Empty,Empty,Empty],[Empty,Empty,Empty]],[[Empty,X,Empty],[Empty,Empty,Empty],[Empty,Empty,Empty]],[[Empty,Empty,X],[Empty,Empty,Empty],[Empty,Empty,Empty]],[[Empty,Empty,Empty],[X,Empty,Empty],[Empty,Empty,Empty]],[[Empty,Empty,Empty],[Empty,X,Empty],[Empty,Empty,Empty]],[[Empty,Empty,Empty],[Empty,Empty,X],[Empty,Empty,Empty]],[[Empty,Empty,Empty],[Empty,Empty,Empty],[X,Empty,Empty]],[[Empty,Empty,Empty],[Empty,Empty,Empty],[Empty,X,Empty]],[[Empty,Empty,Empty],[Empty,Empty,Empty],[Empty,Empty,X]]]

remove :: GameState -> [GameState] -> [GameState]
remove game games = filter (\x -> (x /= game)) games

(.*) :: Cell -> Cell -> Cell
Empty .* X = Empty
any .* X = any
_ .* Empty = Empty

(./) :: Cell -> Cell -> Cell
O ./ O = X
X ./ X = X
Empty ./ _ = Empty

(..+) :: GameState -> GameState -> GameState
game ..+ move = zipWith (\x y -> (zipWith (.+) x y)) game move

(..*) :: Cell -> GameState -> GameState
cell ..* game = map (map (cell .* )) game

(../) :: Cell -> GameState -> GameState
cell ../ game = map (map ( ./ cell)) game

initializeEmpty :: GameState
initializeEmpty = [initiliazeERow,initiliazeERow,initiliazeERow]

initiliazeERow :: Row Cell
initiliazeERow = [Empty,Empty,Empty]

initiliazeRow :: Row Cell 
initiliazeRow = []

rowDone :: Row Cell -> Cell
rowDone row 
	| [O,O,O] == row = O
	| [X,X,X] == row = X
	| otherwise = Empty

column1 :: GameState -> Row Cell
column1 ((x:_):(y:_):(z:_):_) = x:y:z:(initiliazeRow)
column2 :: GameState -> Row Cell
column2 ((_:x:_):(_:y:_):(_:z:_):_) = x:y:z:(initiliazeRow)
column3 :: GameState -> Row Cell
column3 ((_:_:x:_):(_:_:y:_):(_:_:z:_):_) = x:y:z:(initiliazeRow)
diag1 :: GameState -> Row Cell
diag1 ((x:_):(_:y:_):(_:_:z:[]):[]) = x:y:z:(initiliazeRow)
diag2 :: GameState -> Row Cell
diag2 ((_:_:x:_):(_:y:_):(z:_):_) = x:y:z:(initiliazeRow)

won :: GameState -> Cell
won game = foldl (\acc x -> let res = rowDone x in if(res/=Empty) then res else acc) Empty ((map (\x -> x game) [column1,column2,column3,diag1,diag2]) ++ game)

numberedState :: Row GameState -> GameState
numberedState gamesLeft = foldl (\acc x -> x ..+ acc) (initializeEmpty) (zipWith (..*) [Q,W,E,A,S,D,Z,C,V] gamesLeft)

showGame :: GameState -> IO ()
showGame [] = do return ()
showGame (x:xs) = do
	putStrLn $ foldl (\acc x -> acc++" "++(show)x) "" x
	showGame xs
	return ()

anal :: String -> Int
anal input
 | input == "Q" = 1
 | input == "W" = 2
 | input == "E" = 3
 | input == "A" = 4
 | input == "S" = 5
 | input == "D" = 6
 | input == "Z" = 7
 | input == "C" = 8
 | input == "V" = 9

thisMove :: Row GameState -> String -> Cell -> GameState
thisMove gamesLeft num move = (last $ take (anal num) $ map (move ..* ) gamesLeft)

playGame :: GamePlay -> Row GameState -> IO ()
playGame (move,winner,game) gamesLeft = do
	callCommand "clear"
	showGame (game ..+ (numberedState gamesLeft))
	if (winner == Empty) then do
		num <- getLine
		playGame ((inv move),(won (game ..+ (thisMove gamesLeft num move))),(game ..+ (thisMove gamesLeft num move))) (filter (\x -> x /= (thisMove gamesLeft num move)) gamesLeft)
		return ()
	else 
		putStrLn $ (show)winner ++ " has Won the game!"

main :: IO ()
main = do
	playGame (O,Empty,(initializeEmpty)) initiliazeGames
	return ()
