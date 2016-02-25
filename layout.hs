import System.IO

infixr 5 .+
infixr 5 ..+
infixr 5 .*
infixr 5 ..*
infixr 5 ./
infixr 5 ../

data Cell = O | X | Empty deriving (Show,Eq)
type Row a = [a]
type GameState = Row (Row Cell)
type Won = Cell
type GamePlay = (Cell,Won,GameState)

data Tree a = EmptyTree | Node a (Row (Tree a)) deriving (Eq, Show)
type GameTree = Tree GamePlay

(.+) :: Cell -> Cell -> Cell
Empty .+ ys = ys
ys .+ Empty = ys
x .+ y = x

inv :: Cell -> Cell
inv O = X
inv X = O

initiliazeGames :: [GameState]
initiliazeGames = [[[X,Empty,Empty],[Empty,Empty,Empty],[Empty,Empty,Empty]],[[Empty,X,Empty],[Empty,Empty,Empty],[Empty,Empty,Empty]],[[Empty,Empty,X],[Empty,Empty,Empty],[Empty,Empty,Empty]],[[Empty,Empty,Empty],[X,Empty,Empty],[Empty,Empty,Empty]],[[Empty,Empty,Empty],[Empty,X,Empty],[Empty,Empty,Empty]],[[Empty,Empty,Empty],[Empty,Empty,X],[Empty,Empty,Empty]],[[Empty,Empty,Empty],[Empty,Empty,Empty],[X,Empty,Empty]],[[Empty,Empty,Empty],[Empty,Empty,Empty],[Empty,X,Empty]],[[Empty,Empty,Empty],[Empty,Empty,Empty],[Empty,Empty,X]]]

remove :: GameState -> [GameState] -> [GameState]
remove game games = filter (\x -> (x /= game)) games

(.*) :: Cell -> Cell -> Cell
O .* X = O
X .* X = X
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
won game = foldl (\acc x -> let res = rowDone x in if(res/=Empty) then res else acc) Empty game

showGame :: GameState -> IO ()
showGame [] = do return ()
showGame (x:xs) = do
	putStrLn $ foldl (\acc x -> if(x == Empty) then acc++"  " else acc++" "++((show)x )) "" x
	showGame xs
