data Cell = O | X | Empty deriving (Show,Eq)
type Row a = [a]
type GameState = Row (Row Cell)

initiliazeEmpty :: GameState
initiliazeEmpty = [initiliazeERow,initiliazeERow,initiliazeERow]

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
column1 game = foldr (\(x:y:z:[]) acc  -> x:acc) (initiliazeRow) game
column2 :: GameState -> Row Cell
column2 game = foldr (\(x:y:z:[]) acc -> y:acc) (initiliazeRow) game
column3 :: GameState -> Row Cell
column3 game = foldr (\(x:y:z:[]) acc -> z:acc) (initiliazeRow) game
diag1 :: GameState -> Row Cell
diag1 ((x:_):(_:y:_):(_:_:z:[]):[]) = x:y:z:(initiliazeRow)
diag2 :: GameState -> Row Cell
diag2 ((_:_:x:_):(_:y:_):(z:_):_) = x:y:z:(initiliazeRow)

won :: GameState -> Cell
won game = foldl (\acc x -> let res = rowDone x in if(res/=Empty) then res else acc) Empty game