import Layout
import System.Process

evaluateRow :: Row Cell -> Integer
evaluateRow row = (\(y,x) -> (10^x)-(10^y)) (foldl (\(acc1,acc2) x ->  if(x == O) then (acc1+1,acc2) else if (x==X) then (acc1,acc2+1) else (acc1,acc2)) (0,0) row)

evaluateState :: GameState -> Integer
evaluateState game = 
	let winner = won game
	in
	if ((winner) == Empty) then sum (map evaluateRow ((map (\x -> x game) [column1,column2,column3,diag1,diag2]) ++ game)) 
	else if (winner == X) then 10000000 
	else -10000000

constructTree :: GamePlay -> Row GameState -> Integer -> GameTree
constructTree gamePlay _ 0 = Node gamePlay []
constructTree (move,O,game) _ _ = Node (move,O,game) []
constructTree (move,X,game) _ _ = Node (move,X,game) []
constructTree (move,winner,game) gamesLeft n = (Node (move,winner,game)) ((map (\(x,y) -> constructTree (inv move,won y,y) (filter (\z -> z/=x) gamesLeft) (n-1))  (zip (gamesLeft) (nextMoves (move,winner,game) gamesLeft))))

minOf :: [Integer] -> Integer
minOf [] = 1000000
minOf (x:xs) = let y = minOf xs in if (x < y) then x else y

maxOf :: [Integer] -> Integer
maxOf [] = -1000000
maxOf (x:xs) = let y = maxOf xs in if (x > y) then x else y

minimize :: GameTree -> (Integer,GameTree)
minimize (Node (a,b,game) []) = (evaluateState game,Node (a,b,game) [])
--minimize (Node (_,_,game) treeList) = foldl1 (\(ax,ay) (x,y) -> if(x > Integerx) then (x,y) else (ax,ay)) $ map (\x -> ((first $ maximize x),x)) (treeList)
minimize (Node (yo,yoo,game) treeList) = (minOf $ map (first . maximize) treeList,(Node (yo,yoo,game) treeList))

first :: (a,b) -> a
first (a,b) = a

maximize :: GameTree -> (Integer,GameTree)
maximize (Node (a,b,game) []) = (evaluateState game,Node (a,b,game) [])
--maximize (Node (_,_,game) treeList) = foldl1 (\(ax,ay) (x,y) -> if(x > Integerx) then (x,y) else (ax,ay)) $ map (\x -> ((first $ minimize x),x)) (treeList)
maximize (Node (yo,yoo,game) treeList) = (maxOf $ map (first . minimize) treeList,(Node (yo,yoo,game) treeList))

process (max,Node (yo,yoo,a) treeList) = if ((length treeList) == 0) then a else let (Node (_,_,df) treeLst):_ = filter (\x -> (max == ((first . minimize) x))) treeList in df

minimax :: GamePlay -> Row GameState -> IO (GameState)
minimax (c,b,gamePlay) gamesLeft = let 
	tree = (constructTree (c,b,gamePlay) gamesLeft 10)
	(max,Node (yo,yoo,a) treeList) = maximize tree
	in do
	putStrLn $ (show) max
	return ((process (max,Node (yo,yoo,a) treeList)) ...++ gamePlay)

playGames :: GamePlay -> Row GameState -> IO ()
playGames (move,winner,game) gamesLeft = do
	callCommand "clear"
	showGame (game ..+ (numberedState gamesLeft))
	if ((winner == Empty) && ((length gamesLeft) /= 0)) then do
		if (move == O) then do
			num <- getLine
			playGames ((inv move),(won (game ..+ (thisMove gamesLeft num move))),(game ..+ (thisMove gamesLeft num move))) (filter (\x -> x /= (move ../ (thisMove gamesLeft num move))) gamesLeft)
			return ()
		else do
			tmove <- minimax (move,winner,game) gamesLeft
			playGames ((inv move),(won (tmove ..+ game)),(tmove ..+ game)) (remove tmove gamesLeft)
	else if ((length gamesLeft) == 0) then
		if (winner /= Empty) then do
			putStrLn $ (show)winner ++ " has Won the game!"
		else do
			putStrLn $ "Draw"	
	else
		putStrLn $ (show)winner ++ " has Won the game!"

main = do
	playGames (O,Empty,[[Empty,Empty,Empty],[Empty,Empty,Empty],[Empty,Empty,Empty]]) initiliazeGames