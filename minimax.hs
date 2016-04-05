import Layout

evaluateRow :: Row Cell -> Integer
evaluateRow row = (\(y,x) -> (10^x)-(10^y)) (foldl (\(acc1,acc2) x ->  if(x == O) then (acc1+1,acc2) else if (x==X) then (acc1,acc2+1) else (acc1,acc2)) (0,0) row)

evaluateState :: GameState -> Integer
evaluateState game = sum (map evaluateRow ((map (\x -> x game) [column1,column2,column3,diag1,diag2]) ++ game))

constructTree :: GamePlay -> Row GameState -> Integer -> GameTree
constructTree gamePlay _ 0 = Node gamePlay []
constructTree (move,O,game) _ _ = Node (move,O,game) []
constructTree (move,X,game) _ _ = Node (move,X,game) []
constructTree (move,winner,game) gamesLeft n = (Node (move,winner,game)) ((map (\(x,y) -> constructTree (inv move,won y,y) (filter (\z -> z/=x) gamesLeft) (n-1))  (zip (gamesLeft) (nextMoves (move,winner,game) gamesLeft))))

minimize :: GameTree -> (Integer,GameTree)
minimize (Node (a,b,game) []) = (evaluateState game,Node (a,b,game) [])
minimize (Node (_,_,game) treeList) = foldl1 (\(ax,ay) (x,y) -> if(x > ax) then (x,y) else (ax,ay)) $ map (\x -> ((first $ maximize x),x)) (treeList)

first :: (a,b) -> a
first (a,b) = a

maximize :: GameTree -> (Integer,GameTree)
maximize (Node (a,b,game) []) = (evaluateState game,Node (a,b,game) [])
maximize (Node (_,_,game) treeList) = foldl1 (\(ax,ay) (x,y) -> if(x > ax) then (x,y) else (ax,ay)) $ map (\x -> ((first $ minimize x),x)) (treeList)

minimax :: GamePlay -> Row GameState -> GameState
minimax (c,b,gamePlay) gamesLeft = let 
	tree = (constructTree (c,b,gamePlay) gamesLeft 5)
	(_,Node (_,_,a) treeList) = maximize tree
	in (a ...++ gamePlay)

main = do
	putStrLn "rolling on the high roads!"
