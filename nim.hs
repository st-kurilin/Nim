module Nim where 
import Char 
import List
import Maybe

--Domain
--
type Board = [Int]	--number of objects in each heap
type Heap = Int		--Heap id
type Turn = (Int, Int)	--heap and number of objects to remove 

applyTurn :: Turn -> Board -> Board
--Build new board according to old one and turn.
applyTurn t b = map 
	(\ (i, v) -> if (i == fst t) then v - snd t else v)
	(zip [1..] b)

empty :: Board -> Bool
--Check if board is empty. When it is, game is over.
empty b = all (<=0) b

indexedHeaps :: Board -> [(Heap, Int)]
--Returns tupples of (heap index, number of object in the heap)
indexedHeaps b = zip [1..] b

availableHeaps :: Board -> [Heap]
--Returns heaps that contains one or more objects.
availableHeaps b = map fst (filter (\ (_, h) -> h > 0) (indexedHeaps b))

availableObjectsByHeap :: Board -> Heap -> Int
--Return number of objects in the heap.
availableObjectsByHeap b h = snd (head (
	filter (\ (i, _) -> i == h) (indexedHeaps b)))

--IO Utils
--
promtInt :: String -> (Int -> Bool) -> IO Int
--Read Int from console. There could be validation using predicate.
promtInt msg p = do 
	putStr (msg ++ "> ")
	c <- getChar
	ignored <- getLine
	let x = ((ord c) - ord('0'))
	if(p x) 
		then return x 
		else promtInt msg p

promtIntFromRange :: String -> (Int, Int) -> IO Int
--Read Int from console. Int should be in range.
promtIntFromRange msg (from, to) = promtInt newMsg p where 
	newMsg = msg ++ "[" ++ show from ++ ";" ++ show to ++"]" 
	p v = v >= from && v <= to

promtIntFromSet :: String -> [Int] -> IO Int
--Read Int from console. Int should be in set.
promtIntFromSet msg s = promtInt newMsg p where 
	newMsg = msg ++ show s
	p v = isJust (find (== v) s)

putAllStr :: [String] -> IO()
--Print each string from new line.
putAllStr [x] = do putStrLn x
putAllStr (x:xs) = do 
	putAllStr [x]
	putAllStr xs

--Game specific IO
--
readturn :: Board -> IO(Turn)
--Dialog for inputing turn data.
readturn b = do 
	heap <- promtIntFromSet "heap" (availableHeaps b)
	objects <- promtIntFromRange "number" 
		(1, (availableObjectsByHeap b heap))
	return (heap, objects)


showboard :: Board -> IO()
--Displays board in user friendly interface.
showboard b = do 
	putAllStr (map stringify (indexedHeaps b))  where
		objectsAtHeap n =  concat(replicate n "*")
		heapIndex  i = "[" ++ show i ++ "]"
		stringify (i, n) =  heapIndex i ++ objectsAtHeap n

--Game
--
play :: IO(Board)-> IO(Board)
--Actually game.
play b = do 
	board <- b
	if (empty board)
	then return [] 
	else do 
		showboard board
		t <- readturn board
		play (return (applyTurn t board))

nim :: IO() 
--Runner function.
nim = do 	
	ignored <- play (return [1, 2, 3, 1])
	putStrLn "done"


