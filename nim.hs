module Nim where 
import Char 
import List
import Maybe

--Domain 
--Nim is a mathematical game of strategy 
--in which two players take turns removing objects from distinct heaps. 
--On each turn, a player must remove at least one object, and may remove 
--any number of objects provided they all come from the same heap.
--Read more at http://en.wikipedia.org/wiki/Nim
--
type Board = [Int]	--number of objects in each heap
type Heap = Int		--Heap id
type Turn = (Int, Int)	--heap and number of objects to remove 

--Build new board according to old one and turn.
applyTurn :: Turn -> Board -> Board
applyTurn t b = map 
	(\ (i, v) -> if (i == fst t) then v - snd t else v)
	(zip [1..] b)

--Check if board is empty. When it is, game is over.
empty :: Board -> Bool
empty b = all (<= 0) b

--Returns tupples of (heap index, number of object in the heap).
indexedHeaps :: Board -> [(Heap, Int)]
indexedHeaps b = zip [1..] b

--Returns heaps that contains one or more objects.
availableHeaps :: Board -> [Heap]
availableHeaps b = map fst (filter (\ (_, h) -> h > 0) (indexedHeaps b))

--Return number of objects in the heap.
availableObjectsByHeap :: Board -> Heap -> Int
availableObjectsByHeap b h = snd (head (
	filter (\ (i, _) -> i == h) (indexedHeaps b)))

--IO Utils
--
--Read Int from console. There could be validation using predicate.
promtInt :: String -> (Int -> Bool) -> IO Int
promtInt msg p = do 
	putStr (msg ++ "> ")
	c <- getChar
	ignored <- getLine
	let x = ((ord c) - ord('0'))
	if(p x) 
		then return x 
		else promtInt msg p

--Read Int from console. Int should be in range.
promtIntFromRange :: String -> (Int, Int) -> IO Int
promtIntFromRange msg (from, to) = promtInt newMsg p where 
	newMsg = msg ++ "[" ++ show from ++ ";" ++ show to ++"]" 
	p v = v >= from && v <= to

--Read Int from console. Int should be in set.
promtIntFromSet :: String -> [Int] -> IO Int
promtIntFromSet msg s = promtInt newMsg p where 
	newMsg = msg ++ show s
	p v = isJust (find (== v) s)

--Print each string from new line.
putAllStr :: [String] -> IO()
putAllStr [x] = do putStrLn x
putAllStr (x:xs) = do 
	putAllStr [x]
	putAllStr xs

--Game specific IO
--
--Dialog for inputing turn data.
readTurn :: Board -> IO(Turn)
readTurn b = do 
	heap <- promtIntFromSet "heap" (availableHeaps b)
	objects <- promtIntFromRange "number" 
		(1, (availableObjectsByHeap b heap))
	return (heap, objects)

--Displays board in user friendly interface.
showBoard :: Board -> IO()
showBoard b = do 
	putAllStr (map stringify (indexedHeaps b)) where
		objectsAtHeap n =  concat(replicate n "*")
		heapIndex  i = "[" ++ show i ++ "]"
		stringify (i, n) =  heapIndex i ++ objectsAtHeap n

--Game
--
--Actually game.
play :: IO(Board)-> IO(Board)
play b = do 
	board <- b
	if (empty board)
	then return [] 
	else do 
		showBoard board
		t <- readTurn board
		play (return (applyTurn t board))

--Runner function.
nim :: IO() 
nim = do 	
	ignored <- play (return [1, 2, 3, 1])
	putStrLn "done"


