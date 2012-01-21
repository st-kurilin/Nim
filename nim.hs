module Nim where 
import Data.Char 
import Data.List
import Data.Maybe

--Domain 
--Nim is a mathematical game of strategy 
--in which two players take turns removing objects from distinct heaps. 
--On each turn, a player must remove at least one object, and may remove 
--any number of objects provided they all come from the same heap.
--Read more at http://en.wikipedia.org/wiki/Nim
--
type Board = [Integer]		--number of objects in each heap
type Heap = Integer		--Heap id
type Turn = (Integer, Integer)	--heap and number of objects to remove 

--Build new board according to old one and turn.
applyTurn :: Turn -> Board -> Board
applyTurn (heapId, removed) board = zipWith decHeap [1..] board
	where decHeap idx n	| idx == heapId	= n - removed 
				| otherwise 	= n

--Check if board is empty. When it is, game is over.
empty :: Board -> Bool
empty b = all (<= 0) b

--Returns tupples of (heap index, number of object in the heap).
indexedHeaps :: Board -> [(Heap, Integer)]
indexedHeaps b = zip [1..] b

--Returns heaps that contains one or more objects.
availableHeaps :: Board -> [Heap]
availableHeaps b = map fst (filter (\ (_, h) -> h > 0) (indexedHeaps b))

--Return number of objects in the heap.
availableObjectsByHeap :: Board -> Heap -> Integer
availableObjectsByHeap board heapId = board !! (fromInteger heapId - 1)

--IO Utils
--
maybeRead :: (Read a) => String -> Maybe a
maybeRead str = listToMaybe [x | (x, "") <- reads str]

maybeReadLn :: (Read a) => IO (Maybe a)
maybeReadLn = fmap maybeRead readLn

--Read Int from console. There could be validation using predicate.
promptInt :: String -> (Integer -> Bool) -> IO Integer
promptInt msg p = do 
	putStr (msg ++ "> ")
	inp <- maybeReadLn
	case inp of 
		Just x | p x	-> return x
		_		-> promptInt msg p

--Read Int from console. Int should be in range.
promptIntFromRange :: String -> (Integer, Integer) -> IO Integer
promptIntFromRange msg (from, to) = promptInt newMsg inRange
	where	newMsg = concat [msg, "[", show from, ";", show to,"]"]
		inRange v = v >= from && v <= to

--Read Int from console. Int should be in set.
promptIntFromSet :: String -> [Integer] -> IO Integer
promptIntFromSet msg s = promptInt newMsg p where 
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
	heap <- promptIntFromSet "heap" (availableHeaps b)
	objects <- promptIntFromRange "number" 
		(1, (availableObjectsByHeap b heap))
	return (heap, objects)

--Displays board in user friendly interface.
showBoard :: Board -> IO()
showBoard b = do 
	putAllStr (map stringify (indexedHeaps b)) 
	where	objectsAtHeap n =  concat(replicate (fromIntegral n) "*")
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


