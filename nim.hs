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
availableHeaps b = map fst . filter ((> 0) . snd) $ indexedHeaps b

--Return number of objects in the heap.
availableObjectsByHeap :: Board -> Heap -> Integer
availableObjectsByHeap board heapId = board !! (fromInteger heapId - 1)

--String representation of board
showHeaps :: Board -> [String]
showHeaps board = map showIdxHeap (indexedHeaps board)
	where	showIdxHeap (heapId, n) = heapIndex heapId++ objects n 
		heapIndex heapId = concat ["[", show heapId, "]"]
		objects n = genericReplicate n '*'

--IO Utils
--
--Try to read from string. 
--On Succes return just some result. On failure - return nothing.
maybeRead :: (Read a) => String -> Maybe a
maybeRead str = listToMaybe [x | (x, "") <- reads str]

--Try to read from input line.
maybeReadLn :: (Read a) => IO (Maybe a)
maybeReadLn = fmap maybeRead getLine

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
promptIntFromSet msg s = promptInt (msg ++ show s) (`elem` s) 

--Print each string from new line.
putAllStr :: [String] -> IO()
putAllStr xs = mapM_ putStrLn xs

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
printBoard :: Board -> IO()
printBoard board = putAllStr $ showHeaps board


--Game
--
--Actually game.
play :: Board -> IO(Board)
play board	| empty board	= return [] 
		| otherwise	= do	printBoard board
					t <- readTurn board
					play $ applyTurn t board

--Runner function.
nim :: IO() 
nim = do
	play [1, 2, 3, 1]
	return ()


