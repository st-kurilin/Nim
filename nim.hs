module Nim where 
import Char 
import List
import Maybe

--domain
type Board = [Int]	--number of objects in each heap
type Heap = Int		--Heap id
type Turn = (Int, Int)	--heap and number of objects to remove 

applyTurn :: Turn -> Board -> Board
applyTurn t b = map 
	(\ (i, v) -> if (i == fst t) then v - snd t else v)
	(zip [1..] b)

empty :: Board -> Bool
empty b = all (<=0) b

indexedHeaps :: Board -> [(Heap, Int)]
indexedHeaps b = zip [1..] b

availableHeaps :: Board -> [Heap]
availableHeaps b = map fst (filter (\ (_, h) -> h > 0) (indexedHeaps b))

availableObjectsByHeap :: Board -> Heap -> Int
availableObjectsByHeap b h = snd (head (
	filter (\ (i, _) -> i == h) (indexedHeaps b)))

--IO stuff

int2St :: Int -> String
int2St x =  [chr(ord('0') + x)]

promtInt :: String -> (Int -> Bool) -> IO Int
promtInt msg p = do 
	putStr (msg ++ "> ")
	c <- getChar
	ignored <- getLine
	let x = ((ord c) - ord('0'))
	if(p x) 
		then return x 
		else promtInt msg p

promtIntFromRange :: String -> (Int, Int) -> IO Int
promtIntFromRange msg (from, to) = promtInt newMsg p where 
	newMsg = msg ++ "[" ++ show from ++ ";" ++ show to ++"]" 
	p v = v >= from && v <= to

promtIntFromSet :: String -> [Int] -> IO Int
promtIntFromSet msg s = promtInt newMsg p where 
	newMsg = msg ++ show s
	p v = isJust (find (== v) s)

	
readturn :: Board -> IO(Turn)
readturn b = do 
	heap <- promtIntFromSet "heap" (availableHeaps b)
	objects <- promtIntFromRange "number" 
		(1, (availableObjectsByHeap b heap))
	return (heap, objects)

putAllStr :: [String] -> IO()
putAllStr [x] = do putStrLn x
putAllStr (x:xs) = do 
	putAllStr [x]
	putAllStr xs

showboard :: Board -> IO()
showboard b = do 
	putAllStr (map (\ (i, n) ->  objectsAtHeap n)  (zip [1..] b))
	where objectsAtHeap n =  concat(["*" | _ <- [1..n]])

--Game
iteration :: Board -> IO(Board)
iteration b = do 
	showboard b
	t <- readturn b
	return (applyTurn t b)

game :: IO(Board)-> IO(Board)
game b = do 
	board <- b
	if (empty board)
	then return [] 
	else game (iteration board)

nim :: IO() 
nim = do 	
	ignored <- game (return [1, 2, 3, 1])
	putStrLn "done"


