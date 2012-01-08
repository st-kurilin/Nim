module Nim where 
import Char 


--domain
type Board = [Int] --number of objects in each heap
type Turn = (Int, Int) --heap and number of objects to remove 

applyTurn :: Turn -> Board -> Board
applyTurn t b = map 
 (\ (i, v) -> if (i == fst t) then v - snd t else v)
 (zip [1..] b)

empty :: Board -> Bool
empty b = all (<=0) b

--IO stuff
promtInt :: String -> IO Int
promtInt m = do 
 putStr (m ++ "> ")
 c <- getChar
 ignored <- getLine
 return ((ord c) - ord('0'));

readturn :: Board -> IO(Turn)
readturn b = do 
 line <- promtInt "line"
 number <- promtInt "number"
 return (line, number)

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