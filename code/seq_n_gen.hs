--DATA TYPES AND IMPORTS----------------------------------------------------------------------
import Data.List
import Data.Char
import System.IO

data Bin = Leaf | Node Bin Bin
    deriving (Show, Eq, Read)

data InpForm =  
            C Char 
            | Lu InpForm InpForm
        deriving (Show,Eq, Read)



-----------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------
--DUPLICATE------------------------------------------------------------------------------------------

simp :: InpForm -> Int -> (InpForm, Int)
simp (C c) b = (C c, b)
simp (Lu (C a) (C b)) z = (Lu (C a) (C b),z)
simp (Lu x y) b 
    | x == y = (Lu (C (chr b)) (C (chr b)), (b+1))
    | otherwise = let (x', b') = simp x b
                      (y', bf) = simp y b'
                  in (Lu x' y', bf)


simplify :: InpForm -> InpForm
simplify x 
    | fst (simp x 97) == x = x
    | otherwise = simplify (fst(simp x 97))


---------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------
-- TREE GENERATION ----------------------------------------------------------------------------
        
-- Catalan(n-1) * bell(n)

joiner :: [Bin] -> [Bin] -> [Bin]
joiner [] _ = []
joiner _ [] = []
joiner xs ys = [Node a b | a <- xs, b <- ys]

helper :: Int -> Int -> Maybe [Bin]
helper _ 1 = Just [Leaf]
helper _ 2 = Just [Node Leaf Leaf]
helper _ 3 = Just [Node (Node Leaf Leaf) Leaf, Node Leaf (Node Leaf Leaf)]
helper counter n = do
    let opp = n - counter
    a <- treeGen counter
    b <- treeGen opp
    Just (joiner a b)

treeGen :: Int -> Maybe [Bin]
treeGen 1 = Just [Leaf]
treeGen 2 = Just [Node Leaf Leaf]
treeGen 3 = Just [Node (Node Leaf Leaf) Leaf, Node Leaf (Node Leaf Leaf)]
treeGen n
    | n > 3 = do
        combinations <- sequence [helper i n | i <- [1..n-1]]
        return (concat combinations)
    | otherwise = Nothing

-------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------
--PARTITION-------------------------------------------------------------------------------------------------

appendToLists :: a -> [[a]] -> [[[a]]]
appendToLists x lists = zipWith (\i lst -> take i lists ++ [lst ++ [x]] ++ drop (i+1) lists) [0..] lists

applyToList :: Int -> [[[Int]]] -> [[[Int]]]
applyToList x [] = []
applyToList x (y:ys) = (appendToLists x y) ++( ([x]:y) : (applyToList x ys))

part :: [Int] -> [[[Int]]]
part [] = [[]]
part [x] = [[[x]]]
part [x, y] = [[[x], [y]], [[x, y]]]
part (x:xy) = applyToList x (part xy)

filt :: Int -> [[Int]] -> Bool
filt a x 
    | length x == a = True
    | otherwise = False

filterer :: Int -> [[[Int]]] -> [[[Int]]]
filterer a [] = []
filterer a (x:xs) 
    |filt a x = x:(filterer a xs)
    | otherwise = filterer a xs

secfilt :: [[Int]] -> Bool
secfilt = all (\lst -> even (length lst))


secfilterer :: [[[Int]]] -> [[[Int]]]
secfilterer [] = []
secfilterer (x:xs)
    | secfilt x = x:(secfilterer xs)
    | otherwise = secfilterer xs
-------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------
--TREE TO INPFORM-----------------------------------------------------------------------------------------------------------
setter :: Int -> Int -> [Int] -> [Int]
-- Sets 1 element in the list to the integer
setter _ _ [] = []
setter x a (y:ys)
  | x == 0    = a : ys          -- Set the x-th element to 'a'
  | otherwise = y : setter (x - 1) a ys  -- Continue traversing the list

setList :: [Int] -> Int -> [Int] -> [Int]
--sets all elements in the list to the integer 
setList [] _ l = l
setList (x:xs) a l = setList xs a (setter x a l)


setSeq :: [[Int]] -> Int -> [Int] -> [Int]
--sets all elements in the seq to the integer increments by 1 and moves to the next list 
setSeq [] _ l= l 
setSeq (x:xs) a l = setSeq xs (a+1) (setList x a l)

zerosList :: Int -> [Int]
zerosList n = replicate n 0

depth :: Bin -> Int
-- Gives depth of a binary tree
depth Leaf = 1
depth (Node a b) = 1 + max (depth a) (depth b)

binMaker :: [Int] -> Bin -> InpForm
binMaker x Leaf = C (chr(x!!0))
binMaker x (Node a b) = (Lu (binMaker (take (countLeaves a 0) x) a) (binMaker (drop (countLeaves a 0) x) b))

countLeaves :: Bin -> Int -> Int 
countLeaves Leaf x = x+1
countLeaves (Node a b) x = do
    let x' = countLeaves a x
    countLeaves b x'

-------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------
--Writer-----------------------------------------------------------------------------------------------------------




writer ::IO ()
writer = do
    let n = 6
    let file1name = "output2.txt"
    let file2name = "output3.txt"
    writeFile file1name (twH(extract (treeGen n)))
    writeFile file2name (partWrite n)

extract :: Maybe [Bin] -> [Bin]
extract (Just x) = x
extract Nothing = [Leaf]
    
twH :: [Bin] -> String
twH [] = ""
twH [x] = show x
twH (x:xs)  =(show x) ++ "\n" ++ (twH xs)

partWrite :: Int -> String
partWrite n = pwH(secfilterer(filterer 1 (part [0..(n-1)])))
--partWrite n = pwH(((part [0..(n-1)])))
--partWrite n = pwH(secfilterer((part [0..(n-1)])))
--partWrite n = pwH((filterer 1 (part [0..(n-1)])))

pwH :: [[[Int]]] -> String
pwH [] = ""
pwH [x] = show x
pwH (x:xs) = (show x) ++ "\n" ++ (pwH xs)

-------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------
--Main, Print, and Test-----------------------------------------------------------------------------------------------------------

processTree :: String -> String -> String
processTree line contents = do
    let t = read line :: Bin
    let n = 6
    let b = False
    let inputLines = lines contents
    let answer = concatMap (processPart t n b) inputLines
    answer

main :: IO ()
main = do
    let filename = "output4.txt"
    contents <- readFile "output2.txt"
    contents3 <- readFile "output3.txt"

    -- Process each line from the file
    let inputLines = lines contents
    let resultString = concatMap (\line -> processTree line contents3) inputLines
    writeFile filename resultString
    putStrLn "Success"


processPart :: Bin -> Int -> Bool -> String -> String 
processPart tree n b line = do
    let p = read line :: [[Int]]
    case b of  
        True -> inpprinter (binMaker (setSeq p 97 (zerosList n)) tree) ++ "\n"
        False -> show (binMaker (setSeq p 97 (zerosList n)) tree) ++ "\n"


inpprinter :: InpForm -> String 
inpprinter (C c) = [c]
inpprinter (Lu a b) = "("++inpprinter a ++ " -o "++inpprinter b++")"


-- 3 for 10 gives 9450 
-- 4 for 10 gives 
