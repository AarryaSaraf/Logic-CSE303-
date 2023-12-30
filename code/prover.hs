import System.IO

data InpForm =  
            C Char 
            | Lu InpForm InpForm
        deriving (Show,Eq,Read)
        
type ListInpForms = [InpForm]  

type Sents = [ListInpForms]

type Allsents = [Sents]

-- Pretty printers
inpprinter :: InpForm -> String 
inpprinter (C c) = [c]
inpprinter (Lu a b) = "("++inpprinter a ++ " -o "++inpprinter b++")"

listprinter :: ListInpForms -> [String]
listprinter [] = []
listprinter (x:xs) = (inpprinter x):(listprinter xs)

sentprinter :: Sents -> [[String]]
sentprinter [] = []
sentprinter (x:xs) = (listprinter x):(sentprinter xs)

msentprinter :: Maybe Sents -> [[String]]
msentprinter Nothing = []
msentprinter (Just x) = (sentprinter x)

allprinter :: Allsents -> [[[String]]]
allprinter [] = []
allprinter (x:xs) = (sentprinter x):(allprinter xs)  

---------------------------------------------------------------------------------------------------------------------

splitone :: InpForm -> ListInpForms
-- splitting a -o  b into a,b
splitone (Lu a b) = [a,b]
splitone (c) = [c]

rrule :: ListInpForms -> ListInpForms
-- applies right rule once
rrule (x:xs)
    |length xs > 0 = x:rrule(xs)
    |otherwise = splitone x

lastchar :: InpForm -> Bool
-- checks if last InpForm is a character. If yes that means we must apply the right rule
lastchar (C _) = True
lastchar (Lu _ _ )= False

repeatrrule :: ListInpForms -> ListInpForms
-- applies the right as many times as possible so we can proceed on to using the right rule
repeatrrule x 
    |lastchar(last x) = x
    |otherwise = repeatrrule (rrule x)
     
irule :: ListInpForms -> Maybe Bool
-- returns true if the indentity rule can be applied. Given only the last 2 list elements
irule [(C c),(C d)] = Just(c==d)
irule _ = Nothing

caseab :: InpForm -> InpForm -> Sents
-- applies the left rule but when there are no delta and gamma
caseab (Lu a b) (C c) = [[a]     ,    [b,(C c)]] 
caseab _ _ = [[]]

listsplitterfst :: ListInpForms -> Int -> ListInpForms
--first split of the list
listsplitterfst (x:xs) n 
    |n==1 = []
    |n>1 = x:(listsplitterfst xs (n-1))
listsplitterfst _ _ = []

listsplittersnd :: ListInpForms -> Int -> ListInpForms
--first split of the list
listsplittersnd (x:xs) n 
    |n==1 = (x:xs)
    |n>1 = listsplittersnd xs (n-1)
listsplittersnd _ _ = []

caseabtd :: ListInpForms -> InpForm -> Int -> Sents
-- applies the left rule when there is a delta or gamma. the int is supposed to decide where the delta starts. A 0 means that there is no delta at all.
-- The inpform is the last character
caseabtd ((Lu a b):xs) (C c) n
    |n == 0 =   [xs++[a]     ,       [b,(C c)]]
    |otherwise = [ (listsplitterfst xs n) ++[a]   ,  [b]++(listsplittersnd xs n)++[(C c)] ]
caseabtd _ _ _ = [[]]

lrule :: ListInpForms -> InpForm -> Int -> Allsents
-- applies all left rule
lrule (x:xs) c n
    |((length xs) == 0) = [caseab x c]
    |n> (-1) = (caseabtd (x:xs) c n):(lrule (x:xs) c (n-1))
    |otherwise = []
lrule [] c _ = [[[c]]]

removeLast :: ListInpForms -> ListInpForms
-- remove the last element from the list
removeLast [] = []         
removeLast [_] = []         
removeLast (x:xs) = x : removeLast xs

bevaluator :: ListInpForms -> Maybe Bool
-- True denotes the formula is true. Nothing denotes nothing can be said about it. False denotes its false
bevaluator [C c] = Just False
bevaluator [x] = Nothing
bevaluator [x,y] = (irule([x,y]))
bevaluator _ = Nothing


evaluator :: Sents ->  Maybe Sents
-- Removes all true Lists and if there is 1 false one then it returns nothing.
evaluator [] = Just []
evaluator (x:xs) 
    |bevaluator x == Just False = Nothing
    |bevaluator x == Just True = evaluator xs 
    |bevaluator x == Nothing = do  
        y <- evaluator xs
        return (x:y)

decider :: ListInpForms -> Allsents
decider x = 
    let 
        tp = last x
        lside = removeLast x
    in (lrule (lside) (tp) (length lside -1))

decider2:: ListInpForms -> Int
decider2 [(C c),(C d)] 
    |c==d = 1
    |otherwise = 0
decider2 ((C _):xs) = 0
decider2 x 
    |bevaluator x == Just False = 0
    |bevaluator x == Just True = 1
    |bevaluator x == Nothing = eval(decider x)
    |otherwise = 0

sdecider :: Sents -> Int
sdecider [] = 1
sdecider (x:xs) = (decider2 (repeatrrule x)) * (sdecider xs)

msdecider :: Maybe Sents -> Int
msdecider Nothing = 0
msdecider (Just [])  =1
msdecider (Just x ) = (sdecider x)

eval:: Allsents -> Int
eval (x:xs) = (msdecider (evaluator x)) + (eval xs)
eval _ = 0
    
-- Your existing code...

main :: IO ()
main = do
    -- Read the input from the file
    handle <- openFile "output5.txt" ReadMode
    contents <- hGetContents handle

    -- Process each line from the file
    let inputLines = lines contents
    mapM_ processLine inputLines

    -- Close the file handle
    hClose handle

processLine :: String -> IO ()
processLine line = do
    let s = read line :: InpForm
    let inputForm = [s]
    let answer = decider2 inputForm
    putStrLn $ "The sequent is " ++ inpprinter s
    putStrLn $ "In our calculus, this sequent has " ++ show answer ++ " proof(s)"
    --if answer > 0
        --then putStrLn $ "Printing s: " ++ show (inpprinter s)
        --else return ()

summer :: IO ()
summer = do
    -- Read the input from the file
    handle <- openFile "output4.txt" ReadMode
    contents <- hGetContents handle

    -- Process each line from the file
    let inputLines = lines contents
    let totalProofs = sum (map addLine inputLines)
    putStrLn (show totalProofs)
    -- Close the file handle
    hClose handle

counting :: IO ()
counting = do
    let initialCounts = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0]
    handle <- openFile "output.txt" ReadMode
    contents <- hGetContents handle
    let inputLines = lines contents
    let (_, finalCounts) = foldl (\(proofs, counts) line -> let updatedCounts = countLine line counts in (proofs + head updatedCounts, updatedCounts)) (0, initialCounts) inputLines
    putStrLn (show finalCounts)
    hClose handle

countLine :: String -> [Int] -> [Int]
countLine line counts = do
    let s = read line :: InpForm -- Replace InpForm with your actual type
    let inputForm = [s]
    let answer = decider2 inputForm -- Replace decider2 with your actual function
    incrementAtIndex counts answer

incrementAtIndex :: [Int] -> Int -> [Int]
incrementAtIndex counts index =
    take index counts ++ [counts !! index + 1] ++ drop (index + 1) counts



    
addLine :: String -> Int
addLine line = do
    let s = read line :: InpForm
    let inputForm = [s]
    let answer = decider2 inputForm
    answer



    

    



    -- let rres = repeatrrule inputForm
    -- let tp = last rres
    -- let lside = removeLast rres
    -- let lres = lrule lside tp (length lside -1)
    -- let answer = eval lres
    -- putStrLn $ "Input: " ++ show (listprinter inputForm)
    -- putStrLn $ "After right rule(s): " ++ show (listprinter rres)
    -- putStrLn $ "All possible after left rule(s):" ++ show (allprinter lres)
    -- putStrLn $ "Machine representationS:" ++ show (lres)
    {-
implies as b  = foldr Lu as b
-}