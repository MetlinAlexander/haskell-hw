import Control.Monad.State

-- see this program as example:
{-
x=foo
y=bar
y=$x
l=l
x=$y
-}
{-
At the end of this program the state is:
x = foo
y = foo
l = l
-}

exampleProgram :: String
exampleProgram = "x=foo\ny=bar\ny=$x\nl=l\nx=$y"
examplewrongProgram :: String
examplewrongProgram = "x=foo\ny=bar\ny=$x\nl=l\nx=$r"

-- one of possible answers. order in list doesn't matter
exampleAns :: [(String, String)]
exampleAns = [("x", "foo"), ("y", "foo"), ("l", "l")]

-- check :: IO ()
-- check = do
--     let resultState = solveState exampleProgram
--     if exampleAns `listEq` resultState
--         then putStrLn "OK!"
--         else error "something wrong:("
--     where listEq l r = leftInRight && rightInLeft
--             where leftInRight = all (\x -> x `elem` r) l
--                   rightInLeft = all (\x -> x `elem` l) r

data Value = Literal String | VariableReference String
    deriving (Show)
data Command = Command { varName :: String, whatToPut :: Value }
    deriving (Show)

-- you can choose something else!
type InterpreterState = [(String, String)] 
    

solveState :: String -> Either String [(String, String)]
solveState input = interpretToState (map parse $ lines input)

-- example: "foo=bar" -> Command "foo" (Literal "bar")
-- example: "foo=$bar" -> Command "foo" (VariableReference "bar")
parse :: String -> Command
parse x = parse' "" x
    where parse' comnd (x:xs) | x == '=' = (if (head xs)=='$' 
                                            then Command comnd $ VariableReference $ tail xs 
                                            else Command comnd $ Literal xs)
                              | otherwise = parse' (comnd++[x]) xs

-- you may rewrite this. e.g. you can use fold
-- but if you look at standard library there might be
-- a better alternative for chaining state functions.
-- In other words, executing a list of (State s a)
-- functions is a common task, and it has a standard implementation
interpretMany :: [Command] ->  StateT InterpreterState (Either String) ()
interpretMany [] = return ()
interpretMany (x:xs) = do
    interpretOne x
    interpretMany xs

-- using get, set and other State functions, interpret the command

change_val [] var val = [(var, val)]
change_val (x:xs) var val   | (fst x) == var = (  [(var, val)] ++ xs  ) 
                            | otherwise = [x] ++ change_val xs var val 

interpretOne :: Command -> StateT InterpreterState (Either String) ()
-- interpretOne = undefined
interpretOne (Command var (Literal val)) = do
            temp <- get
            put $ change_val temp var val
            -- lift $ Left "no"
interpretOne (Command var (VariableReference val)) = do
            temp <- get
            if(check_val temp val == False) then lift $ Left $ "this variable was not declared: " ++ val 
            else put $ change_val temp var (find_real_val temp val)

            where   find_real_val (x:xs) val  | fst x == val = snd x
                                              | otherwise = find_real_val xs val
                    check_val (x:xs) val    | fst x == val = True
                                            | otherwise = check_val xs val
                    check_val [] val = False

-- you can choose other type for result
interpretToState :: [Command] -> Either String [(String, String)]
interpretToState commands = execStateT ((interpretMany commands)) emptyState
    where emptyState = []

