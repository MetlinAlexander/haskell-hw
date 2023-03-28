-- state monad

import Control.Monad.State
type RandState = Int

rollDice :: State RandState RandState
rollDice = do
    x <- get
    put ((x*1234567 + 1) `mod` x)
    return $ if (x `mod` 7 == 0) then ((x+1) `mod` 7 ) else (x `mod` 7)


game :: State RandState String
game = do
    firstPlayerRes <- rollDice
    secondPlayerRes <- rollDice
    return $ if firstPlayerRes > secondPlayerRes then ("First wins witn " ++ (show firstPlayerRes) ++ "/" ++ (show secondPlayerRes)) else ("Second wins with " ++ (show firstPlayerRes) ++ "/" ++ (show secondPlayerRes))

runGame :: String
runGame = evalState game startSeed
    where startSeed = 265