type Variables = (Int, Int, Int)
data MonadVars a = MonadVars { actualComputation :: (Variables -> (a, Variables)) }

runComputation :: MonadVars a -> Variables -> (a, Variables)
runComputation computation vars = actualComputation computation $ vars

getVars :: MonadVars Variables
getVars = MonadVars $ \vars -> (vars, vars)


instance Functor MonadVars where
    -- fmap :: (a -> b) -> (MonadVars a) -> (MonadVars b)
    fmap f (MonadVars comp) =
        MonadVars $ \vars -> let (result, newVars) = comp vars in (f result, newVars)

instance Applicative MonadVars where
    pure x = MonadVars $ \vars -> (x, vars)
    (MonadVars compF) <*> (MonadVars compX) = MonadVars $ \vars -> let (f, vars1) = compF vars
                                                                       (r, vars2) = compX vars1
                                                                       in (f r, vars2)

instance Monad MonadVars where
    -- (>>=) :: m a –> (a –> m b) –> m b
    -- (>>=) :: MonadVars a -> (a -> MonadVars b) -> MonadVars b

    (MonadVars comp1) >>= f = MonadVars $ \vars -> let (r, vars1) = comp1 vars
                                                   in runComputation (f r) vars1

putVars :: Variables -> MonadVars ()
putVars newVars = MonadVars $ \vars -> ((), newVars)

computation :: MonadVars Int
computation = do
    (x1, x2, x3) <- getVars
    putVars (x1 + 2, x2 + 1, x3 * 3)
    return 200

main :: IO ()
main = do
    print $ runComputation computation (20, 30, 40)