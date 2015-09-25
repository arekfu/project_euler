module MonadUtils
( breakM
, takeWhileM
) where

breakM :: (Eq a, Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
breakM _ xs@[] = return (xs, xs)
breakM predM l@(x:xs) = do
    val <- predM x
    case val of
        True -> return ([], l)
        _ -> do
                (ys, zs) <- breakM predM xs
                return (x:ys, zs)

takeWhileM :: Monad m => (a -> m Bool) -> [a] -> m [a]
takeWhileM _ xs@[] = return xs
takeWhileM predM l@(x:xs) = do
    val <- predM x
    case val of
        False -> return []
        _ -> do
                ys <- takeWhileM predM xs
                return (x:ys)
