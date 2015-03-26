{-
	This file contains logging/debugging functions.
-}

module Error (liftEither) where

import Job

-- The error to show if verbose errors are off.
defaultErr :: String
defaultErr = "compilation failed"

-- Removes the value from an Either monad, or throws an error if there is one.
-- If errors are off, throws the defaultErr instead of the error supplied.
liftEither :: (Show e, Monad m) => Job -> Either e a -> m a
liftEither job (Left err) = error $ if verb job then show err else defaultErr
liftEither _ (Right val) = return val
