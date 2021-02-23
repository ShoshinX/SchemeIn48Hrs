module Error
    ( LispError(..)
    , ThrowsError
    , extractValue
    , trapError
    ) where

-- Why would this module be used?
import Control.Monad.Except
import Common



trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

