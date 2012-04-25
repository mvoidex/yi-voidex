module Yi.Mode.Haskell.HLint (
    hlint
    ) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)

import Yi.Core
import Yi.File

import qualified Language.Haskell.HLint as HLint

-- | HLint current buffer
hlint :: YiM ()
hlint = do
    fwriteE
    Just fileName <- withBuffer $ gets file
    hints <- liftIO $ HLint.hlint [fileName, "--quiet"]
    void $ withOtherWindow $ withEditor $
        newBufferE (Left "hlint") (fromString $ unlines $ map show hints)
