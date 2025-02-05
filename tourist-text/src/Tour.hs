module Tour (reverse) where

import Data.Text (Text)
import Prelude hiding (reverse)

import Data.Text qualified as T

reverse :: Text -> Text
reverse str = case T.uncons str of
    Just (c, rest) -> T.snoc (reverse rest) c
    Nothing -> str
