{-# LANGUAGE OverloadedStrings #-}

module Parser.URI where

import Control.Applicative
import Control.Monad
import Data.Text (Text)
import Data.Void

import Data.Char qualified as C
import Data.Text qualified as T
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = M.Parsec Void Text

data Uri = Uri
    { uriScheme :: Scheme
    , uriAuthority :: Authority
    }
    deriving (Eq, Show)

data Scheme
    = SchemeHttp
    | SchemeHttps
    | SchemeS3
    | SchemeWs
    | SchemeWss
    deriving (Eq, Show)

data Authority = Authority
    { authHost :: Text
    , authPort :: Maybe Int
    }
    deriving (Eq, Show)

isUnreserved :: Char -> Bool
isUnreserved c =
    any ($ c) [C.isAlpha, C.isDigit, (== '-'), (== '.'), (== '_'), (== '~')]

isReserved :: Char -> Bool
isReserved c =
    c `elem` (genDelims ++ subDelims)

unreserved :: (M.MonadParsec e s m, M.Token s ~ Char) => m (M.Token s)
unreserved = M.satisfy isUnreserved

reserved :: (M.MonadParsec e s m, M.Token s ~ Char) => m (M.Token s)
reserved = M.oneOf (genDelims ++ subDelims)

genDelims :: [Char]
genDelims = [':', '/', '?', '#', '[', ']', '@']

subDelims :: [Char]
subDelims = ['!', '$', '&', '\'', '(', ')', '*', '+', ',', ';', '=']

pUri :: Parser Uri
pUri = do
    scheme <- pScheme
    authority <- pAuthority
    return (Uri scheme authority)

pScheme :: Parser Scheme
pScheme = do
    schemeText <- T.pack <$> M.manyTill M.anySingle (void (M.char ':'))
    case schemeText of
        "http" -> return SchemeHttp
        "https" -> return SchemeHttps
        "s3" -> return SchemeS3
        "ws" -> return SchemeWs
        "wss" -> return SchemeWss
        _ -> fail $ "Unknown scheme: " ++ T.unpack schemeText

pAuthority :: Parser Authority
pAuthority = do
    void (M.string "//")
    authorityText <- T.pack <$> M.some unreserved
    authorityPort <- M.optional (M.char ':' *> L.decimal)
    return $ Authority authorityText authorityPort
