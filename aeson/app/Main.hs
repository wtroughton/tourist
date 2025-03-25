{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson

main :: IO ()
main = putStrLn "Hello world"

data Currency
    = USD
    | BTC
    deriving (Eq, Show)

instance FromJSON Currency where
    parseJSON = withText "Currency" $ \s ->
        case s of
            "USD" -> return USD
            "BTC" -> return BTC
            _ -> fail $ "parsing Currency failed, unexpected " ++ show s

instance ToJSON Currency where
    toJSON USD = "USD"
    toJSON BTC = "BTC"

data Money = Money
    { amount :: Integer
    , currency :: Currency
    }
    deriving (Eq, Show)

instance FromJSON Money where
    parseJSON (Object v) =
        Money
            <$> v .: "amount"
            <*> v .: "currency"
    parseJSON _ = fail "expected an object"

instance ToJSON Money where
    toJSON Money{amount, currency} =
        object
            [ "amount" .= amount
            , "currency" .= toJSON currency
            ]
