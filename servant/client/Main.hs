{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Proxy
import Data.Text (Text)
import Servant.API
import Servant.Client

import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS qualified as HTTP
import Network.HTTP.Types.Header qualified as HTTP

import NWS.Product
    ( Locations (..)
    , ProductAPI
    , ProductTypes (..)
    )

main :: IO ()
main = do
    manager' <- HTTP.newManager HTTP.defaultManagerSettings
    res <- runClientM queries (mkClientEnv manager' (BaseUrl Http "localhost" 8081 ""))
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right resp -> do
            print resp

api :: Proxy ProductAPI
api = Proxy

productTypes :: ClientM ProductTypes
locations :: Text -> ClientM Locations
productTypes :<|> locations = client api

queries :: ClientM (ProductTypes, Locations)
queries = do
    p <- productTypes
    loc <- locations "CLI"
    return (p, loc)

managerSettings :: HTTP.ManagerSettings
managerSettings =
    HTTP.tlsManagerSettings
        { HTTP.managerModifyRequest = \req -> do
            req' <- HTTP.managerModifyRequest HTTP.tlsManagerSettings req
            return
                $ req'
                    { HTTP.requestHeaders =
                        (HTTP.hUserAgent, "tourist/0.1.0.0")
                            : HTTP.requestHeaders req'
                    }
        }
