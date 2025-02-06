{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module NWS.Server (main) where

import Data.Aeson ((.:), (.=))
import Data.Text (Text)
import GHC.Generics
import Network.Wai.Handler.Warp (run)
import Servant

import Data.Aeson qualified as Aeson
import Network.Wai qualified as Wai

type ProductTypesAPI = "products" :> "types" :> Get '[JSON] Products

data Product = Product
    { productCode :: Text
    , productName :: Text
    }
    deriving (Generic, Show)

instance Aeson.FromJSON Product

instance Aeson.ToJSON Product

data Products = Products
    { context :: Aeson.Value
    , graph :: [Product]
    }
    deriving (Generic, Show)

instance Aeson.FromJSON Products where
    parseJSON = Aeson.withObject "Products" $ \v ->
        Products
            <$> v .: "@context"
            <*> v .: "@graph"

instance Aeson.ToJSON Products

products1 :: Products
products1 =
    Products
        { context =
            Aeson.object
                [ "@version" .= ("1.1" :: Text)
                , "@vocab" .= ("https://api.weather.gov/ontology#" :: Text)
                ]
        , graph =
            [ Product
                { productCode = "CLI"
                , productName = "Climatological Report (Daily)"
                }
            , Product
                { productCode = "CLM"
                , productName = "Climatological Report (Monthly)"
                }
            ]
        }

productsAPI :: Proxy ProductTypesAPI
productsAPI = Proxy

app :: Wai.Application
app = serve productsAPI server1

server1 :: Server ProductTypesAPI
server1 = return products1

main :: IO ()
main = run 8081 app
