{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module NWS.Product
    ( ProductAPI
    , ProductType (..)
    , ProductTypes (..)
    , Location (..)
    , Locations (..)
    , FromJSON (..)
    , ToJSON (..)
    ) where

import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , (.:)
    , (.=)
    )
import Data.Aeson.Types (Parser)
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics
import Servant.API

import Data.Aeson qualified as Aeson
import Data.Map qualified as Map

type ProductAPI =
    "products" :> "types" :> Get '[JSON] ProductTypes
        :<|> "products" :> "types" :> Capture "typeId" Text :> "locations" :> Get '[JSON] Locations

data ProductType = ProductType
    { productCode :: Text
    , productName :: Text
    }
    deriving (Generic, Show)

instance FromJSON ProductType

instance ToJSON ProductType

newtype ProductTypes = ProductTypes [ProductType] deriving (Show)

instance FromJSON ProductTypes where
    parseJSON = Aeson.withObject "ProductTypes" $ \obj -> do
        graph <- obj .: "@graph"
        ProductTypes <$> mapM Aeson.parseJSON graph

instance ToJSON ProductTypes where
    toJSON (ProductTypes productTypes) =
        Aeson.object
            [ "@context"
                .= Aeson.object
                    [ "@version" .= ("1.1" :: Text)
                    , "@vocab" .= ("https://api.weather.gov/ontology#" :: Text)
                    ]
            , "@graph" .= productTypes
            ]

data Location = Location
    { locationId :: Text
    , locationName :: Maybe Text
    }
    deriving (Generic, Show)

instance FromJSON Location

instance ToJSON Location

newtype Locations = Locations [Location] deriving (Show)

instance FromJSON Locations where
    parseJSON = Aeson.withObject "Locations" $ \obj -> do
        locMap <- obj .: "locations"
        let locList = Map.toList locMap
        locs <- mapM (\(id, nameVal) -> parseLocation id nameVal) locList
        return (Locations locs)

instance ToJSON Locations where
    toJSON (Locations locs) =
        Aeson.object
            [ "@context" .= ([] :: [Aeson.Value])
            , "locations" .= toLocationMap locs
            ]

toLocationMap :: [Location] -> Map Text Aeson.Value
toLocationMap = Map.fromList . map (\loc -> (locationId loc, maybe Aeson.Null Aeson.toJSON $ locationName loc))

parseLocation :: Text -> Aeson.Value -> Parser Location
parseLocation id nameVal = do
    name <- case nameVal of
        Aeson.Null -> return Nothing
        _ -> Just <$> Aeson.parseJSON nameVal
    return (Location id name)
