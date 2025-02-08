{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import Servant

import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp

import NWS.Product
    ( Location (..)
    , Locations (..)
    , ProductAPI
    , ProductType (..)
    , ProductTypes (..)
    )

main :: IO ()
main = Warp.run 8081 app

app :: Wai.Application
app = serve productAPI server

productAPI :: Proxy ProductAPI
productAPI = Proxy

server :: Server ProductAPI
server = productTypes :<|> locations
    where
        productTypes :: Handler ProductTypes
        productTypes = return productTypesResponse

        locations :: Text -> Handler Locations
        locations productCode = case productCode of
            "CLI" -> return locationsResponse1
            _ -> return locationsResponse2

productTypesResponse :: ProductTypes
productTypesResponse =
    ProductTypes
        [ climateReportDaily
        , climateReportMonthly
        ]

climateReportDaily :: ProductType
climateReportDaily =
    ProductType
        { productCode = "CLI"
        , productName = "Climatological Report (Daily)"
        }

climateReportMonthly :: ProductType
climateReportMonthly =
    ProductType
        { productCode = "CLM"
        , productName = "Climatological Report (Monthly)"
        }

locationsResponse1 :: Locations
locationsResponse1 =
    Locations
        [Location "LOT" (Just "Chicago, IL")]

locationsResponse2 :: Locations
locationsResponse2 =
    Locations
        [ Location "1V4" Nothing
        , Location "20V" Nothing
        , Location "ABQ" (Just "Albuquerque, NM")
        , Location "ABR" (Just "Aberdeen, SD")
        ]
