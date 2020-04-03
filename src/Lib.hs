{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Network.HTTP.Conduit
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy (toStrict)
import qualified Data.Map.Strict as M
import Data.CSV.Conduit.Parser.ByteString
import Data.CSV.Conduit.Types
import Graphics.Matplotlib

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type DataSet = ([B.ByteString], (M.Map B.ByteString [Int]))

url :: String
url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"

addLists :: Num a => [a] -> [a] -> [a]
addLists = zipWith (+)

parseLine :: [B.ByteString] -> (B.ByteString, [Int])
parseLine s = (country, fmap ((\(Just (i,_)) -> i) . B.readInt) xs)
  where
    province:country:_:_:xs = s

download :: IO DataSet
download = do
  response <- fmap toStrict $ simpleHttp url
  let Right ((_:_:_:_:hs):xs) = parseCSV defCSVSettings response
  let xs' = fmap parseLine xs
  return $ (hs, foldr (\(k,v) m -> M.insertWith addLists k v m) M.empty xs')

mean :: Fractional a => [a] -> a
mean xs = sum xs / fromIntegral n
  where
    n = length xs

model :: [Double] -> (Double, Double)
model y = (a,b)
  where
    n  = length y
    x  = fmap fromIntegral [1..n]
    xy = zipWith (*) x y
    x2 = fmap (^2) x
    a  = (mean xy - mean x * mean y) / (mean x2 - (mean x)^2)
    b  = mean y - a * mean x

predict :: (Double, Double) -> Double -> Double
predict (a,b) x = a * x + b

mse :: (Double, Double) -> [Double] -> Double
mse (a,b) ys = mean es
  where
    es = zipWith (\x y -> (a*x + b - y)^2) [1..] ys

plotCountry :: DataSet -> B.ByteString -> Matplotlib
plotCountry (dates, rows) country = scatter xs ys
  where
    ys = rows M.! country
    xs = zipWith const [0..] dates

plotCountryLog :: DataSet -> B.ByteString -> Matplotlib
plotCountryLog (dates, rows) country = scatter xs $ fmap (log . fromIntegral . (+1)) ys
  where
    ys = rows M.! country
    xs = zipWith const [0..] dates

diffs :: Num a => [a] -> [a]
diffs xs = zipWith (-) (tail xs) xs

average :: Fractional a => [a] -> [a]
average (x1:x2:x3:x4:x5:xs) = (x1+x2+x3+x4+x5)/5 : average (x2:x3:x4:x5:xs)
average _ = []

growthRate :: Fractional a => [a] -> [a]
growthRate (x1:x2:x3:xs) = (x3 - x1) / (2 * x2) : growthRate (x2:x3:xs)
growthRate _ = []

plotCountryNew :: DataSet -> B.ByteString -> Matplotlib
plotCountryNew (dates, rows) country = foldr (%) mp $ zipWith (\x y -> bar x y) xs (0 : diffs ys)
  where
    ys = (rows M.! country)
    xs  = zipWith const [0..] $ ys

plotCountryDoubling :: DataSet -> B.ByteString -> Int -> Matplotlib
plotCountryDoubling (dates, rows) country n = plot xs ds
  where
    ys = fmap fromIntegral $ rows M.! country
    as = average ys
    gs = growthRate as
    ds = reverse $ take n $ reverse $ fmap (\x -> log 2 / x) gs
    xs = zipWith const [0..] ds

europe :: [B.ByteString]
europe = [ "Italy"
         , "France"
         , "Spain"
         , "Austria"
         , "Switzerland"
         , "Germany"
         , "Poland"
         , "Portugal"
         , "Belgium"
         , "Netherlands"
         , "Denmark"
         , "Sweden"
         , "Norway"
         , "Finland"
         , "China"
         , "Korea, South"
         , "Japan"
         , "Singapore"
         , "US"
         , "Canada" ]

plotEurope :: IO ()
plotEurope = do
  m <- download
  mapM_ (\country -> do
    let pc = plotCountryDoubling m country 20 @@ [o2 "label" (B.unpack country)]
    let p = xlabel "last 20 days (as of 3 April 2020)" %
            ylabel "doubling time of reported cases" %
            xticks (take 20 [0..]) %
            pc %
            legend @@ [o2 "loc" "upper left"]
    res <- file ("plots/" <> B.unpack country <> ".pdf") p
    print res) europe

plotUSvsSweden :: IO Matplotlib
plotUSvsSweden = do
  m <- download
  let pUS = plotCountryDoubling m "US" 20 @@ [o2 "label" "US"]
  let pSw = plotCountryDoubling m "Sweden" 20 @@ [o2 "label" "Sweden"]
  let p = xlabel "last 20 days" % ylabel "doubling time of positive tests" % xticks (take 20 [0..]) % pUS % pSw
  return $ p % legend @@ [o2 "loc" "upper left"]
