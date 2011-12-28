module Street (Street(..), 
               findCityName, 
               findStreetName, 
              ) where

import MyUtils(pad)


data Street = Street { county :: String,
                       streetid :: StreetID,
                       streetname :: String,
                       cityid :: CityID,
                       cityname :: String
                     } deriving Show 

type CityID = String
type StreetID = String

                       

-- get the name of the city. the Street array holds info about 
-- streets, but also lets us lookup City's. Since there will be lots
-- of matches for a CityID (one for each street in that city, in fact),
-- we'll just use the name from the first one.
findCityName :: [Street] -> CityID -> String 
findCityName streets cid = cityname $ head $ [ s | s <- streets, cid == cityid s]


-- return name of street specified by CityID, StreetID 
findStreetName :: [Street] -> CityID -> StreetID -> String
findStreetName streets cid sid = 
    case findStreetArr streets cid sid of
         [] -> error ("no street found for city: " ++ cid ++ ", street: " ++ sid)
         (x:y:zs) -> error "multiple streets found in street database for city: " ++
               cid ++ ", street: " ++ sid
         (s:ss) -> streetname s 


-- find a street given a city and street id
-- find all streets matching..
findStreetArr :: [Street] -> CityID -> StreetID -> [Street]
findStreetArr streets cid sid = [ s | s <- streets, 
                                      cityid s == cid, 
                                      streetid s == (padStreet sid) ]

-- given a StreetID, return the same id prepended 
-- with 0's so that it is 5 characters long.
padStreet :: StreetID -> StreetID 
padStreet s = pad s 5
