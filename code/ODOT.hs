module ODOT (Crash(..), 
             DateTime(..), 
             Day(..), 
             Hour(..),
             Location(..),
             Distance(..),
             Direction(..), 
             Details(..),
             Street(..), 
             crashFromRecord,
             streetFromRecord, 
             findStreetName,
             findCityName,
            ) where

import CSV(CSV, Record)
import ODOTCrash
import Street
import MyUtils(arrRange, pad)



-- the code in this file meshes the street, crash, and CSV definitions


-- convert from CSV format to Street data type
streetFromRecord :: Record -> Street
streetFromRecord r = Street (r!!0) (r!!1) (r!!2) (r!!3) (r!!4)


-- crashFromRecord, being the top level parser for the crash Record 
-- passes off the parsing of each section to the ..FromArray functions.
-- the [Street] is built from streetFromRecord on the street CSV file.
crashFromRecord :: [Street] -> Record -> Crash
crashFromRecord streets r = Crash (r!!0) 
                          (dateFromArray (arrRange r (1,5))) 
                          (locationFromArray streets (arrRange r (6,10)))
                          (detailsFromArray (arrRange r (11,13)))


dateFromArray :: [String] -> DateTime
dateFromArray a = DateTime (a!!0) (a!!1) (a!!2) 
                           (toDay (read (a!!3)::Int)) (Hour (read (a!!4)::Int))
-- is there a better way of writing this?  
-- something like DateTime (a[0..2] Day.. a[4])


--note, the only problem with this example is that the values of the 
--enumerated type Day is 0..6, but in the ODOT data it is recorded as
-- 1..7. This necessitates the use of fromDay and toDay.  
-- granted, the name 'toDay' might be confused with 'today', 
-- but we'll leave it in keeping with names like toEnum.
toDay :: Int -> Day
toDay n = toEnum (n-1)::Day

fromDay :: Day -> Int
fromDay d = (fromEnum d) + 1


--parse the crash location
--streets is the street database that we'll use to find the 
--name of cities and streets.
locationFromArray :: [Street] -> [String] -> Location
locationFromArray streets a = Location (findCityName streets (a!!0)) 
                               (findStreetName streets (a!!0) (a!!1))
                               (findStreetName streets (a!!0) (a!!2))
                               (parseDistance (a!!3))
                               (toDirection (read (a!!4)::Int))


parseDistance:: String -> Distance 
parseDistance d
              | head d == ' ' = UnknownDistance
              | otherwise = Distance (read d::Int)
--parseDistance (' ':ds) = Unknown..
--parseDistance (ds) = Distance.. TODO

--parse the crash details
detailsFromArray :: [String] -> Details
detailsFromArray a = Details (a!!0) (a!!1) (a!!2)




-- some example crashes. 
{- to re-enable this code, we'd need to make code handle empty streetlist
crashes :: [Crash]
crashes =  map crashFromRecord crashlist
-}


-- DEBUG / testing: string version of crashes, as would be found in CSV file.
crashlist :: CSV -- CSV == [[String]]
-- Each crash record is of the form:
-- ID Month DayOfMonth Year WeekDay Hour city street nearbystreet distance 
--     ... direction crashtype collisiontype severity 
crashlist =  [
              ["241463","7","29","1997","3","16","41","201","5520","    ","9","6","6","4"],
              ["221111","8","1","2006","3","18","18","2602","8005","0","5","6","6","2"],
              ["106636","5","28","2004","6","16","0","2659","2279","100","3","6","5","2"],
              ["159674","6","18","2005","7","6","194","2302","2604","0","9","6","3","2"],
              ["227402","9","15","2006","6","18","0","2626","2724","0","9","6","1","2"],
              ["42804","4","12","2001","5","14","92","601","1201","0","9","6","6","4"]
             ]


--streetlist is an example of the array we'll get when parsing the CSV file
streetlist :: CSV
-- format is ["CNTY_NO","STREET_NO","STREET_NM","CITY_ID","CITY_NM"]
streetlist = [
               ["34","00904","00904 UNKNOWN NAME","249","NW Portland"],
               ["34","02511","VERMONT ST",        "247","SW Portland"],
               ["34","02552","TERRI CT",          "247","SW Portland"],
               ["34","01828","OLESON RD",         "247","SW Portland"],
               ["34","01901","01901 UNKNOWN NAME","234","Wilsonville"],
               ["34","00301","CANYON CREEK RD",   "234","Wilsonville"],
               ["34","02401","UMATILLA ST",       "215","Tualatin"],
               ["34","01415","LONDON CT",         "211","Tigard"]
             ]
