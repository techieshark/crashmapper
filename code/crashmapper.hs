import System.Exit(exitFailure)
import List(sort)

-- import ODOT for Crash, Street, and friends 
import ODOT 
-- import other modules we'll need
import CSV(CSV, parse, csvFile)
import KML
import MyUtils(pad)


-- For now, files are hard coded:
streetfile = "data/streets.csv"
crashfile = "data/crashes.csv"

main :: IO ()
main = do
       crashes <- readCrashes streetfile crashfile
       putStrLn $ kmlHeader
       putStrLn (crashesToKML crashes)
       putStrLn $ kmlFooter


-- using the streets and crashes CSV files, create a list of crashes  
-- the crahes returned will be complete with street and city names
readCrashes :: FilePath -> FilePath -> IO [Crash]
readCrashes streetfile crashfile = do
                c <- readFile crashfile 
                case parse csvFile crashfile c of
                     Left e -> do putStrLn "Error parsing crash input:"
                                  print e
                                  exitFailure -- we quit!
                     Right r -> do 
                                  streets <- readStreets streetfile
                                  return (map (crashFromRecord streets) r)

-- return the list of Streets in the streets CSV file
readStreets :: FilePath -> IO [Street]
readStreets streetsfile = do
                f <- readFile streetsfile
                case parse csvFile streetsfile f of
                     Left e -> do putStrLn "Error parsing street input: " 
                                  print e
                                  exitFailure -- we quit!
                     Right r -> do 
                                 return (map streetFromRecord r)
                


crashesToKML :: [Crash] -> String
crashesToKML crashes = concatMap (show) placemarks
         where placemarks = map placemarkFromCrash (select crashes)

crashesToText :: [Crash] -> String
crashesToText crashes = concatMap (show) (select crashes)

-- Select which crashes we return (filter and sort them).
-- The filter function can be chosen for all or specific crashes.
select :: [Crash] -> [Crash]
select crashes = sort (filter isIntersectionCrash crashes) 
 
-- filter that returns every crash.
anyCrash :: Crash -> Bool
anyCrash crash = True

-- filter that returns crashes at intersections or where distance
-- from intersection is unknown; these are the easily mappable crashes.
isIntersectionCrash :: Crash -> Bool
isIntersectionCrash crash = distance (location crash) == Distance 0 ||
                            direction (location crash) == Center 



-- convert a Crash to a Placemark
placemarkFromCrash :: Crash -> Placemark
placemarkFromCrash c = Placemark ("ID: " ++ cid c)
                               (address (location c))
                               (description c)
                               (timestamp c)

-- get the crash intersection for use in a KML <address>                        
address :: Location -> String
address l = street l ++ " and " ++ 
            nearbyStreet l ++ 
-- Google Earth doesn't recognize 'outside city limits' so we leave it off
            (case city l of
             "Outside City Limits" -> ", OR"
             _ -> ", " ++ city l ++ ", OR")

description :: Crash -> String
description c = 
     "When: " ++ show (when c) ++ "\n" ++
     "Where: " ++ xmlify (show (location c)) ++ "\n" ++
     "Details: " ++ xmlify (show (details c)) ++ "\n"

-- timestamp will at least be YYYY-MM-DD, and if hour is known, 
--  will include T:hh:00:00-08:00 (Oregon is in UTC-8 time zone)
timestamp :: Crash -> String
timestamp c = datestamp (when c) ++ hourstamp (hour (when c))

datestamp :: DateTime -> String
datestamp (DateTime m d yyyy _ _) = yyyy ++ "-" ++ 
                                    (pad m 2) ++ "-" ++ (pad d 2)

hourstamp :: Hour -> String
hourstamp (Hour h)
    | h < 24 = "T" ++ (pad (show h) 2) ++ ":00:00-08:00"
    | otherwise = "" --ignore unknown or invalid time
