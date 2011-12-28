module ODOTCrash(Crash(..),
                 DateTime(..),
                 Day(..),
                 Hour(..),
                 Location(..),
                 Distance(..),
                 Direction(..), 
                 Details(..),
                 toDirection
                ) where

type CrashID = String -- hmm...perhaps this could be an Int.

-- Crash is the container for all the crash data 
-- (including children elements like Location & Details)
data Crash = Crash 
                  {
                    cid::CrashID,
                    when::DateTime, --record elements 1..5 stored here
                    location :: Location, -- 6..10
                    details :: Details -- 11..13.  crashtype, severity, etc.
                  } deriving Eq

-- DateTime format: 
--    Month Day Year DayOfWeek Hour
--    Month: 1-12. 
--      Day: 1-31.  
--      Year: YYYY.  
-- DayOfWeek: 1=Sun,7=Sat.  
--      Hour: 00=12am-1am, 23=11pm-11:59pm 99=unknown
data DateTime = DateTime 
                    { m::String,
                      d::String,
                      yyyy::String,
                      day::Day,
                      hour::Hour 
                    } deriving Eq

data Day = Sun | Mon | Tue | Wed | Thu | Fri | Sat deriving (Show, Enum, Eq)


data Hour = Hour Int deriving Eq

data Location = Location 
                  { 
                    city::String, -- city name
                    street::String, -- street name
                    nearbyStreet::String, --name of nearest intersecting street
                    distance::Distance, --from nearest intersection
                    direction::Direction --from center of nearest intersection
                  } deriving Eq
                 --note, zip isn't given, and State is always Oregon.

data Distance = UnknownDistance | Distance Int deriving Eq

data Direction = Unknown | North | Northeast | 
                 East | Southeast | South | Southwest | West | 
                 Northwest | Center deriving (Show, Enum, Eq)

toDirection :: Int -> Direction
toDirection n = toEnum n::Direction
		
type CrashType = String
type CollisionType = String

data Details = Details 
                  { 
                     -- crashtype is bike or ped
                     crashType::CrashType, 
                     -- collision type is like head-on, sideswipe, etc
                     collisionType::CollisionType, 
                     -- severity is injury or fatal
                     severity::String
                  } deriving Eq



instance Show Crash where
    show (Crash cid when location details) =
        "Crash: \n" ++
        "\tCrashID: " ++ cid ++ "\n" ++
        "\tWhen: " ++ show when ++ "\n" ++
        "\tWhere: " ++ show location ++ "\n" ++
        "\tDetails: " ++ show details ++ "\n"


--provide a human friendly view of a DateTime
instance Show DateTime where
    show (DateTime m d y day hour) = 
		(show day) ++ ", " ++ (m) ++ "/" ++ (d) ++ "/" ++ (y) ++ 
		" @ " ++ (show hour) ++ " "


--cmp two dates, return true if date1 is <= date2
cmp :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> Bool
cmp (y1, m1, d1, h1) (y2, m2, d2, h2) =
    y1 < y2 ||
    (y1 == y2 && m1 < m2) ||
    (y1 == y2 && m1 == m2 && d1 < d2) ||
    (y1 == y2 && m1 == m2 && d1 == d2 && h1 <= h2)

-- convert DateTime to (y,m,d,h) format
dateToTuple :: DateTime -> (Int, Int, Int, Int)
dateToTuple (DateTime m d y _ (Hour h)) =
    (read y::Int, read m::Int, read d::Int, h)

-- allow comparison of two dates
instance Ord DateTime where
    (<=) d1 d2 = 
        cmp (dateToTuple d1) (dateToTuple d2)     

-- crash1 is "less than" crash2 if crash1 happened before crash2
-- this allows a list of crashes to be sorted by date.
instance Ord Crash where
    (<=) c1 c2 = (when c1) <= (when c2)

instance Show Location where
    show (Location city street nearby dist dir) = 
        show dist ++ " (" ++ 
        show dir ++ ") of intersection of " ++
        street ++ " & " ++ 
        nearby ++ " in " ++ city

instance Show Distance where
    show (Distance d)
      | (d == 0000) = "inside of"
      | (d < 9999) = show d ++ " ft from"
      | (d == 9999) = "over 9999 ft from"
    show UnknownDistance = "unknown distance from"

instance Show Details where
    show (Details crashtype collision severity) = 
        "Type: " ++ (showCrashType crashtype) ++ 
        ". Collision: " ++ (showCollision collision) ++
        ". Severity: " ++ (showSeverity severity) ++ "."


-- TODO: Hour should be either UnknownHour | Hour Int
instance Show Hour where
    show (Hour h)   
        | (h == 0)  = "12am"
        | (h < 12)  = (show h) ++ "am" --ex: 1 = 1am, 11=11am
        | (h == 12) = "12pm"
        | (h < 24)  = (show (h-12)) ++ "pm" --ex: 13 = 1pm, 23 = 11pm
        -- note, according to the docs, '24' is not used.
        | (h == 99) = "unknown time"
        | otherwise = error ("error in input data, found invalid hour: " ++ (show h))


showCrashType :: CrashType -> String
showCrashType ct = case ct of 
                   "3" -> "Pedestrian"
                   "6" -> "Bike"
                   _   -> "Other: [" ++ ct ++ "]"

showCollision :: CollisionType -> String
showCollision col = case col of 
                    "1" -> "Angle"
                    "2" -> "Head-On"
                    "3" -> "Rear-End"
                    "4" -> "Sideswipe-meeting"
                    "5" -> "Sideswipe-overtaking"
                    "6" -> "Turning Movement"
                    "7" -> "Parking Movement"
                    "8" -> "Non-collision"
                    "0" -> "Fixed- or Other- object"
                    "-" -> "Backing"
                    "&" -> "Miscellaneous"

showSeverity :: String -> String
showSeverity sev = case sev of
                   "2" -> "Fatal"
                   "4" -> "Injury"
                   "5" -> "Property Damage Only"

