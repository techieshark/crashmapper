module KML (kmlHeader, 
            kmlFooter, 
            Placemark(Placemark), 
            xmlify
           ) where

-- KML required at the top of the file
kmlHeader :: String
kmlHeader = unlines 
          [
            "<kml xmlns=\"http://www.opengis.net/kml/2.2\">",
            "<Folder>",
            "<name>Bike/Ped Crash Locations</name>" 
          ]

--placemarks go between header and footer
	
-- KML required at the bottom of the file
kmlFooter :: String
kmlFooter = unlines 
            [
              "</Folder>",
              "</kml>"
            ]

-- a Placemark holds the fields needed for a KML placemark
data Placemark = Placemark { name::String, 
                             address::String, 
                             description::String,
                             timestamp::String }

-- give the KML for a Placemark w/ included fields.
instance Show Placemark where
    show (Placemark name address description timestamp) = 
	"\t<Placemark>\n" ++ 
	"\t\t<name>" ++ name  ++ "</name>\n" ++
	"\t\t<address>" ++ address ++ "</address>\n" ++
	"\t\t<description>" ++ description ++ "</description>\n" ++
	"\t\t<TimeStamp><when>" ++ timestamp ++ "</when></TimeStamp>\n" ++
	"\t</Placemark>\n"


-- convert text to XML-valid text
-- XXX / BUG. currently this deals with the minimal amount of 
-- conversions necessary to make crashmapper work (namely, "&").
xmlify :: String -> String
xmlify [] = []
xmlify (c:cs)
    | c == '&' = "&amp;" ++ xmlify cs
    | otherwise = [c] ++ xmlify cs
-- TODO: consider using pattern matching here:
-- xmlify ('&':cs) = ...
-- xmlify (  c:cs) = ...
