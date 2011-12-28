# Crashmapper - Mapping crashes in Oregon

This was originally done as a coding project for a Functional Languages class at Portland State University.
Seems pretty useful though, so it is now available for general use.

## Requirements

The code was developed and tested with the following:

* 'pandoc' for creating documentation
* 'ghc' (Glasgow Haskell Compiler) 

## Documentation

Run `make report` to build the report.html file.

## Creating the map file

Run `cd code; make kml` to produce the mapfile at `code/output/krashes.kml`.

## Viewing the map

Load `code/output/krashes.kml` in [Google Earth](http://earth.google.com "Download Google Earth").

## Contributors 

Cody by Peter W (@techieshark). Data from the [ODOT Crash Analysis & Reporting Unit](http://www.oregon.gov/ODOT/TD/TDATA/car/CAR_Main.shtml).

## License

Provided under the GPLv3.
