SCHEMA=http://schemas.opengis.net/kml/2.2.0/ogckml22.xsd
OUTDIR:=output
CRASHBASE:= $(OUTDIR)/crashes

all: kml validate 

kml:
	mkdir -p $(OUTDIR)
	runghc crashmapper.hs > $(CRASHBASE).kml
	# now run google earth or xmllint to check the KML file.

validate:
	xmllint --schema $(SCHEMA) $(CRASHBASE).kml

