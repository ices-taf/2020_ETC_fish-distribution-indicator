
download.file(
  "http://gis.ices.dk/shapefiles/ICES_areas.zip",
  "temp.zip")

unzip("temp.zip")
unlink("temp.zip")
