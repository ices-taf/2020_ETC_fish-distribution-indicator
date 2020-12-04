

download.file(
  "http://gis.ices.dk/shapefiles/ICES_StatRec_mapto_ICES_Areas.zip",
  "temp.zip")

unzip("temp.zip")
unlink("temp.zip")
