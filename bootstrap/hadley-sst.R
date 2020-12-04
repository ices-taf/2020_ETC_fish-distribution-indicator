
# download gridded netcdf SST data from UK Met office
library(icesTAF)

url <- "https://www.metoffice.gov.uk/hadobs/hadsst4/data/netcdf/"
download(paste0(url, "HadSST.4.0.0.0_median.nc"))

