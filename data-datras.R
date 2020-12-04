## Preprocess data, write TAF data tables

## Before:
## After:

source("utilities.R")
detachAllPackages()

library(icesTAF)
library(dplyr)
library(tidyr)
library(sf)

mkdir("data")
source("utilities.R")

# load control file and write out summary table:
data_overview <- read.taf("bootstrap/data/control_file/control_file.csv")

# read in datras data
datras <-
  read.taf(
    "bootstrap/data/datras/datras_data.csv",
    colClasses = c("StatRec" = "character")
  )

# read in sst data by area
sst <-
  read.taf(
    "data/sst.csv"
  )

# read in statsq table from previous step
statrecs <-
  read.taf(
    "data/statrecs.csv",
    colClasses = c("StatRec" = "character")
  )

# read in species info
biogeog <-
  read.taf(
    "data/biogeog.csv"
  )

# join areas onto datras then filter by control file,
# then join sst values and finally
# join this data onto the species affinity data
lusitanian <-
  datras %>%
    left_join(
      statrecs, by = "StatRec"
    ) %>%
    right_join(
      data_overview,
      by = c("Survey" = "Survey.name", "F_CODE" = "Division", "Quarter" = "Quarter")
    ) %>%
    filter(
      Year >= Start.year
    ) %>%
    select(
      -Start.year
    ) %>%
    left_join(
      sst, by = c("Year", "F_CODE")
    ) %>%
    inner_join(
      biogeog, by = c("Valid_Aphia" = "AphiaID")
    )

if (FALSE) {
  # checks
  lusitanian %>%
    filter(
      F_CODE == "4.c"
    ) %>%
    select(
      Survey, Year, Quarter, F_CODE
    ) %>%
    unique()
}

write.taf(lusitanian, dir = "data", quote = TRUE)
