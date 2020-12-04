## Run analysis, write model results

## Before:
## After:

source("utilities.R")
detachAllPackages()

library(icesTAF)
library(dplyr)
library(tidyr)

mkdir("model")

# read in data
lusitanian <-
  read.taf(
    "data/lusitanian.csv",
    colClasses = c("StatRec" = "character")
  )

if (FALSE) {
  # investigate unknown species
  lusitanian %>%
    filter(Biogeo.affinity == "Unknown") %>%
    select(Valid_Aphia, Biogeo.affinity, Species) %>%
    unique()

  # check
  lusitanian %>%
    filter(is.na(Biogeo.affinity)) %>%
    select(Valid_Aphia, Species) %>%
    unique()


  lusitanian %>%
    filter(
      F_CODE == "4.c"
    ) %>%
    select(
      Survey, Year, Quarter, F_CODE
    ) %>%
    unique()

}

# Compute stat square and annual summaries
stsq_results <-
  lusitanian %>%
  group_by(
    Survey, Quarter, Year, StatRec, F_CODE, Lat, Lon, WKT, Biogeo.affinity
  ) %>%
  summarise(
    count = n()
  ) %>%
  ungroup() %>%
  pivot_wider(
    names_from = Biogeo.affinity,
    values_from = count,
    values_fill = list(count = 0)
  ) %>%
  mutate(
    ratio = ifelse(Boreal > 0, Lusitanian / Boreal, 99)
  )

# check
stsq_results %>% filter(StatRec == "41E8" & Year == 2011)

write.taf(stsq_results, dir = "model", quote = TRUE)

# compute annual summaries
annual_results <-
  lusitanian %>%
  select(
    Year, Survey, Quarter, F_CODE, Valid_Aphia, sst, sst1, sst2, Biogeo.affinity
  ) %>%
  unique() %>%
  group_by(
    Year, Survey, Quarter, F_CODE, sst, sst1, sst2,, Biogeo.affinity
  ) %>%
  summarise(
    count = n()
  ) %>%
  ungroup() %>%
  pivot_wider(
    names_from = Biogeo.affinity,
    values_from = count,
    values_fill = list(count = 0)
  ) %>%
  mutate(
    ratio = ifelse(Boreal > 0, Lusitanian / Boreal, 99)
  )

# checks
annual_results %>% filter(grepl("4[.]", F_CODE) & Year == 2011)

write.taf(annual_results, dir = "model", quote = TRUE)
