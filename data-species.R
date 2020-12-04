
source("utilities.R")
detachAllPackages()

library(icesTAF)
library(dplyr)
library(tidyr)

# read species aphia code table
worms <- read.taf("bootstrap/data/ecotaxanomic.csv")

# read in species groupings
reclaim <- read.taf("bootstrap/data/Reclaim_species_grouping.csv")

# read in datras data
datras <-
  read.taf(
    "bootstrap/data/datras/datras_data.csv",
    colClasses = c("StatRec" = "character")
  )

# subset to only keep those species observed
# join affinity info to aphia code
biogeog <-
  as.data.frame.table(
    table(AphiaID = datras$Valid_Aphia),
    responseName = "nObs",
    stringsAsFactors = FALSE
  ) %>%
  mutate(AphiaID = as.integer(AphiaID)) %>%
  left_join(
    worms %>%
      select(
        ScientificName, AphiaID, Phylum, Class, Order,
        Family, Genus, Species, Vernacular
      ) %>%
      unique(), # required as two haddocks!?
    by = "AphiaID"
  ) %>%
  left_join(
    reclaim %>%
      select(
         Species,
         #Common.name,
         Biogeographical.affinity
      ),
      by = "Species"
  ) %>%
  select(
    AphiaID, Family, Species,
    Vernacular,
    Phylum,
    Biogeographical.affinity, nObs
  ) %>%
  filter(
    Phylum == "Chordata" &
    !is.na(Phylum)
  ) %>%
  select(-Phylum) %>%
  mutate(
    Biogeographical.affinity =
      ifelse(is.na(Biogeographical.affinity), "Unknown", Biogeographical.affinity)
  ) %>%
  arrange(Family) %>%
  rename(Biogeo.affinity = Biogeographical.affinity)


# write out
write.taf(biogeog, dir = "data", quote = TRUE)

if (FALSE) {
  # how manay observations are of unknown affinity
  biogeog %>%
    group_by(Biogeo.affinity) %>%
    summarise(count = sum(nObs))

  # deal with some special cases ?
  biogeog %>% filter(Family == "Lophiidae")
}
