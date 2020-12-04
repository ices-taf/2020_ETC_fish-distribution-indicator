
# -------------------------------------
#
# download datras data files
#
# -------------------------------------

library(icesDatras)
library(icesTAF)
library(dplyr)

# read design table and look download all required surveys
ctab <- read.taf("../control_file/control_file.csv")
#ctab <- read.taf("bootstrap/data/control_file/control_file.csv")

# surveys to get are:
toget <-
  ctab %>%
    select(Survey.name, Quarter, Start.year, Gear) %>%
    unique() %>%
    group_by(Survey.name, Quarter, Gear) %>%
    summarise(Start.year = min(Start.year)) %>%
    as.data.frame()



# datras_fname utility from repo root:
source("../../../utilities.R")

hh_list <- list()
hl_list <- list()

# loop over surveys and download (NOTE final year fixed at 2018)
for (i in seq_len(nrow(toget))) {
  # which survey etc.
  survey <- toget[i, "Survey.name"]
  start.year <- toget[i, "Start.year"]
  end.year <- pmin(2018, max(icesDatras::getSurveyYearList(survey)))
  quarter <- toget[i, "Quarter"]
  gear <- toget[i, "Gear"]

  for (year in start.year:end.year) {
    # within loop short-cut for file name
    fname <- function(what) {
      datras.fname(what, survey, year, quarter)
    }

    # download
    message("---- downloading HH ----")
    hh <- getDATRAS(record = "HH", survey = survey, year = year, quarter = quarter)
    if (!is.null(hh) && !identical(hh, FALSE)) {
      hh <-
        hh %>%
          select(Survey, Quarter, Country, Ship, Gear, SweepLngt,
                 GearExp, DoorType, StNo, HaulNo, Year,
                 StatRec) %>%
          filter(Gear == gear)
      hh_list[[length(hh_list) + 1]] <- hh
    }
    rm(hh); gc()

    message("---- downloading HL ----")
    hl <- getDATRAS(record = "HL", survey = survey, year = year, quarter = quarter)
    if (!is.null(hl) && !identical(hl, FALSE)) {
      hl <-
        hl %>%
          select(Survey, Quarter, Country, Ship, Gear, SweepLngt,
                 GearExp, DoorType, StNo, HaulNo, Year,
                 HLNoAtLngt, Valid_Aphia) %>%
          filter(HLNoAtLngt > 0 & !is.na(Valid_Aphia) & Gear == gear) %>%
          select(-HLNoAtLngt) %>%
          unique()
      hl_list[[length(hl_list) + 1]] <- hl
    }
    rm(hl); gc()
  }
}

# summarise hh and hl data
datras_data <-
  hl_list %>%
    do.call(rbind, .) %>%
    left_join(
      do.call(rbind, hh_list),
      by = c("Survey", "Quarter", "Country", "Ship", "Gear", "SweepLngt",
             "GearExp", "DoorType", "StNo", "HaulNo", "Year")
    ) %>%
    select(
      Survey, Quarter, Year, StatRec, Valid_Aphia
    ) %>%
    unique()

head(datras_data)

write.taf(datras_data, quote = TRUE)
