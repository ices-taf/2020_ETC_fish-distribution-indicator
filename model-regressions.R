## Run analysis, write model results

## Before:
## After:

source("utilities.R")
detachAllPackages()

library(icesTAF)
library(Kendall)
library(dplyr)

# read annual data
annual_results <-
  read.taf(
    "model/annual_results.csv"
  )

# run a Kendall test

sst_regressions <-
  annual_results %>%
    mutate(
      ratio = ifelse(ratio == Inf, 30, ratio)
    ) %>%
    group_by(
      Survey, Quarter, F_CODE
    ) %>%
    summarise(
      pval = Kendall::Kendall(sst, ratio)$sl,
      pval1 = Kendall::Kendall(sst1, ratio)$sl,
      pval2 = Kendall::Kendall(sst2, ratio)$sl
    ) %>%
    ungroup() %>%
    arrange(F_CODE) %>%
    select(-Survey, -Quarter)

write.taf(sst_regressions, dir = "model", quote = TRUE)
