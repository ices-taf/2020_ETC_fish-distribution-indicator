## Run analysis, write model results

## Before:
## After:


library(icesTAF)

mkdir("model")

sourceTAF("model-summarise.R")
sourceTAF("model-regressions.R")
