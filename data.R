## Preprocess data, write TAF data tables

## Before:
## After:

library(icesTAF)

mkdir("data")

# processing
sourceTAF("data-sst.R")
sourceTAF("data-statrecs.R")
sourceTAF("data-species.R")

# joining together
sourceTAF("data-datras.R")
