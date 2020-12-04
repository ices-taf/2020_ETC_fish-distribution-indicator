# -------------------------------------
#
# process supplied control file
#
# -------------------------------------

library(icesTAF)

control_file <- read.taf("../Overview_of_surveys_to_be_used.csv", check.names = TRUE)
#control_file <- read.taf("bootstrap/data/Overview_of_surveys_to_be_used.csv", check.names = TRUE)

write.taf(control_file, quote = TRUE)
