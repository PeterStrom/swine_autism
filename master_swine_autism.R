# #############################################################################
# Master script for the study of swine flu (H1N1) vaccine and autism.
#
# Author: Peter Ström (peter.strom@ki.se)
# PI: Jonas Ludvigsson (jonasludvigsson@yahoo.com)
# #############################################################################

# #############################################################################
# Packages
# #############################################################################
wants <- c("tidyverse", "survival", "survminer", "tableone", "xlsx")
has <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
lapply(wants, require, character.only = TRUE)
rm(wants, has)

# #############################################################################
# Dates
# #############################################################################
datum_start <- as.Date("2009-10-01")
datum_end <- as.Date("2011-10-01")
start_outpat <- as.Date("2001-01-01")
datum_exit <- as.Date("2016-12-31")

# #############################################################################
# Create analysis data from register data
# #############################################################################
source("raw_to_analysis.R")

# #############################################################################
# Do the analyses and create the tables and plot for publication
# #############################################################################
source("tables.R")
