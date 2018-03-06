# #############################################################################
# This script combine and clean the register data used in the swineflu-autism 
# study.
#
# Registers:
#   Medical birth register 1973-2012
# 	Cause of death register 1952-2016
# 	Outpatient register 1997-2016
# 	Inpatient register 1964-2016
#
# Cohort coverage:
# 	The cohort covers 7 health care regions (61% of Swedens population). The
# 	study population consist of:
# 		1. all women pregnant or giving birth 2009-2010 (vaccinated and 
# 		non-vaccinated). 
# 		2. all children and pregnansies of these women up to and including
# 		those in 2009-2010.
# 	In total 424049 unique social security numbers (of which 77 not in use).
#
# Author: Peter Ström (peter.strom@ki.se)
# PI: Jonas Ludvigsson (jonasludvigsson@yahoo.com)
# #############################################################################

# #############################################################################
# Packages
# #############################################################################
wants <- c("tidyverse")
has <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
lapply(wants, require, character.only = TRUE)
rm(wants, has)

# #############################################################################
# Read data
# #############################################################################
codreg <- haven::read_sas("../data/ut_dors_10516_2017.sas7bdat")
outpat <- haven::read_sas("../data/ut_par_ov_10516_2017.sas7bdat")
in_pat <- haven::read_sas("../data/ut_par_sv_10516_2017.sas7bdat")
fg_inf <- haven::read_sas("../data/fg_info.sas7bdat")
mf_reg <- haven::read_sas("../data/mfr.sas7bdat")

moth_comorb_out <- read.csv("../data/oppenvard_comorb.csv")
moth_comorb_in <- read.csv("../data/slutenvard_comorb2.csv")
exc <- read.csv("../data/___.csv")

# #############################################################################
# Remove child ID with NA (still born) from medical birth register.
# #############################################################################
mf_reg = filter(mf_reg, !is.na('lpnr_BARN'))

# #############################################################################
# Join data.
# #############################################################################
# Keep track of excluded subjects.
excluded <- data.frame(Reason = "All in FG", N = dim(fg_inf)[1], 
                       stringsAsFactors = FALSE)
# MFR
dta <- inner_join(fg_inf, select(mf_reg, -lpnr_mor), by='lpnr_BARN')
excluded <- rbind(excluded, list("Not in MFR", dim(dta)[1]))

# Select subset of columns in cohort 
dta <- subset(dta, select=c("lpnr_BARN", "lpnr_mor", "vdat1", "educ",
                            "ethnew", "lan", "dispink", "excl_reason", 
                            "BFODDAT", "KON", "MFODDAT", "GRDBS", 
                            "ROK1", "MVIKT", "MLANGD", "PARITET",
                            "INDATMHV"))

# Dates
dta$BFODDAT <- lubridate::ymd(dta$BFODDAT)
dta$age_at_vacc <- dta$BFODDAT - dta$vdat1

# restrict to 2001-01-01 to 2011-12-31 due to coverage in oppen/slutenvard.
# (email 2015-09-15).
dta <- subset(dta, BFODDAT >= "2001-01-01" & BFODDAT < "2012-01-01") 
excluded <- rbind(excluded, list("Not in 2001_2011", dim(dta)[1]))

# Not eligeble to participate 
dta <- subset(dta, excl_reason == "Eligible", select = -excl_reason)
excluded <- rbind(excluded, list("Not Eligable (FG)", dim(dta)[1]))

# Exclude chromosome caused malformation (Jonas definies these criteria).
dta <- dta[!(dta$lpnr_BARN %in% exc), ]
excluded <- rbind(excluded, list("Chromosome (Jonas exc-file)", dim(dta)[1]))

# Exclude missing pregnancy length GRDBS 
dta <- dta[!is.na(dta$GRDBS), ]
excluded <- rbind(excluded, list("Missing pregnancy days", dim(dta)[1]))

# Exclude missing/unknown sex 
dta <- dta[dta$KON %in% 1:2, ]
excluded <- rbind(excluded, list("Missing/unknown sex", dim(dta)[1]))

