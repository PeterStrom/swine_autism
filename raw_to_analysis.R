# #############################################################################
# This script combine and clean the register data used in the swineflu-autism 
# study.
#
# Registers:
# 	*Cancer register 1958-2015
# 	*Basal cell cancer register 2004-2015
# 	Cause of death register 1952-2016
# 	Outpatient register 1997-2016
# 	Inpatient register 1964-2016
# 	Prescription register (1 July 2005)-2016
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
prescr <- haven::read_sas("../data/ut_lmed_10516_2017.sas7bdat")
codreg <- haven::read_sas("../data/ut_dors_10516_2017.sas7bdat")
outpat <- haven::read_sas("../data/ut_par_ov_10516_2017.sas7bdat")
in_pat <- haven::read_sas("../data/ut_par_sv_10516_2017.sas7bdat")
fg_inf <- haven::read_sas("../data/fg_info.sas7bdat")
mf_reg <- haven::read_sas("../data/mfr.sas7bdat")

# #############################################################################
# Remove child ID with NA (still born) from medical birth register.
# #############################################################################
mf_reg = filter(mf_reg, !is.na('lpnr_BARN'))

# #############################################################################
# Join data.
# #############################################################################
dta <- inner_join(fg_inf, select(mf_reg, -lpnr_mor), by='lpnr_BARN')
dta$age_at_vacc <- lubridate::ymd(dta$BFODDAT) - dta$vdat1
any(is.na(lubridate::ymd(dta$BFODDAT)))
summary(lubridate::ymd(dta$BFODDAT))
           