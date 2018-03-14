# #############################################################################
# This script combine and clean the register dta used in the swineflu-autism 
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
# Read dta
# #############################################################################
codreg <- haven::read_sas("../data/ut_dors_10516_2017.sas7bdat")
outpat <- haven::read_sas("../data/ut_par_ov_10516_2017.sas7bdat")
in_pat <- haven::read_sas("../data/ut_par_sv_10516_2017.sas7bdat")
fg_inf <- haven::read_sas("../data/fg_info.sas7bdat")
mf_reg <- haven::read_sas("../data/mfr.sas7bdat")
moth_comorb_ou <- read.csv("../data/oppenvard_comorb.csv")
moth_comorb_in <- read.csv("../data/slutenvard_comorb2.csv")
load('../data/lpnr_malformation.Rdata')

# #############################################################################
# Remove child ID with NA (still born) from medical birth register.
# #############################################################################
mf_reg = filter(mf_reg, !is.na('lpnr_BARN'))

# #############################################################################
# Join dta.
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
dta$age_at_vacc <- dta$BFODDAT - dta$vdat1  # difference in days 
dta$MFODDAT <- trimws(dta$MFODDAT)
dta$MFODDAT <- ifelse(nchar(dta$MFODDAT)==6, 
                      paste(dta$MFODDAT, "15", sep=""),
                      dta$MFODDAT)
dta$MFODDAT <- lubridate::ymd(dta$MFODDAT) 
dta$Age_mother <- as.numeric((dta$BFODDAT - dta$MFODDAT) / 365.25)
dta$INDATMHV <- ifelse(nchar(dta$INDATMHV) == 4, NA, dta$INDATMHV)
dta$INDATMHV <- lubridate::ymd(dta$INDATMHV) 

# restrict to 2001-01-01 to 2011-12-31 due to coverage in oppen/slutenvard.
# (email 2015-09-15).
dta <- subset(dta, BFODDAT >= "2001-01-01" & BFODDAT < "2012-01-01") 
excluded <- rbind(excluded, list("Not in 2001_2011", dim(dta)[1]))

# Not eligeble to participate 
dta <- subset(dta, excl_reason == "Eligible", select = -excl_reason)
excluded <- rbind(excluded, list("Not Eligable (FG)", dim(dta)[1]))

# Exclude chromosome caused malformation (Jonas definies these criteria)
# and any malformation defined in [Ludvigsson, 2016].
exc_mal <- unique(c(exc, missb))
dta <- dta[!(dta$lpnr_BARN %in% exc_mal), ]
excluded <- rbind(excluded, list("Chromosome + Malformation", dim(dta)[1]))

# Exclude missing pregnancy length GRDBS 
dta <- dta[!is.na(dta$GRDBS), ]
excluded <- rbind(excluded, list("Missing pregnancy days", dim(dta)[1]))

# Exclude missing/unknown sex 
dta <- dta[dta$KON %in% 1:2, ]
excluded <- rbind(excluded, list("Missing/unknown sex", dim(dta)[1]))

# Calculate excluded for each step of exclusion.
excluded$removed <- lag(excluded$N) - excluded$N

# #############################################################################
# Create indicator for comorbidity in the mother.
# Join the two tables from Jonas (2015-07-02), use earliest date, and then 
# attach a mothers comorbidity variable to the cohort. subset all lpnr who 
# has at least one date < vac.period start.
# #############################################################################
moth_comorb <- rbind(moth_comorb_in[, c("lpnr", "INDATUM")],
                     moth_comorb_ou[, c("lpnr", "INDATUM")])
moth_comorb$INDATUM <- lubridate::mdy(moth_comorb$INDATUM)
moth_comorb <- moth_comorb[!is.na(moth_comorb$INDATUM), ]
moth_comorb <- moth_comorb %>% 
                  group_by(lpnr) %>%
                  summarise(INDATUM = min(INDATUM)) %>%
                  ungroup()
moth_comorb <- filter(moth_comorb, INDATUM < as.Date("20091001", "%Y%m%d"))
moth_comorb <- moth_comorb$lpnr
dta$moth_comorb <- dta$lpnr_mor %in% moth_comorb

# #############################################################################
# Dates
# #############################################################################
## Remove obviously wrong values on vdat1
tmp <- dta$vdat1 >= as.Date("2009-10-01")
tmp[is.na(tmp)] <- TRUE
dta <- dta[tmp, ]
excluded <- rbind(excluded, list("Wrong vaccin date", dim(dta)[1]))

# #############################################################################
# Create variables
# #############################################################################
dta$BMI <- dta$MVIKT/(dta$MLANGD/100)^2

dta$Paritet_cat <- ifelse(dta$PARITET >=3, "3+", dta$PARITET)
dta$Ethnicity <- ifelse(dta$ethnew == 1, "Swe", "NoSwe")
dta$Dispink_cat <- ifelse(dta$dispink < 200000, "<200000", ">=200000")

dta$MotherAgeAtBirth <- cut(as.numeric(dta$Age_mother), 
                             breaks = c(0, 25, 30, 35, 200),
                             labels=c("<25", "25 - 29", "30-34", ">35"))
dta$BMI <- cut(dta$BMI,
                breaks = c(0, 18.5, 25, 30, 200),
                labels=c("<18.5", "18.5 - <25", "25 - <30", ">30"))
dta$Sex <- as.factor(dta$KON)
levels(dta$Sex) <- c("Male", "Female")
dta$SmokingAtFirstVisit <- as.factor(dta$ROK1)
levels(dta$SmokingAtFirstVisit) <- c("Non-smoker", "1-9 cig/day", "10+ cig/day")
dta$SmokingAtFirstVisit2 <- dta$ROK1
dta$SmokingAtFirstVisit2[dta$SmokingAtFirstVisit2 %in% 2:3] <- "Smoker"
dta$SmokingAtFirstVisit2[dta$SmokingAtFirstVisit2 == 1] <- "Non-smoker"
dta$Ethnicity <- as.factor(dta$Ethnicity)
dta$DisposableIncome <- as.factor(dta$Dispink_cat)
dta$PostHighSchool <- dta$educ
dta$PostHighSchool[dta$PostHighSchool %in% 1:3] <- "No"
dta$PostHighSchool[dta$PostHighSchool %in% 4:6] <- "Yes"
dta$PostHighSchool[dta$PostHighSchool == 9] <- NA

# Vaccination and dates (14 and 8 are including last week)
# According to Sven C (mail 2015-10-02) should first trimester be 97 days = 13w + 6 days
dta$Impregnation <- dta$BFODDAT - dta$GRDBS
dta$Week14 <- dta$Impregnation + 7*13 + 6
dta$Week22 <- dta$Impregnation + 7*22

dta$VaccFull <- ifelse(is.na(dta$vdat1 ) | 
                          dta$vdat1 < dta$Impregnation | 
                          dta$vdat1 > dta$BFODDAT, 0, 1)

dta$Vacc14 <- ifelse(is.na(dta$vdat1 ) | 
                        dta$vdat1 < dta$Impregnation | 
                        dta$vdat1 > dta$Week14, 0, 1)

dta$Vacc22 <- ifelse(is.na(dta$vdat1 ) |
                          dta$vdat1 < dta$Impregnation |
                          dta$vdat1 > dta$Week22, 0, 1)

dta$BirthYear <- lubridate::year(dta$BFODDAT)

