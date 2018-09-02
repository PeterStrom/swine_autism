# #############################################################################
# This script produce the tables in the article.
#
# Table 1 is descriptives of the cohort.
# Table 2 is HR for population study.
# Table 3 is HR for sibling study.
#
# Table 2 and 3 are also done for each of the subanalyses.
#
# Author: Peter Ström (peter.strom@ki.se)
# PI: Jonas Ludvigsson (jonasludvigsson@yahoo.com)
# #############################################################################

# #############################################################################
# Subset data.
#
# For population analysis all children should be born during vaccinaion period
# defined by 'datum_start' and 'datum_end'.
#
# For sibling study there should be at least two children per mother and at
# least one exposed and one non-exposed. Also, because of condtitional 
# analyses there must be at least one event for each family.
# #############################################################################

# Population cohort.
dta_pop <- subset(dta_ICD10, BFODDAT >= datum_start & BFODDAT <= datum_end)

# Sibling cohort.
# Families non-discordant on exposure still contribute to estimation of
# adjusting variables in the sibling analysis but are not considered for
# table 1 on sibling data.
dta_sib <- dta_ICD10 %>%
  group_by(lpnr_mor) %>%
  filter(n_distinct(VaccFull) == 2) %>%  # this line is excluded in analyses.
  filter(any(event)) %>%
  ungroup()

# #############################################################################
# Table 1.
#
# Using the package 'tableone' by Max Gordon.
# #############################################################################
vars <- c("MotherAgeAtBirth",
          "BMI",
          "Sex",
          "Paritet_cat",
          "SmokingAtFirstVisit",
          "Ethnicity",
          "DisposableIncome",
          "event",
          "exit_age")

table1 <- lapply(list(dta_ICD10, dta_pop, dta_sib),
                 function(d){
                   CreateTableOne(data=d,
                                  vars=vars,
                                  factorVars=vars[1:8],
                                  strata=c("VaccFull"),
                                  includeNA = TRUE)
                 })

t1_1 <- print(table1[[1]], showAllLevels = TRUE, noSpaces = TRUE,
              printToggle = FALSE)
t1_2 <- print(table1[[2]], showAllLevels = TRUE, noSpaces = TRUE,
              printToggle = FALSE)
t1_3 <- print(table1[[3]], showAllLevels = TRUE, noSpaces = TRUE,
              printToggle = FALSE)

# #############################################################################
# Table A1.
#
# Using the package 'tableone' by Max Gordon.
# #############################################################################
vars <- c("MotherAgeAtBirth",
          "BMI",
          "Sex",
          "Paritet_cat",
          "SmokingAtFirstVisit",
          "Ethnicity",
          "DisposableIncome",
          "VaccFull",
          "Vacc22",
          "Vacc14",
          "exit_age")

table1 <- lapply(list(dta_ICD10, dta_pop, dta_sib),
                 function(d){
                   CreateTableOne(data=d,
                                  vars=vars,
                                  factorVars=vars[1:10],
                                  strata=c("event"),
                                  includeNA = TRUE)
                 })

tA1_1 <- print(table1[[1]], showAllLevels = TRUE, noSpaces = TRUE,
              printToggle = FALSE)
tA1_2 <- print(table1[[2]], showAllLevels = TRUE, noSpaces = TRUE,
              printToggle = FALSE)
tA1_3 <- print(table1[[3]], showAllLevels = TRUE, noSpaces = TRUE,
              printToggle = FALSE)

# #############################################################################
# Table 2 and 3.
# #############################################################################
analysis <- function(dta, sib, vacc_period){
  # Cox estimated hazard ratios for autism by swine flu vaccine during
  # pregnancy.
  # 
  # Args:
  #   dta: (data) dta_ICD10 or dta_F84_0.
  #   sib: (bool) if TRUE sibling analysis else cohort analysis.
  #   vacc_period: exposure column in dta.
  #
  # Return: N, events (%), Crude HR (95% CI), Adjusted HR (95% CI), p(prop_haz).
  
  if (sib){
    d <- dta %>%
      group_by(lpnr_mor) %>%
      filter(any(event))

    if (vacc_period == 'VaccFull')    d <- d %>% filter(n_distinct(VaccFull) == 2)
    if (vacc_period == 'Vacc22')    d <- d %>% filter(n_distinct(Vacc22) == 2)
    if (vacc_period == 'Vacc14')    d <- d %>% filter(n_distinct(Vacc14) == 2)

    d <- d %>% ungroup()
    
    crude <- paste(vacc_period, "strata(lpnr_mor)", sep = " + ")
    adjust <- paste(vacc_period, "SmokingAtFirstVisit", "MotherAgeAtBirth",
                    "Paritet_cat", "Sex", "BMI", "strata(lpnr_mor)", sep = " + ")
  } else {
    d <- subset(dta, BFODDAT >= datum_start & BFODDAT <= datum_end)
    
    crude <- vacc_period
    adjust <- paste(vacc_period, "DisposableIncome", "SmokingAtFirstVisit",
                    "Sex", "MotherAgeAtBirth", "Ethnicity", "Paritet_cat",
                    "BMI", sep = " + ")
  }
  
  # Create helper functions 
  
  # ...for formating numbers.
  fn <- function(x, dig){
    format(round(x, dig), nsmall = dig)
  }
  
  # ...for estimating HR
  HR <- function(adjusted) {
    if (adjusted){
      formula <- as.formula(paste("Surv(exit_age, event)", adjust, sep = " ~ "))
    } else {
      formula <- as.formula(paste("Surv(exit_age, event)", crude, sep = " ~ "))
    }
    
    fit <- coxph(formula, d)
    
    # N
    N <- fit$n
    
    # Events
    events <- fit$nevent
    events <- paste0(events, " (", fn(events / N * 100, 1), ")")
    
    # PH assumption
    ph_assump <- fn(cox.zph(fit)$table[vacc_period, "p"], 3) 
    
    # HR (95% CI)
    fit <- summary(fit)$conf.int
    
    if (adjusted){
      fit <- fit[1, ]
    }
    
    hr <- paste0(fn(fit[1], 2),
                 " (",
                 fn(fit[3], 2),
                 " - ", 
                 fn(fit[4], 2),
                 ")")
    
    return(c(N, events, hr, ph_assump))
  }
  
  # Get estimates of HR
  adj <- HR(adjusted=TRUE)
  cru <- HR(adjusted=FALSE)[3]
  
  # Return a data.frame
  df <- data.frame(Exposure = vacc_period,
                   N = adj[1],
                   Events = adj[2],
                   HR_crude = cru,
                   HR_adjust = adj[3],
                   p_phassum = adj[4])
  return(df)
} 

expos <- list('VaccFull', 'Vacc14', 'Vacc22')

table_HR <- function(subanalysis, sib){
  # Generate table 2 and 3: HR for poplation and sibling analyses
  # 
  # Args:
  #   subanalysis:
  #     0. Main analysis
  #     1. Remove all mothers with autism (defined by the same ICD-10 codes as
  #        for the offsprings) or severe comorbidity (the same definition as
  #        in [Ludvigsson, 2016]).
  #     2. Only consider one ICD-10 code (F84.0 “infant autism”)
  #     3. Remove the three counties (Kalmar, Värmland, and Norrbotten) with
  #        somewhat uncertain vaccine data.
  #   sib: (bool) if TRUE sibling analysis else cohort analysis.
  # 
  # Return: Table 2 or 3 depending on argument 'sib'.
  if (subanalysis == 0){
    dta_ana <- dta_ICD10
  }
  
  if (subanalysis == 1){
    dta_ana <- dta_ICD10 %>%
      filter(!moth_comorb)
  }
  
  if (subanalysis == 2){
    dta_ana <- dta_F84_0
  }
  
  if (subanalysis == 3){
    dta_ana <- dta_ICD10 %>%
      filter(!(lan %in% c(8, 17, 25)))
  }
  
  l <- lapply(expos,
              function(x){
                analysis(dta=dta_ana, sib=sib, vacc_period=x)
              })
  return(bind_rows(l))
}

# Generate table 2 and subanalyses
t2_0 <- table_HR(subanalysis = 0, sib = FALSE)
t2_1 <- table_HR(subanalysis = 1, sib = FALSE)
t2_2 <- table_HR(subanalysis = 2, sib = FALSE)
t2_3 <- table_HR(subanalysis = 3, sib = FALSE)

# Generate table 3 and subanalyses
t3_0 <- table_HR(subanalysis = 0, sib = TRUE)
t3_1 <- table_HR(subanalysis = 1, sib = TRUE)
t3_2 <- table_HR(subanalysis = 2, sib = TRUE)
t3_3 <- table_HR(subanalysis = 3, sib = TRUE)

# #############################################################################
# Write tables to disk..
# #############################################################################
# Save to a CSV file
write.csv(t1_1, file = "../output/Table1.csv")
write.csv(t1_2, file = "../output/Table1_pop.csv")
write.csv(t1_3, file = "../output/Table1_sib.csv")
write.csv(tA1_1, file = "../output/Table1A.csv")
write.csv(tA1_2, file = "../output/Table1A_pop.csv")
write.csv(tA1_3, file = "../output/Table1A_sib.csv")

# Write table 2
write.xlsx(t2_0, "../output/Table2.xlsx", sheetName = "Main analysis",
           row.names = FALSE)
write.xlsx(t2_1, "../output/Table2.xlsx", sheetName = "Mother comorb",
           row.names = FALSE, append = TRUE)
write.xlsx(t2_2, "../output/Table2.xlsx", sheetName = "Only F84.0",
           row.names = FALSE, append = TRUE)
write.xlsx(t2_3, "../output/Table2.xlsx", sheetName = "County",
           row.names = FALSE, append = TRUE)

# Write table 3
write.xlsx(t3_0, "../output/Table3.xlsx", sheetName = "Main analysis",
           row.names = FALSE)
write.xlsx(t3_1, "../output/Table3.xlsx", sheetName = "Mother comorb",
           row.names = FALSE, append = TRUE)
write.xlsx(t3_2, "../output/Table3.xlsx", sheetName = "Only F84.0",
           row.names = FALSE, append = TRUE)
write.xlsx(t3_3, "../output/Table3.xlsx", sheetName = "County",
           row.names = FALSE, append = TRUE)

# Write excluded
write.xlsx(excluded, "../output/excluded.xlsx", row.names = FALSE)

# #############################################################################
# Kaplan-Meier Plot.
# #############################################################################
a <- survfit(Surv(exit_age, event) ~ strata(VaccFull), data = dta_pop)
b <- ggsurvplot(a,
                size=.5,
                ylim=c(.985, 1),
                conf.int = TRUE,
                censor = FALSE,
                legend.title = "H1N1 vaccine during pregnancy:",
                legend.labs = c("No", "Yes"),
                xlab="Age",
                title="Autism among children born during the Swine flu epidemic")

ggsave(file="../output/KaplanMeier.pdf", plot=print(b), device="pdf", height=6, width=10)
