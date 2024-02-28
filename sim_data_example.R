setwd("~/stremr_jso")
source("wide_to_long_format.R")

time_horizon <- 3
names_baseline_covariates <- c("sex","education","agegroups","tertile_income","index_heart_failure","diabetes_duration", "secondline_duration","first_2ndline")
names_time_covariates <- "insulin"
name_treatment <- "GS"
name_outcome <- "heart_failure"
name_censoring <- "Censored"
id_var <- 'pnr'

library(data.table)
dd <- wide_to_long_format(dat = readRDS("dat_example.rds"), 
                          time_horizon = time_horizon, 
                          names_baseline_covariates = names_baseline_covariates, 
                          names_time_covariates = names_time_covariates, 
                          name_treatment = name_treatment, 
                          name_outcome = name_outcome, 
                          name_censoring = name_censoring, 
                          id_var = id_var)
## set interventions for use with stremr
dd[, ("GS.set1") := 1L]
dd[, ("GS.set0") := 0L]


library(stremr)
dd <- importData(dd, 
                 ID = "pnr", 
                 t = "t", 
                 covars = c(names_baseline_covariates, "insulin"), 
                 CENS = "Censored", 
                 TRT = "GS", 
                 OUTCOME = "heart_failure.tplus1")
gform_CENS <- paste0("Censored ~ ", 
                     paste0(c(setdiff(names_baseline_covariates,"pnr"),"GS", "insulin"), collapse = " + "))
gform_TRT <- paste0("GS ~ ", 
                    paste0(c(setdiff(names_baseline_covariates,"pnr"), "insulin"), collapse = " + "))

dd <- fitPropensity(dd,
                       gform_CENS = gform_CENS,
                       gform_TRT = gform_TRT)

tvals <- c(0:2)
Qforms <- rep.int(paste0("Qkplus1 ~ ", paste0(c(setdiff(names_baseline_covariates,"pnr"), "insulin", "GS"), collapse = " + ")), (max(tvals)+1))
tmle_est1 <- fit_TMLE(dd, tvals = tvals, intervened_TRT = "GS.set1", Qforms = Qforms)
tmle_est0 <- fit_TMLE(dd, tvals = tvals, intervened_TRT = "GS.set0", Qforms = Qforms)