wide_to_long_format <- function(dat, 
                                time_horizon, 
                                names_baseline_covariates, 
                                names_time_covariates, 
                                name_treatment, 
                                name_outcome, 
                                name_censoring, 
                                id_var = 'pnr') {
  d <- copy(dat)
  
  time_vars <- c(name_censoring, name_outcome, names_time_covariates, name_treatment)
  time_vars <- paste(rep(time_vars, time_horizon), rep(1:time_horizon, 1, each=length(time_vars)), sep = "_")  
  keep_names <- c("pnr",names_baseline_covariates, paste0(c(names_time_covariates,name_treatment),"_0"), time_vars)
  ## remove Dead and time_covariates from last time point
  keep_names <- keep_names[!grepl(paste0(name_treatment,"_",time_horizon,"|",paste0(paste0(names_time_covariates,"_",time_horizon),collapse="|")),keep_names)]
  d <- d[, ..keep_names]
  for (t in 1:time_horizon){
    d[[paste0(name_censoring,"_", t)]] <- 1-d[[paste0(name_censoring,"_", t)]]
    ## this is to remove repeated observations further down when use na.omit
    for (v in paste0(c(name_censoring, name_outcome), "_", t)){
      ## find the variables that end with _t, ... _time_horizon
      later_nodes <- grep(paste0("_",(t+1):time_horizon,"$",collapse="|"), names(d))
      if(any(has_event <- (d[[v]]%in%1))){
        for(l in later_nodes) {set(d,j=l,i=which(has_event),value=NA )} 
      }
    }
  }
  ## rename id_var to pnr
  setnames(d, old = id_var, new = "pnr")
  for (i in (time_horizon-1):0) {
    for (v in c(names_time_covariates, name_treatment)){
      setnames(d, old = paste0(v,"_",i), new = paste0(v,"_",i+1))
    }
  }
  time_varying_vars <- c(name_censoring, name_outcome, name_treatment, names_time_covariates)
  # Generate combinations
  combinations_time_covariates <- expand.grid(prefix = time_varying_vars, suffix = 1:time_horizon)
  # Apply paste function to combine elements
  names_time_covariates_time <- c("pnr", paste0(combinations_time_covariates$prefix, "_", combinations_time_covariates$suffix))
  d_time_vary <- d[,..names_time_covariates_time]
  dd <- data.table::melt(d_time_vary, 
                         id.vars = "pnr", 
                         measure.vars = lapply(time_varying_vars, function(x) paste0(paste0(x,"_"),1:time_horizon)), 
                         value.name = time_varying_vars, variable.name = 't')
  setnames(dd, old = name_outcome, new = paste0(name_outcome, ".tplus1"))
  ## make a temporary data set where we remove pnr and id from dd
  
  dd <- dd[rowSums(!is.na(dd)) > 2, ] ## remove rows where every variable is missing except for id and t
  setkey(dd,pnr,t)
  dd$N <- 1 ## artificial monitoring indicator
  dd$t <- as.numeric(dd$t)
  dd$t <- dd$t-1
  dd[N==1 & Censored==1, N:=NA]
  names_baseline_covariates <- c("pnr",names_baseline_covariates)
  d_baseline <- d[,..names_baseline_covariates]
  ## merge dd with d_baseline by pnr
  merge(dd,d_baseline,by="pnr")
}