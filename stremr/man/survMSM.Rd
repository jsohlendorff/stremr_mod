% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MSM_hazard_time.R
\name{survMSM}
\alias{survMSM}
\title{Estimate Survival with a particular MSM for the survival-hazard function using previously fitted weights.}
\usage{
survMSM(
  wts_data,
  OData,
  tbreaks,
  use_weights = TRUE,
  stabilize = TRUE,
  trunc_weights = 10^6,
  weights = NULL,
  getSEs = TRUE,
  est_name = "IPW",
  glm_package = c("glm", "speedglm", "h2o"),
  return_wts = FALSE,
  tmax = NULL,
  verbose = getOption("stremr.verbose")
)
}
\arguments{
\item{wts_data}{A list of \code{data.table}s, each data set is a result of calling the function
\code{getIPWeights}. Must contain the treatment/monitoring rule-specific estimated IPTW weights.
This argument can be also a single \code{data.table} obtained with \code{data.table::rbindlist(wts_data)}.}

\item{OData}{The object returned by function \code{fitPropensity}. Contains the input dat and the
previously fitted propensity score models for the exposure, monitoring and
right-censoring.}

\item{tbreaks}{The vector of integer (or numeric) breaks that defines the dummy indicators of the
follow-up in the observed data. Used for fitting the parametric (or saturated) MSM for
the survival hazard. See Details.}

\item{use_weights}{Logical value. Set to \code{FALSE} to ignore the weights in \code{wts_data} and
fit a "crude" MSM that does not adjust for the possible confounding due to non-random
assignment of the exposure/censoring and monitoring indicators.}

\item{stabilize}{Set to \code{TRUE} to stabilize the weights by the empirical conditional probability
of having followed the rule at time-point \code{t}, given the subject has followed the rule all the way up to
time-point \code{t}.}

\item{trunc_weights}{Specify the numeric weight truncation value. All final weights exceeding the
value in \code{trunc_weights} will be truncated.}

\item{weights}{Optional \code{data.table} with additional observation-time-specific weights.
Must contain columns \code{ID}, \code{t} and \code{weight}.
The column named \code{weight} is merged back into the original data according to (\code{ID}, \code{t}).}

\item{getSEs}{A logical indicator. Set to \code{TRUE} to evaluate the standard errors for the
estimated survival by using the MSM influence curve.}

\item{est_name}{A string naming the current MSM estimator. Ignored by the current routine but is
used when generating reports with \code{make_report_rmd}.}

\item{glm_package}{Which R package should be used for fitting the weighted logistic regression
model (MSM) for the survival hazard?
Currently available options are \code{"glm"}, \code{"speedglm"} and \code{"h2o"}.
\code{h2o} can provided better performance
when fitting MSM with many observations and large number of time-points.}

\item{return_wts}{Return the data.table with subject-specific IP weights as part of the output.
Note: for large datasets setting this to \code{TRUE} may lead to extremely large object sizes!}

\item{tmax}{Maximum value of the follow-up period.
All person-time observations above this value will be excluded from the MSM model.}

\item{verbose}{Set to \code{TRUE} to print messages on status and information to the console.
Turn this on by default using \code{options(stremr.verbose=TRUE)}.}
}
\value{
MSM estimation results composed of a separate list for each treatment regimen.
Each regimen-specific list contains an item named \code{"estimates"}, which is a data.table
with MSM survival estimates in a column \code{"St.MSM"}. The data.table \code{"estimates"} contains
a separate row for each time-point \code{t}. The \code{"estimates"} also contains the
standard error estimates for MSM survival and the observation-specific influence-curve estimates for
the MSM survival saved in a column named \code{"IC.St"}.
}
\description{
Estimate the causal survival curve for a particular stochastic, dynamic or static intervention on the
treatment/exposure and monitoring processes based on
the user-specified Marginal Structural Model (MSM) for the counterfactual survival function.
}
\section{MSM}{

**********************************************************************
This routine will run the weighted logistic regression using the (possibly-weighted) outcomes from
many regimens, with dummy indicators for each treatment/monitoring
regimen available in \code{wts_data} and each follow-up time interval specified in \code{tbreaks}.
When \code{use_weights = TRUE}, the logistic regression for the survival hazard is weighted by the
\strong{IPW} (Inverse Probability-Weighted or Horvitz-Thompson) estimated weights
in \code{wts_data}. These IPW weights are based on the previously fitted propensity scores (function
\code{fitPropensity}), allowing
adjustment for confounding by possibly non-random assignment to exposure and monitoring and possibly
informative right-censoring.
}

\section{Specifying time-intervals}{

**********************************************************************

\code{tbreaks} is used for defining the time-intervals of the MSM coefficients for estimation of the
survival hazard function.
The first value in \code{tbreaks} defines a dummy variable (indicator) for a fully closed interval,
with each subsequent value in \code{tbreaks} defining a single right-closed time-interval.
For example, \code{tbreaks = c(0,1)} will define the MSM dummy indicators: I(\code{tmin} <= t <= 0 ),
I(0 < t <= 1) and I(1 < t <= \code{tmax}),
where \code{tmin} is the minimum value of the time variable (automatically determined from input weights)
and \code{tmax} is the maximum value of the time variable ( if omitted this will also be automatically
determined from the input weights).

On the other hand \code{tbreaks = c(1)} will define the following (more parametric) MSM dummy
indicators: I(\code{mint} <= t <=1 ) and I(1 < t <= \code{tmax}).
If omitted, the default is to define a saturated (non-parametric) MSM with a separate dummy variable
for every unique period in the observed data.
}

\examples{
#-------------------------------------------------------------------
# Simulated data with informative right-censoring
#-------------------------------------------------------------------
require("data.table")
require("magrittr")
data(OdataCatCENS)
OdataDT <- as.data.table(OdataCatCENS, key=c("ID", "t"))
# Indicator that the person has never been treated in the past:
OdataDT[, "barTIm1eq0" := as.integer(c(0, cumsum(TI)[-.N]) \%in\% 0), by = ID]
OdataDT[, ("N.tminus1") := shift(get("N"), n = 1L, type = "lag", fill = 1L), by = ID]

#-------------------------------------------------------------------
# Regressions for modeling the exposure (TRT)
#-------------------------------------------------------------------
gform_TRT <- "TI ~ CVD + highA1c + N.tminus1"
# Fit a separate model for TRT (stratify) for each of the following subsets:
stratify_TRT <- list(
  TI=c(
       # MODEL TI AT t=0
       "t == 0L",
       # MODEL TRT INITATION WHEN MONITORED
       "(t > 0L) & (N.tminus1 == 1L) & (barTIm1eq0 == 1L)",
       # MODEL TRT INITATION WHEN NOT MONITORED
       "(t > 0L) & (N.tminus1 == 0L) & (barTIm1eq0 == 1L)",
       # MODEL TRT CONTINUATION (BOTH MONITORED AND NOT MONITORED)
       "(t > 0L) & (barTIm1eq0 == 1L)"
      ))

#-------------------------------------------------------------------
# Regressions for modeling the categorical censoring (CENS)
#-------------------------------------------------------------------
gform_CENS <- c("CatC ~ highA1c")
# stratify by time-points (separate model for all t<16 and t=16)
stratify_CENS <- list(CatC=c("t < 16", "t == 16"))

#-------------------------------------------------------------------
# Regressions for modeling the monitoring regimen (MONITOR)
#-------------------------------------------------------------------
# Intercept only model, pooling across all time points t
gform_MONITOR <- "N ~ 1"

#-------------------------------------------------------------------
# Define the counterfactual monitoring regimen of interest
#-------------------------------------------------------------------
# probability of being monitored at each t is 0.1
OdataDT[, "gstar.N" := 0.1]

# Define two dynamic rules: dlow & dhigh
OdataDT <- defineIntervedTRT(OdataDT, theta = c(0,1), ID = "ID", t = "t", I = "highA1c",
                            CENS = "C", TRT = "TI", MONITOR = "N", tsinceNis1 = "lastNat1",
                            new.TRT.names = c("dlow", "dhigh"), return.allcolumns = TRUE)

#-------------------------------------------------------------------
# Import data into stremr object
#-------------------------------------------------------------------
OData <- importData(OdataDT, ID = "ID", t = "t", covars = c("highA1c", "lastNat1"),
                    CENS = "CatC", TRT = "TI", MONITOR = "N", OUTCOME = "Y.tplus1")

# ----------------------------------------------------------------------
# Look at the input data object
# ----------------------------------------------------------------------
print(OData)

# ----------------------------------------------------------------------
# Access the input data
# ----------------------------------------------------------------------
get_data(OData)

#-------------------------------------------------------------------
# Estimate Propensity scores
#-------------------------------------------------------------------
OData <- fitPropensity(OData, gform_CENS = gform_CENS, gform_TRT = gform_TRT,
                      stratify_TRT = stratify_TRT, gform_MONITOR = gform_MONITOR)

#-------------------------------------------------------------------
# Defining weights for two dynamic regimens "dlow" and "dhigh"
#-------------------------------------------------------------------
wts.St.dlow <- getIPWeights(OData, intervened_TRT = "dlow")
wts.St.dhigh <- getIPWeights(OData, intervened_TRT = "dhigh")

# ------------------------------------------------------------------
# Estimate survival with IPW-adjusted MSM for the hazard (logistic model)
# 1. Saturated model for time points 0 to 7
# 2. Smoothing with one hazard coefficient over time-points 8 to 11
# 3. Smoothing with one hazard coefficient over time-points 12 to 15
# ------------------------------------------------------------------
IPW_MSM_res <- survMSM(OData, wts_data = list(dlow = wts.St.dlow, dhigh = wts.St.dhigh),
                      tbreaks = c(1:8,12,16)-1,
                      est_name = "IPAW", getSEs = TRUE)
names(IPW_MSM_res)
# Survival estimates over time
IPW_MSM_res$St
# SE estimates for each time point:
IPW_MSM_res$IC.Var.S.d$dhigh$se.S
IPW_MSM_res$IC.Var.S.d$dlow$se.S
# MSM coefficient fits
IPW_MSM_res$MSM.fit

# ------------------------------------------------------------------
# Generate automatic html report with results of the analysis
# This assumes that pandoc is already installed
# For more information, go to: http://pandoc.org/installing.html
# ------------------------------------------------------------------
\dontrun{
make_report_rmd(OData, MSM = IPW_MSM_res,
  AddFUPtables = TRUE,
  RDtables = get_MSM_RDs(IPW_MSM_res, t.periods.RDs = c(12, 15), getSEs = FALSE),
  WTtables = get_wtsummary(IPW_MSM_res$wts_data,
    cutoffs = c(0, 0.5, 1, 10, 20, 30, 40, 50, 100, 150), by.rule = TRUE),
  file.name = "sim.data.example.fup",
  title = "Custom Report Title",
  author = "Jane Doe",
  y_legend = 0.95)
}
}
\seealso{
\code{\link{fitPropensity}}, \code{\link{getIPWeights}}.
}
