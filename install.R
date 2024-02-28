## these packages are on CRAN
packages <- c(
  "assertthat",
  "condensier",
  "data.table",
  "dplyr",
  "magrittr",
  "methods",
  "origami",
  "pander",
  "purrr",
  "R6",
  "rmarkdown",
  "speedglm",
  "sl3",
  "stats",
  "tibble",
  "tidyr",
  "zoo",
  "ggplot2",
  "SuperLearner",
  "xgboost",
  "devtools",
  "h2o",
  "fastglm"
)

install.packages(packages)

## local "fixed version"
devtools::install_local("gridisl")

## others available on github
devtools::install_github('osofr/condensier', build_vignettes = FALSE)
devtools::install_github('osofr/condensier', build_vignettes = FALSE)
devtools::install_github("cran/imputeMissings",build_vignettes = FALSE)
devtools::install_github("tlverse/sl3",build_vignettes = FALSE)

devtools::install_local("stremr")