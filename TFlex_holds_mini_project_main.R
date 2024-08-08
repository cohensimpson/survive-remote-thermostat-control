## -----------------------------------------------------------------------------
## Atmospheric Temperature and Consumer Rejection of Remote Thermostat Control
## Replication Code for "TFlex" Mini Project
## 
## Author/Maintainer: Cohen R. Simpson
## -----------------------------------------------------------------------------






## Install Necessary Modules ---------------------------------------------------
## It is recommended that one install the requisite modules using Micromamba/Conda.
## See "TFlex_setup_environment.yml" and "TFlex_requirements.R" in project replication pack.






## Import Necessary Modules ----------------------------------------------------
library(readr)
library(readxl)
library(RSQLite)

library(magrittr)

library(dplyr)
library(tidyr)
library(tidyselect)
library(tibble)
library(forcats)
library(lubridate)
library(glue)
library(fastDummies)

library(rgdal)
library(sp) 
library(KrigR)
library(terra)
library(exactextractr)
library(tigris)
library(humidity)

library(ggplot2)
library(scales)
library(ggh4x)
library(ggridges)
library(svglite)
library(latex2exp)
library(ggmagnify)
library(patchwork)
library(ragg)
library(ggrastr)
library(export)

library(survival)
library(rstanarm)
library(bayesplot)
library(bayestestR)
library(tidybayes)
library(muhaz)
library(survminer)

library(modelsummary)
library(gt)
library(gtsummary)
library(formattable)
library(see)


## DPLYR Print Options
## https://stackoverflow.com/a/77708697
base::options("pillar.print_min" = 50)


## Modelsummary Backend Options
## https://modelsummary.com/reference/modelsummary.html#model-extraction-functions
base::options(modelsummary_get = "all")






## Set Random Seed for Reproducible Results ------------------------------------
## https://stackoverflow.com/a/68008152
## https://www.math.ucla.edu/~anderson/rw1001/library/base/html/Random.html
## https://mc-stan.org/docs/reference-manual/reproducibility.html
random_seed = base::as.integer(216585314)
base::RNGkind(
    kind = "Mersenne-Twister",
    normal.kind = "Inversion", 
    sample.kind = "Rejection"
    )
base::set.seed(random_seed)






## ECMWF Copernicus Data Store ERA5 API ID/KEY ---------------------------------
## Must define "API_User" and "API_Key" before attempting to download the ERA5 data.
## https://www.erikkusch.com/courses/krigr/setup/
## base::source("TFlex_holds_mini_project_process_data_ERA5_geometry_API_credentials.R")


## Obtain and Save ERA5 Weather Data and TX Shapefiles -------------------------
## Downloading these data will take quite some time!
## base::source("TFlex_holds_mini_project_process_data_ERA5_geometry.R")


## Load + Process Data For Model Fitting ---------------------------------------
## https://modelsummary.com/vignettes/datasummary.html
base::source("TFlex_holds_mini_project_process_data.R")


## Create TX Choropleth --------------------------------------------------------
## This stage will take a bit to complete due to generation of the map.
base::source("TFlex_holds_mini_project_process_tx_choropleth.R")


## Estimate the Survival Models ------------------------------------------------
## This stage will take a very long time to complete (Bayesian inference).
base::source("TFlex_holds_mini_project_results_estimate_models.R")


## Create Table of Estimates ---------------------------------------------------
base::source("TFlex_holds_mini_project_results_estimates_tabular.R")


## Assess/Visualise Model Fit --------------------------------------------------
## This stage will take a very long time to complete (Bayesian inference).
## Note, "rstanarm:::posterior_survfit.stansurv" will spit out some harmless warnings.
## See: https://github.com/stan-dev/rstanarm/issues/440#issuecomment-639198936
## This script also calculates posterior probabilities for points along standardised survival curve.
base::source("TFlex_holds_mini_project_results_figure_survfit_basehazfit.R")


## Obtain/Visualised Fitted Values ---------------------------------------------
## This stage will take a very long time to complete (Bayesian inference).
## Note, "rstanarm:::posterior_survfit.stansurv" will spit out some harmless warnings.
## See: https://github.com/stan-dev/rstanarm/issues/440#issuecomment-639198936
## This script also calculates posterior probabilities for points along temperature-specific standardised survival curve.
base::source("TFlex_holds_mini_project_results_figure_survival_fitted_values.R")


## Save Workspace --------------------------------------------------------------
base::save.image("TFlex_holds_mini_project_workspace.RData") 

## Reload Complete Workspace/Environment Used to Complete Research Paper
# base::load("*TFlex_holds_mini_project_workspace (2024-07-04).RData")
