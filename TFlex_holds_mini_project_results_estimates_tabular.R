## Create Table of Parameter Estimates with Modelsummary -----------------------
## https://modelsummary.com/vignettes/modelsummary.html






## Create List of Stanfit Objects to Pass to use to Build Table ----------------
model_objects_as_stanfits = base::lapply(
    X = base::rev(model_objects), 
    FUN = function(mod){mod[["stanfit"]]}
    )

## https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/rev
base::names(model_objects_as_stanfits) = base::rev(c("HVAC Aware", "HVAC Naïve"))






## Relabel + Define Order of all Parameters to be Displayed in Table -----------
## https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/Quotes
model_objects_coefs_pretty_mapping_all = c(
      `(Intercept)` = "Intercept"
    
    , `weibull-shape` = "Weibull Shape Parameter Gamma"
    
    , `hvac_modeAUTO` = "HVAC Mode = Auto (Ref. = Cool)"
    , `hvac_modeHEAT` = "HVAC Mode = Heat (Ref. = Cool)"
    , `hvac_modeOFF`  = "HVAC Mode = Off (Ref. = Cool)"
    
    , `starting_temperature_Z` = "Int. Temp. ($^{\\circ}$F) 60 Min. Prior to Event Hour (Z-Score)" 
    , `I(starting_temperature_Z^2)` = "Int. Temp. ($^{\\circ}$F) 60 Min. Prior to Event Hour (Z-Score)$^2$"
    , `hvac_modeAUTO:starting_temperature_Z` = "HVAC = Auto $\\times$ Int. Temp. ($^{\\circ}$F) at Event Start (Z-Score)" 
    , `hvac_modeHEAT:starting_temperature_Z` = "HVAC = Heat $\\times$ Int. Temp. ($^{\\circ}$F) at Event Start (Z-Score)" 
    , `hvac_modeOFF:starting_temperature_Z`  = "HVAC = Off $\\times$ Int. Temp. ($^{\\circ}$F) at Event Start (Z-Score)"
    
    , `mean_2m_temperature_tminusoneF_Z` = "Ext. Temp. ($^{\\circ}$F) 60 Min. Prior to Event Hour (Z-Score)"
    , `I(mean_2m_temperature_tminusoneF_Z^2)` = "Ext. Temp. ($^{\\circ}$F) 60 Min. Prior to Event Hour (Z-Score)$^2$"
    , `hvac_modeAUTO:mean_2m_temperature_tminusoneF_Z` = "HVAC = Auto $\\times$ Ext. Temp. ($^{\\circ}$F) 60 Min. Prior to Event Hour (Z-Score)"
    , `hvac_modeHEAT:mean_2m_temperature_tminusoneF_Z` = "HVAC = Heat $\\times$ Ext. Temp. ($^{\\circ}$F) 60 Min. Prior to Event Hour (Z-Score)"
    , `hvac_modeOFF:mean_2m_temperature_tminusoneF_Z`  = "HVAC = Off $\\times$ Ext. Temp. ($^{\\circ}$F) 60 Min. Prior to Event Hour (Z-Score)" 
    
    , `starting_temperature_Z:mean_2m_temperature_tminusoneF_Z` = "Int. Temp. ($^{\\circ}$F) (Z-Score) $\\times$ Ext. Temp. ($^{\\circ}$F) (Z-Score)"
    
    , `mean_2m_dewpoint_temperature_tminusoneF_Z` = "Ext. Dew Point ($^{\\circ}$F) 60 Min. Prior to Event Hour (Z-Score)"
    , `I(mean_2m_dewpoint_temperature_tminusoneF_Z^2)` = "Ext. Dew Point ($^{\\circ}$F) 60 Min. Prior to Event Hour (Z-Score)$^2$"
    
    , `desired_cool_Z` = "Desired Int. Temp. ($^{\\circ}$F) - Cooling (Z-Score)"
    , `hvac_modeAUTO:desired_cool_Z` = "HVAC = Auto $\\times$ Des. Int. Temp. ($^{\\circ}$F) - Cooling (Z-Score)"
    , `hvac_modeHEAT:desired_cool_Z` = "HVAC = Heat $\\times$ Des. Int. Temp. ($^{\\circ}$F) - Cooling (Z-Score)"
    , `hvac_modeOFF:desired_cool_Z`  = "HVAC = Off $\\times$ Des. Int. Temp. ($^{\\circ}$F) - Cooling (Z-Score)"
    
    , `desired_heat_Z` = "Desired Int. Temp. ($^{\\circ}$F) - Heating (Z-Score)"
    , `hvac_modeAUTO:desired_heat_Z` = "HVAC = Auto $\\times$ Des. Int. Temp. ($^{\\circ}$F) - Heating (Z-Score)"
    , `hvac_modeHEAT:desired_heat_Z` = "HVAC = Heat $\\times$ Des. Int. Temp. ($^{\\circ}$F) - Heating (Z-Score)"
    , `hvac_modeOFF:desired_heat_Z`  = "HVAC = Off $\\times$ Des. Int. Temp. ($^{\\circ}$F) - Heating (Z-Score)"
    
    ## Intra-Day History of Load Shifting
    , `number_of_holds_any_length_earlier_on_same_day_Z` = "Number of Holds (Any Length) Earlier On Event Day (Z-Score)"
    , `hold_tenure_Z` = "Hold Tenure at Event Issuance (Z-Score)"
    
    ## Customer Characteristics (Fundamental)
    , `octopus_tenure_Z` = "OEUS Tenure at Event Issuance (Z-Score)"
    , `total_hourly_consumption_tminusone_Z` = "Total Consumption (kWh) 60 Min. Prior to Event Hour (Z-Score)"
    
    , `ERCOT_load_zoneNorth` = "ERCOT Load Zone = North (Ref. Houston)"
    , `ERCOT_load_zoneSouth` = "ERCOT Load Zone = South (Ref. Houston)"
    , `ERCOT_load_zoneWest` = "ERCOT Load Zone = West (Ref. Houston)"
    
    
    ## Standard Deviations of Varying Intercepts 
    ## "modelsummary::modelsumamry" does not pick up the variance parameters.
    , `Sigma[account_number:(Intercept),(Intercept)]` = "St. Dev. of Varying Intercepts: Account Number"
    , `Sigma[hold_event_report_date:(Intercept),(Intercept)]` = "St. Dev. of Varying Intercepts: Calendar Day of Event"
    , `Sigma[hour_of_day:(Intercept),(Intercept)]` = "St. Dev. of Varying Intercepts: Hour of Day of Event"
    )






## Manually Build Custom Rows to Append to Bottom of Table ---------------------
## "modelsummary::modelsummary" is particular about how rows are manually added.
    # See: https://modelsummary.com/articles/modelsummary.html#add_rows

model_objects_extra_rand_eff_stdevs_modelsummary_format = (
    ## First, make a list of the posterior draws for the variance parameters from each model.
    ## Summoned draws should be in order of RUN: "base::names(base::rev(model_objects))"
    ## http://mjskay.github.io/tidybayes/articles/tidy-rstanarm.html
    ## http://mjskay.github.io/tidybayes/articles/tidy-posterior.html
    ## http://mjskay.github.io/tidybayes/reference/summarise_draws.grouped_df.html
    base::list(
        `HVAC Naïve` = tidybayes::spread_draws(
            model = model_objects[["spec_two"]][["stanfit"]]
            , `Sigma[account_number:(Intercept),(Intercept)]`
            , `Sigma[hold_event_report_date:(Intercept),(Intercept)]`
            , `Sigma[hour_of_day:(Intercept),(Intercept)]`
            ) %>% dplyr::select( !c(".chain", ".iteration", ".draw")) 
        
        , `HVAC Aware` = tidybayes::spread_draws(
            model = model_objects[["spec_one"]][["stanfit"]]
            , `Sigma[account_number:(Intercept),(Intercept)]`
            , `Sigma[hold_event_report_date:(Intercept),(Intercept)]`
            , `Sigma[hour_of_day:(Intercept),(Intercept)]`
            ) %>% dplyr::select( !c(".chain", ".iteration", ".draw"))
        )
    
    ## Second, transform draws to get the posterior modes and credible intervals. 
    %>% base::lapply(
        FUN = function(draws){
            (
                ## https://easystats.github.io/parameters/reference/model_parameters.stanreg.html
                ## https://github.com/easystats/bayestestR/issues/549
                parameters::model_parameters(
                    model = draws,
                    as_draws = TRUE,
                    fmt = 3,
                    
                    ## https://easystats.github.io/parameters/reference/model_parameters.stanreg.html
                    ## https://easystats.github.io/bayestestR/reference/point_estimate.html
                    centrality = c("mode"),
                    
                    ## Highest Density/Credible Interval == Continuous; Shortest Probability Interval (SPI)
                    ## https://easystats.github.io/bayestestR/articles/credible_interval.html
                    ## https://easystats.github.io/bayestestR/reference/spi.html
                    ## https://statmodeling.stat.columbia.edu/2020/06/29/shortest-posterior-intervals/
                    ci = 0.95,
                    ci_method = "spi", 
                    
                    test = NULL
                    ) 
                %>% tibble::tibble()
                )
            }
        )
    
    ## Third, rename, format, and pivot posterior summary to meet the requirements of "modelsummary::modelsummary". 
    ## https://tidyr.tidyverse.org/reference/pivot_wider.html#see-also
    %>% dplyr::bind_rows(.id = "specification")
    %>% dplyr::rename(term = "Parameter")
    %>% dplyr::mutate(
        ## "modelsummary::modelsummary" processes manual rows in a "dumb/lazy" manner. Map names here.
        term = model_objects_coefs_pretty_mapping_all[term], 
        mode = base::sprintf(fmt = "%.2f", Mode),
        hdi = base::paste0("[", base::sprintf(fmt = "%.2f", CI_low), ", ", base::sprintf(fmt = "%.2f", CI_high), "]"),
        PD = "—"
        )
    %>% dplyr::select(specification, term, mode, hdi, PD)
    
    ## https://stackoverflow.com/a/71161511
    %>% tidyr::pivot_wider(names_from = c(specification), values_from = c(mode, hdi, PD), names_vary = "slowest")
    )






## Put Everything Together to Make the Table of Results ------------------------
## https://modelsummary.com/vignettes/appearance.html
## https://modelsummary.com/articles/appearance.html
## https://modelsummary.com/reference/modelsummary.html

base::options(modelsummary_format_numeric_latex = "plain")
model_objects_pretty_results = modelsummary::modelsummary(
    
    title = "Bayesian Survival Models of Time Until Override of OEUS Thermostat Holds (Oct. 2022 - May. 2024)",
    
    output = "latex",
    escape = FALSE,
    
    models = model_objects_as_stanfits,
    
    align = "lrrrrrr",
    estimate  = c("Mode" = "mode"), ## Posterior Median
    statistic = c("95\\% HDI" = "[{conf.low}, {conf.high}]", "PD" = "pd"),
    shape = term ~ model + statistic,
    
    ## https://modelsummary.com/reference/get_estimates.html
    ## https://easystats.github.io/parameters/reference/model_parameters.stanreg.html
    ## https://easystats.github.io/bayestestR/reference/point_estimate.html
    ## RUN: modelsummary::get_estimates(model_objects_as_stanfits[[1]], centrality = c("median", "mode"))
    centrality = c("mode"),
    
    ## Highest Density/Credible Interval == Continuous; Shortest Probability Interval 
    ## https://easystats.github.io/bayestestR/articles/credible_interval.html
    ## https://easystats.github.io/bayestestR/reference/spi.html
    ## https://statmodeling.stat.columbia.edu/2020/06/29/shortest-posterior-intervals/
    ## https://statmodeling.stat.columbia.edu/2015/03/28/publication-one-pet-ideas-simulation-efficient-shortest-probability-intervals/
    ci = 0.95,
    ci_method = "spi", 
    test = "p_direction",
    effects = "all",
    group_level = FALSE,
    component = "all",
    diagnostic = "all", 
    
    expoentiate = FALSE,
    fmt = modelsummary::fmt_statistic("mode" = 2, "conf.low" = 2, "conf.high" = 2, "pd" = 2), 
    include_reference = TRUE,  ## Sadly does not work for stanfit objects.
    
    gof_map = NA,
    coef_omit = NULL, ## https://modelsummary.com/articles/modelsummary.html#coef_omit
    coef_map = model_objects_coefs_pretty_mapping_all,
    add_rows = model_objects_extra_rand_eff_stdevs_modelsummary_format
    ) 
