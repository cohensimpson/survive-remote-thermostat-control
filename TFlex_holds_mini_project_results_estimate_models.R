## Estimate Survival Models ----------------------------------------------------
## https://grodri.github.io/survival/ParametricSurvival.pdf
## https://grodri.github.io/survival/

## https://www.sambrilleman.com/vignettes/stan_surv
## https://discourse.mc-stan.org/t/survival-models-in-rstanarm/3998/100
## https://discourse.mc-stan.org/t/survival-models-in-rstanarm/3998/103
## https://discourse.mc-stan.org/t/interpretation-of-stan-surv-with-categorical-variables-in-rstanarm/17771
## https://discourse.mc-stan.org/t/survival-predictions-by-categorical-covariates-in-stan-jm/9516/4
## https://discourse.mc-stan.org/t/calculating-hazard-ratios-and-interpreting-differences-in-survival-and-between-a-multi-level-covariate-from-rstanarm-stan-surv/22542
## https://www.andrewheiss.com/blog/2021/11/10/ame-bayes-re-guide/

## STAN_SURV is experimental. See:
    ## https://github.com/stan-dev/rstanarm/issues/570#issuecomment-1937215082
    ## https://github.com/stan-dev/rstanarm/issues?q=is%3Aissue+is%3Aopen+%22survival%22+%22stan_surv%22
    ## https://github.com/stan-dev/rstanarm/issues/582#issuecomment-1422266540






## Define Analytic Helper Functions --------------------------------------------
## Model Fitting Routine
stan_surv_weibull_fit = function(
    model_formula, 
    analysed_data,
    mcmc_chains = 4, cpu_cores = 4, iterations = 2000, warmup = 1000,
    reproducible_seed = random_seed
    ){
    
    ## Survival Model for Right-Censored Event Times
    ## https://mc-stan.org/rstan/reference/stan.html 
    ## https://github.com/stan-dev/rstanarm/tree/feature/survival/R
    ## https://github.com/stan-dev/rstanarm/blob/feature/survival/R/stan_surv.R
    model_fit = rstanarm::stan_surv(
        
        formula = model_formula, 
        data = analysed_data, 
        
        basehaz = "weibull", 
        
        ## Specify Prior Probability Distributions
        ## https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
        ## https://mc-stan.org/rstanarm/reference/priors.html
        ## http://mc-stan.org/rstanarm/articles/glmer.html
        ## https://discourse.mc-stan.org/t/improving-performance-on-logistic-regression-with-informative-priors/14707/2
        ## https://discourse.mc-stan.org/t/stan-glmer-speed-for-large-sample-sizes/18040/5
        prior_intercept = rstanarm::normal(location = 0, scale = 5, autoscale = FALSE),
        prior = rstanarm::normal(location = 0, scale = 0.25, autoscale = FALSE),
        prior_aux = rstanarm::exponential(rate = 2), ## Weibull Shape Param. Gamma: https://www.desmos.com/calculator/unqhjpvqdd
        
        ## https://mc-stan.org/rstanarm/reference/priors.html#covariance-matrices
        ## http://mc-stan.org/rstanarm/articles/glmer.html#priors-on-covariance-matrices-1
        ## https://discourse.mc-stan.org/t/the-decov-prior/2477/2
        ## https://github.com/stan-dev/rstanarm/issues/483#issuecomment-724823484
        ## https://github.com/stan-dev/rstanarm/issues/483#issuecomment-724991882
        ## Shape/Scale Relate to the Gamma Distribution: https://www.desmos.com/calculator/vk2tqrxpk5
        prior_covariance = rstanarm::decov(regularization = 1, concentration = 1, shape = 0.25, scale = 0.25),
        
        chains = mcmc_chains,
        cores = cpu_cores,
        iter = iterations, 
        warmup = warmup,
        thin = 1,
        
        ## https://mc-stan.org/rstanarm/reference/rstanarm-package.html#estimation-algorithms
        ## https://github.com/stan-dev/rstanarm/blob/590ba0aaab632c2f4dd18ce4e24f8fb0d1c4c510/R/stan_surv.R#L467
        algorithm = "sampling",
        
        ## https://mc-stan.org/rstanarm/reference/adapt_delta.html
        ## https://discourse.mc-stan.org/t/divergent-transitions-a-primer/17099
        ## https://discourse.mc-stan.org/t/adapt-delta/7947/3
        adapt_delta = 0.8,
        
        ## https://discourse.mc-stan.org/t/one-chain-always-slower-x10-than-other-chains/11960/5
        ## https://discourse.mc-stan.org/t/sampling-function-taking-too-long-to-run-at-chain-4/27060/2
        ## https://discourse.mc-stan.org/t/one-mcmc-chain-not-moving/2104
        ## https://groups.google.com/g/stan-users/c/xoXSGukOd3c/m/eGPVNy9arTsJ
        ## https://discourse.mc-stan.org/t/gradient-evaluation-time-differs-across-chains/1063
        ## https://discourse.mc-stan.org/t/rstan-tree-depth-leapfrog/7829/4
        init = "random",
        init_r = 0.5, 
        
        save_warmup = TRUE,
        
        ## https://mc-stan.org/docs/reference-manual/reproducibility.html
        seed = reproducible_seed
    )
    
    return(model_fit)
}






## Specify Models --------------------------------------------------------------
model_specifications = base::list(
    
    spec_one = stats::as.formula(
        
        survival::Surv(time = hold_survival_time_granular, event = hold_overridden) 
        
        ~ 1
        
        ## Heating, Ventilation, and Cooling System Mode During Event
        + hvac_mode
        
        ## Internal Temperature At Event Start
        + starting_temperature_Z
        + starting_temperature_Z:hvac_mode
        + I(starting_temperature_Z^2)
        
        ## External Temperature At Event Start
        + mean_2m_temperature_tminusoneF_Z
        + mean_2m_temperature_tminusoneF_Z:hvac_mode
        + I(mean_2m_temperature_tminusoneF_Z^2)
        
        + mean_2m_temperature_tminusoneF_Z:starting_temperature_Z
        
        ## External Dew Point At Event Start
        + mean_2m_dewpoint_temperature_tminusoneF_Z
        + I(mean_2m_dewpoint_temperature_tminusoneF_Z^2)
        
        ## Internal Temperature Preference At Event Start
        + desired_cool_Z
        + desired_cool_Z:hvac_mode
        
        + desired_heat_Z
        + desired_heat_Z:hvac_mode
        
        ## Intra-Day History of Load Shifting
        + number_of_holds_any_length_earlier_on_same_day_Z
        + hold_tenure_Z
        
        ## Customer Characteristics (Fundamental)
        + octopus_tenure_Z
        + total_hourly_consumption_tminusone_Z
        + ERCOT_load_zone
        
        ## Varying Intercepts (To Account for Clustered/Non-Independent Observations)
        ## On Varying Effects: Nested Vs. Crossed
            ## https://stats.stackexchange.com/a/228814
            ## https://stats.stackexchange.com/a/213366
            ## https://stats.stackexchange.com/a/275460
            ## https://stats.stackexchange.com/a/395919
            ## https://stats.stackexchange.com/a/492516
            ## https://rpsychologist.com/r-guide-longitudinal-lme-lmer/
        + (1 | account_number)
        + (1 | hold_event_report_date)
        + (1 | hour_of_day)
        )
    
    , spec_two = stats::as.formula(
        
        survival::Surv(time = hold_survival_time_granular, event = hold_overridden) 
        
        ~ 1
        
        ## Heating, Ventilation, and Cooling System Mode During Event
        + hvac_mode
        
        ## Internal Temperature At Event Start
        + starting_temperature_Z
        
        ## External Temperature At Event Start
        + mean_2m_temperature_tminusoneF_Z
        
        ## External Dew Point At Event Start
        + mean_2m_dewpoint_temperature_tminusoneF_Z
        
        ## Internal Temperature Preference At Event Start
        + desired_cool_Z
        + desired_heat_Z
        
        ## Intra-Day History of Load Shifting
        + number_of_holds_any_length_earlier_on_same_day_Z
        + hold_tenure_Z
        
        ## Customer Characteristics (Fundamental)
        + octopus_tenure_Z
        + total_hourly_consumption_tminusone_Z
        + ERCOT_load_zone
        
        ## Varying Intercepts (To Account for Clustered/Non-Independent Observations)
        ## On Varying Effects: Nested Vs. Crossed
            ## https://stats.stackexchange.com/a/228814
            ## https://stats.stackexchange.com/a/213366
            ## https://stats.stackexchange.com/a/275460
            ## https://stats.stackexchange.com/a/395919
            ## https://stats.stackexchange.com/a/492516
            ## https://rpsychologist.com/r-guide-longitudinal-lme-lmer/
        + (1 | account_number)
        + (1 | hold_event_report_date)
        + (1 | hour_of_day)
        )
    )






## Sampling Settings -----------------------------------------------------------
## https://discourse.mc-stan.org/t/is-having-4-chains-with-length-of-5000-the-same-as-having-8-chains-with-length-of-2500/26082/4
posterior_sample_cores = 10
posterior_sample_chains = posterior_sample_cores
posterior_sample_warmup = 1000
posterior_sample_iterations = 3000

posterior_sample_size = (posterior_sample_cores * (posterior_sample_iterations - posterior_sample_warmup))

loo_cv_cores = 4    ## Careful with the choice here! Ram can easily be exhausted!






## Fit Models ------------------------------------------------------------------
## https://discourse.mc-stan.org/t/multi-chain-vs-single-chain/30094/4
## https://discourse.mc-stan.org/t/multi-chain-vs-single-chain/30094/8

model_objects = base::list()
model_fits = base::list()

for (form in base::names(model_specifications)){
    
    ## Fit the model using rstanarm
    temp_model_obj = stan_surv_weibull_fit(
        model_formula = model_specifications[[form]], 
        analysed_data = hold_events_processed,
        mcmc_chains = posterior_sample_chains, 
        cpu_cores = posterior_sample_cores, 
        iterations = posterior_sample_iterations, 
        warmup = posterior_sample_warmup,
        reproducible_seed = random_seed
        )
    
    ## Print the results
    base::print(
        bayestestR::describe_posterior(
            posterior = temp_model_obj[["stanfit"]], 
            centrality = "mode",
            dispersion = TRUE, 
            ci_method = "hdi", ## Credible Interval (CI) type. 
            ci = c(0.95), ## Credible Interval (CI) level. 
            diagnostic = "all", 
            test = NULL, 
            effects = "fixed",   ## Options == c("fixed", "random", "all")
            verbose = FALSE
            )
        )
    
    ## Display Sampler Diagnostics
    ## https://mc-stan.org/rstan/reference/check_hmc_diagnostics.html
    base::print(rstan::check_hmc_diagnostics(temp_model_obj[["stanfit"]]))
    
    ## Store the fitted model
    model_objects[[form]] = temp_model_obj 
    
    ## Save the fitted model (Estimation is slow! Lost progress is very costly!)
    base:::saveRDS(temp_model_obj, file = glue::glue("temp_model_obj_{form}.rds"), compress = FALSE)
    
    ## Clean-up Memory
    base::gc()
    
    
    ## Perform approximate Leave-One-Out Cross-Validation with fitted model
    ## http://mc-stan.org/loo/reference/loo.html
    ## http://mc-stan.org/loo/articles/loo2-example.html
    ## http://mc-stan.org/rstanarm/reference/loo.stanreg.html
    temp_model_loo_obj = loo::loo(
        x = temp_model_obj,
        save_psis = FALSE,
        cores = loo_cv_cores
        )
    
    ## Print a bit of progress to make the wait less painful.
    ## https://discourse.mc-stan.org/t/one-high-k-pareto-value/33419
    ## https://mc-stan.org/loo/reference/loo-glossary.html
    ## https://mc-stan.org/loo/articles/online-only/faq.html
    ## https://discourse.mc-stan.org/t/understanding-looic/13409
    ## https://discourse.mc-stan.org/t/understanding-looic/13409/8
    ## https://discourse.mc-stan.org/t/a-quick-note-what-i-infer-from-p-loo-and-pareto-k-values/3446/3
    ## https://discourse.mc-stan.org/t/a-quick-note-what-i-infer-from-p-loo-and-pareto-k-values/3446/20
    ## https://discourse.mc-stan.org/t/loo-performance-large-k-pareto-and-p-loo-with-linear-random-effects-model-on-longitudinal-data/31751/2
    ## https://discourse.mc-stan.org/t/model-selection-of-nonlinear-flexible-hierarchical-model-with-loo/22933/2
    ## https://discourse.mc-stan.org/t/model-checking-comparison-using-loo-vs-loo-compare/33694/2
    ## https://discourse.mc-stan.org/t/interpret-pareto-k-diagnostic/32318/4
    base::print(temp_model_loo_obj, digits = 3)
    
    ## Save Results From Loo
    model_fits[[form]] = temp_model_loo_obj
    
    ## Save LOO-CV stats (Very slow! Lost progress continues to be costly!)
    base:::saveRDS(temp_model_loo_obj, file = glue::glue("temp_model_loo_obj_{form}.rds"), compress = FALSE)
    
    ## Clean-up Memory
    base::gc()
}


## Useful One-Liners For Grab Diagnostics
## https://mc-stan.org/loo/reference/pareto-k-diagnostic.html
## model_objects[["spec_one"]][["data"]][loo::pareto_k_ids(model_fits[["spec_one"]], threshold = 0.7), ]
## model_objects[["spec_two"]][["data"]][loo::pareto_k_ids(model_fits[["spec_two"]], threshold = 0.7), ]

## https://mc-stan.org/rstan/articles/stanfit_objects.html#sampler-diagnostics 
## sampler_stats_spec_one = rstan::get_sampler_params(model_objects[["spec_one"]][["stanfit"]], inc_warmup = FALSE)
## sampler_stats_spec_two = rstan::get_sampler_params(model_objects[["spec_two"]][["stanfit"]], inc_warmup = FALSE)

## https://mc-stan.org/rstan/reference/check_hmc_diagnostics.html
## rstan::check_hmc_diagnostics(model_objects[["spec_one"]][["stanfit"]])
## rstan::check_hmc_diagnostics(model_objects[["spec_two"]][["stanfit"]])


## If Needed (e.g., R Crash/Ram Exhaustion), Load Models/LOO One at a Time
## https://stackoverflow.com/a/21505731
## base::readRDS("temp_model_obj_spec_one.rds")
## base::readRDS("temp_model_obj_spec_two.rds")

## base::readRDS("temp_model_loo_obj_spec_one.rds")
## base::readRDS("temp_model_loo_obj_spec_two.rds")






## Chain Diagnostics -----------------------------------------------------------
## https://mc-stan.org/bayesplot/reference/bayesplot-colors.html
bayesplot::color_scheme_set("brewer-Spectral")


## Visualisation of Chain Mixing For Core Parameters
## http://mc-stan.org/rstanarm/reference/plot.stanreg.html
# rstanarm:::plot.stanreg(
#     model_objects[["spec_one"]], 
#     pars = c("alpha", "beta"), 
#     regex_pars = c("Sigma"), 
#     plotfun = "dens_overlay"
#     )
# 
# rstanarm:::plot.stanreg(
#     model_objects[["spec_two"]], 
#     pars = c("alpha", "beta"), 
#     regex_pars = c("Sigma"), 
#     plotfun = "dens_overlay"
#     )


## https://mc-stan.org/rstan/reference/monitor.html
rhat_ess_spec_one = (
    rstan::monitor(model_objects[["spec_one"]][["stanfit"]], print = FALSE) 
    %>% tibble::as_tibble() 
    %>% dplyr::select("Rhat", "Bulk_ESS", "Tail_ESS") 
    %>% dplyr::summarise(
        max_rhat = base::max(Rhat), 
        min_bulk_ess = base::min(Bulk_ESS), min_tail_ess = base::min(Tail_ESS)
        )
    )

rhat_ess_spec_two = (
    rstan::monitor(model_objects[["spec_two"]][["stanfit"]], print = FALSE)  
    %>% tibble::as_tibble() 
    %>% dplyr::select("Rhat", "Bulk_ESS", "Tail_ESS") 
    %>% dplyr::summarise(
        max_rhat = base::max(Rhat),
        min_bulk_ess = base::min(Bulk_ESS), min_tail_ess = base::min(Tail_ESS)
        )
    )
    
rhat_ess_spec_all = (
    dplyr::bind_rows(rhat_ess_spec_one, rhat_ess_spec_two)
    %>% dplyr::mutate(
        spec = c(
            "HVAC Aware",
            "HVAC Naïve"
            )
        )
    )






## Table Summarising Approximate Leave-One-Out Cross-Validation ----------------
## Compare fitted models
## http://mc-stan.org/loo/reference/loo_compare.html
## http://mc-stan.org/rstanarm/reference/loo.stanreg.html
## https://discourse.mc-stan.org/t/one-high-k-pareto-value/33419/4
model_fit_stats_comparison = loo::loo_compare(x = model_fits)
base::print(model_fit_stats_comparison, digits = 3)


## Relative Contribution of Components of Fitted Models
## http://mc-stan.org/loo/reference/loo_model_weights.html
## https://mc-stan.org/loo/articles/loo2-weights.html
## https://discourse.mc-stan.org/t/how-to-describe-bayesian-stacking-weights/4162
model_fit_stack = loo::loo_model_weights(x = model_fits, method = "stacking", cores = 1) 
model_fit_stack = (
    model_fit_stack
    %>% tibble::as_tibble(rownames = "spec") 
    %>% dplyr::rename(stacking_weight = "x")
    %>% dplyr::mutate(
        spec = c(
            "HVAC Aware", 
            "HVAC Naïve"
            )
        )
    )


## https://mc-stan.org/loo/reference/loo_compare.html
loo_stat_names = base::attr(model_fit_stats_comparison, "dimnames")[[2]]

(
    model_fit_stats_comparison[ ,loo_stat_names] 
    
    %>% tibble::as_tibble(rownames = "spec") 
    %>% tibble::rownames_to_column(var = "model_rank")
    ## https://dplyr.tidyverse.org/reference/relocate.html
    %>% dplyr::relocate(spec, .before = model_rank) 
    %>% dplyr::mutate(
            spec = dplyr::case_when(
                (spec == "spec_one") ~ "HVAC Aware", 
                (spec == "spec_two") ~ "HVAC Naïve"
            ),
            pareto_k_bad = c(
                base::length(loo::pareto_k_ids(model_fits[["spec_one"]], threshold = 0.7)),
                base::length(loo::pareto_k_ids(model_fits[["spec_two"]], threshold = 0.7))
                )
        )
    %>% tidyr::unite(col = "elpd_diff_se_diff", elpd_diff, se_diff, sep = "_", remove = FALSE)
    %>% dplyr::select(-elpd_diff, -se_diff, -elpd_loo, -se_elpd_loo, -p_loo, -se_p_loo, -looic, -se_looic)
    %>% dplyr::right_join(
        y = model_fit_stack,
        by = dplyr::join_by(spec),
        # multiple = "all",
        relationship = "one-to-one", 
        copy = FALSE,
        suffix = c(".x", ".y")
        )
    %>% dplyr::right_join(
        y = rhat_ess_spec_all,
        by = dplyr::join_by(spec),
        # multiple = "all",
        relationship = "one-to-one", 
        copy = FALSE,
        suffix = c(".x", ".y")
        )
    
    %>% gt::gt() 
    # %>% gt::tab_header(title = "Model Predictive Performance Using Approximate Leave-One-Out Cross-Validation (LOO-CV)", subtitle = NULL)
    ## https://gt.rstudio.com/reference/md.html
    ## https://github.com/rstudio/gt/pull/1578
    %>% gt::cols_label(
              model_rank = gt::md("Rank")
            , spec = gt::md("Model Spec.")
            # , elpd_diff = gt::md("ELPD Diff.")
            # , se_diff = gt::md("ELPD Diff. (S.E.)")
            , elpd_diff_se_diff = gt::md("ELPD Diff. (S.E.)")
            # , elpd_loo = gt::md("ELPD")
            # , se_elpd_loo = gt::md("ELPD (S.E.)")
            # , p_loo = gt::md("p LOO")
            # , se_p_loo = gt::md("p LOO (S.E.)")
            , pareto_k_bad = gt::md("k-Hat > 0.7")
            , stacking_weight = gt::md("Stacking Weight")
            , max_rhat = gt::md("R-Hat (Max)")
            , min_bulk_ess = gt::md("Bulk ESS (Min)")
            , min_tail_ess = gt::md("Tail ESS (Min)")
            )
    ## https://gt.rstudio.com/reference/fmt_number.html
    %>% gt::fmt_number(
            columns = tidyselect::everything(),
            decimals = 2,
            suffixing = FALSE
            )
    %>% gt::fmt_number(
            columns = tidyselect::contains("ESS"),
            decimals = 0,
            suffixing = FALSE
            )
    %>% gt::fmt_number(
            columns = tidyselect::contains("Hat"),
            decimals = 3,
            suffixing = FALSE
            )
    %>% gt::fmt_number(
            columns = tidyselect::starts_with("k"),
            decimals = 0,
            suffixing = FALSE
            )
    %>% gt::cols_align(
            columns = -tidyselect::contains("Spec"),
            align = "center"
            )

    ## https://gt.rstudio.com/reference/tab_options.html
    %>% gt::tab_options(table.font.size = 12)
    
    ## https://gt.rstudio.com/reference/gtsave.html
    %>% gt::gtsave(filename = "all_fit_stats_tabular.tex") ## Open file and copy into Latex source.
    )






## Give LOO-CV, Which Model is "Best"? -----------------------------------------
## This variable is called/used throughout the remaining scripts/code for plotting (amongst other things).
best_mod_loo = "spec_one"






## Figure: Varying Intercepts for Hour of Day From Best Model ------------------
## http://mjskay.github.io/tidybayes/articles/tidy-rstanarm.html
varying_intercepts_hour_of_day_draws = (
    tidybayes::spread_draws(
        model = model_objects[[best_mod_loo]][["stanfit"]]
        , b[parameter, level]
        # , sep = ":"
        ) 
    %>% dplyr::select( !c(".chain", ".iteration", ".draw"))
    ## https://stackoverflow.com/a/40233929
    %>% dplyr::filter(stringr::str_detect(level, "_NEW_", negate = TRUE))
    %>% dplyr::filter(stringr::str_detect(level, "hour_of_day", negate = FALSE))
    %>% tidyr::separate(level, c("level", "hour_of_day"), ":")
    %>% dplyr::mutate(hour_of_day = forcats::fct_relevel(hour_of_day, base::paste0(0:23, "H")))
    %>% dplyr::group_by(parameter, level, hour_of_day)
    %>% dplyr::rename(estimate_u_H = b)
    %>% dplyr::mutate(
        ## https://dplyr.tidyverse.org/reference/dplyr_by.html
        PD = bayestestR::p_direction(x = estimate_u_H, .by = c(hour_of_day))[["pd"]]
        )
    )


## Create the Visualisation
varying_intercepts_hour_of_day_plot = (
    ggplot2::ggplot(
        data = varying_intercepts_hour_of_day_draws,
        mapping = ggplot2::aes(
            y = estimate_u_H, 
            x = hour_of_day, 
            group = hour_of_day, 
            
            ## https://ggplot2.tidyverse.org/reference/aes_eval.html
            fill = ggplot2::after_stat(y > 0)
            )
        )
    
    + ggplot2::geom_hline(
        yintercept = 0, linetype = "dotted", color = "#A7A7A7", linewidth = 0.75
        )
    
    ## Combo HDI/line-range + entire posterior distribution mini plot of each day's draws.
    ## https://mjskay.github.io/ggdist/reference/stat_halfeye.html
    ## https://mjskay.github.io/ggdist/articles/slabinterval.html
    + ggdist::stat_halfeye(
        ## "hdci" yields the highest-density _continuous_ interval, also
        ## known as the shortest probability interval. *Note:* If the
        ## distribution is multimodal, this may not actually be the
        ## highest-density interval (there may be a higher-density
        ## *discontinuous* interval, which can be found using "hdi").
        ## https://easystats.github.io/bayestestR/articles/credible_interval.html
        point_interval = ggdist::mode_hdi, ## Note, ggdist::mode_hdci used for survival curves.
        .width = c(0.68, 0.95),
        n = posterior_sample_size,
        point_size = 2.5,
        height = 2, ## https://mjskay.github.io/ggdist/articles/thickness.html
        interval_colour = "black",
        interval_linetype = "solid",
        point_colour = "black"
        )
    
    + ggplot2::scale_x_discrete(name = "\nHour of Day")
    + ggplot2::scale_y_continuous(
        name = latex2exp::TeX(input = r"(Hour-specific Deviations $u_{0, H}$ \[Log Hazard\] from Population Intercept $\beta_{0}$)", output = "expression"),
        )
    
    ## https://ggplot2.tidyverse.org/reference/theme.html
    + ggplot2::theme(
        panel.spacing.x = grid::unit(x = .75, units = "cm"),
        panel.spacing.y = grid::unit(x = .75, units = "cm"),
        
        strip.placement = "inside",
        
        axis.title = ggplot2::element_text(margin = ggplot2::margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm")),
        # axis.title.x = ggplot2::element_blank(),
        axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm")),
        # axis.title.y = ggplot2::element_blank(),
        
        plot.background = ggplot2::element_blank(),
        
        ##https://stackoverflow.com/a/18266219
        plot.margin = ggplot2::margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm"),
        
        panel.background = ggplot2::element_rect(fill = "transparent", color = "transparent"), 
        strip.background = ggplot2::element_blank(),
        legend.background = ggplot2::element_rect(fill = "transparent"), 
        legend.box.background = ggplot2::element_rect(fill = "transparent", color = "transparent"),
        legend.key = ggplot2::element_rect(fill = "transparent", color = "transparent"), 
        
        panel.grid = ggplot2::element_line(colour = "#A7A7A7", linetype = "solid", linewidth = 0.05),
        
        text = ggplot2::element_text(size = 12),
        axis.text = ggplot2::element_text(size = 12),
        legend.title = ggplot2::element_text(size = 12), 
        legend.text = ggplot2::element_text(size = 12),
        strip.text.x = ggplot2::element_text(size = 12),
        strip.text.y = ggplot2::element_text(size = 12),
        
        axis.ticks = ggplot2::element_line(colour = "#A7A7A7", linetype = "solid", linewidth = 0.05),
        
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "vertical",
        legend.margin =  ggplot2::margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
        legend.key.size = grid::unit(x = 0.5, units = "cm"), ## https://www.statology.org/ggplot2-legend-size/
        legend.spacing.y = grid::unit(x = 0.1, units = "cm")
        )
    
    + ggplot2::scale_fill_manual(
        ## https://jtools.jacob-long.com/reference/jtools_colors.html
        ## https://personal.sron.nl/~pault/
        ## Qualitative Colour Scheme:
            ## "#767676" ## Grey
            ## "#009988" ## Green
            ## "#CC3311" ## Red
            ## "#EECC66" ## Yellow
            ## "#DDAA33" ## Dark Yellow
            ## "#0077BB" ## Blue
            ## "#33BBEE" ## Cyan
            ## "#EE7733" ## Orange 
        ## Diverging Colour Scheme:
            ## "#A7A7A7" ## Pale Grey
            ## "#4393C3" ## Blue Pastel
            ## "#FDB366" ## Orange Pastel
            ## "#D6604D" ## Red Pastel
            ## "#4DC3D6" ## Turquoise
            
        # name = "HVAC Mode: ",
        # values = c("#4393C3", "#D6604D", "#FDB366", "#A7A7A7"),
        # breaks = c("COOL", "HEAT", "AUTO", "OFF"),
        # labels = c("Cool", "Heat", "Auto", "Off")
        
        name = "Sampled Posterior Parameter Value Positive (i.e., Certeris Paribus, Higher Instantaneous Probability of Override)? ",
        values = c("#FDB366", "#A7A7A7"),
        breaks = c("TRUE", "FALSE"),
        labels = c("True", "False")
        )
    
    ## https://aosmith.rbind.io/2020/07/09/ggplot2-override-aes/
    + ggplot2::guides(fill = ggplot2::guide_legend(override.aes = base::list(linetype = 0, size = NA, shape = NA)))
    )


## Save the Visualisation
## https://ggplot2.tidyverse.org/reference/ggsave.html
## https://www.tidyverse.org/blog/2021/02/svglite-2-0-0/
## Need EMF? https://sscc.wisc.edu/sscc/pubs/dvr/saving-plots.html
ggplot2::ggsave(
    filename =  "varying_intercepts_hour_of_day_plot.svg",
    plot = varying_intercepts_hour_of_day_plot,
    device = svglite::svglite,
    width = 15,
    height = 7.5,
    units = "in",
    scale = 0.95,
    dpi = 600,
    bg = "transparent"
)


## https://cran.r-project.org/web/packages/export/index.html
## https://github.com/tomwenseleers/export
# export::graph2bitmap(
#     file = "varying_intercepts_hour_of_day_plot.png",
#     x = varying_intercepts_hour_of_day_plot,
#     type = "PNG",
#     font = "Helvetica",
#     scaling = 125, ## scale width & height by a certain percentage.
#     width = 15, ## Inches
#     height = 7.5,
#     cairo = TRUE,
#     dpi = 1200,
#     bg = "transparent"
# )
