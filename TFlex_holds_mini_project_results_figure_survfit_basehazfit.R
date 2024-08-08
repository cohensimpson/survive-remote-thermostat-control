## Assessment of Model Fit (Overall Survival Curve + Baseline Hazard) ----------
## https://github.com/stan-dev/rstanarm/issues/280#issuecomment-605539720
## https://github.com/stan-dev/rstanarm/issues/280#issuecomment-600569187
## https://discourse.mc-stan.org/t/calculating-hazard-ratios-and-interpreting-differences-in-survival-and-between-a-multi-level-covariate-from-rstanarm-stan-surv/22542
## https://stackoverflow.com/questions/63222192/posterior-survfit-is-not-using-the-nd/63254315#63254315
## https://github.com/stan-dev/rstanarm/issues/440#issuecomment-639198936

## https://discourse.mc-stan.org/t/plotting-and-validating-survival-analysis-models/19905/8
## https://github.com/ermeel86/surv_stan_stancon2019
## https://stats.stackexchange.com/a/285473






## Number of Timepoints for Plotting -------------------------------------------
## Number of points between 0 and 150 minutes.
time_points_survfit = 150 
time_points_base_haz = 150 

## Number of draws used for plotting (which can take long + use lots of memory)
posterior_sample_size_subsamples_survfit = 500 






## Non-Parametric Kaplan-Meier Survival Curve ----------------------------------
km_survfit_best_mod_fitted_data = survival::survfit.formula(
    formula = survival::Surv(hold_survival_time_granular, hold_overridden) ~ 1, 
    data = model_objects[[best_mod_loo]][["data"]], 
    id = kraken_flex_device_id,
    cluster = account_number,
    type = "kaplan-meier",
    se.fit = TRUE,
    # conf.type = "none",
    conf.int = 0.95
    )

## https://rpkgs.datanovia.com/survminer/reference/surv_summary.html
km_survfit_best_mod_fitted_data = (
    tibble::tibble(
        model = "KM",
        estimand = "surv_prob",
        survminer::surv_summary(x = km_survfit_best_mod_fitted_data)
    )
    %>% dplyr::select(model, time, surv, lower, upper)
    %>% dplyr::rename(estimate = surv, ci_lb = lower, ci_ub = upper)
    )






## Standardised Survival Curve From Bayesian Model -----------------------------
## https://github.com/stan-dev/rstanarm/blob/feature/survival/R/posterior_survfit.R
ppc_survfit_standardised_best_mod = rstanarm:::posterior_survfit.stansurv(
    object = model_objects[[best_mod_loo]],
    type = "surv",
    newdata = NULL, ## Use all observations/study units.
    times = 0,
    extrapolate = TRUE,
    control = base::list(
        edist = base::max(model_objects[[best_mod_loo]][["data"]][["hold_survival_time_granular"]]), 
        epoints = time_points_survfit 
        ),
    ## "standardise = TRUE" -> subject-specific survival probabilities are averaged
    ## across all individuals for whom the subject-specific predictions are obtained.
    ## This can be used to average over the covariate and random effects
    ## distributions of the individuals used to estimate the model, or the individuals
    ## included in "newdata".
    standardise = TRUE, ## Average across the entire sample.
    
    ## A scalar between 0 and 1 specifying the width to use for the uncertainty interval 
    ## for the predictions. For example prob = 0.95 (the default) means that the 
    ## 2.5th and 97.5th percentiles will be provided.
    prob = 0.95,
    draws = posterior_sample_size_subsamples_survfit, ## Using all samples takes a very long time.
    ## "return_matrix = TRUE" -> Return Underlying Data 
    ## https://discourse.mc-stan.org/t/survival-models-in-rstanarm/3998/119
    return_matrix = TRUE,
    seed = random_seed
    )

## "return_matrix = TRUE" results in a list of data frames with a length equal to 
## "epoints". Thus, each data frame is for each interpolated time for the fitted values.
## The columns of each data frames are the conditions in the order of the rows of "newdata".
## The rows of each data frame are the posterior draws for each condition at a given time point.
## Rename the list of data frames of fitted values using the times.
base::names(ppc_survfit_standardised_best_mod) = base::unlist(
    base::lapply(
        X = ppc_survfit_standardised_best_mod, 
        FUN = function(df){base::unique(base::attr(df, which = "time"))}
        )
    )

ppc_survfit_standardised_best_mod_long = (
    base::lapply(
        X = ppc_survfit_standardised_best_mod, 
        FUN = function(df){tibble::as_tibble(df) %>% tibble::rownames_to_column(var = "draw")}
        ) 
    ## https://dplyr.tidyverse.org/reference/bind.html
    %>% bind_rows(.id = "time")
    %>% dplyr::rename(estimate = standardised_survprob)
    %>% dplyr::mutate(
        model = "WeibullSurvFit",
        estimand = "surv_prob",
        time = base::as.numeric(time)
        )
)






## Visualisation: Model-Based vs. Non-Parametric Survival Curve ----------------
survfit_best_mod_plot = (
    ggplot2::ggplot(
        data = ppc_survfit_standardised_best_mod_long, 
        mapping = ggplot2::aes(
            x = time,
            y = estimate
            )
        )
    
   ## https://mjskay.github.io/ggdist/reference/stat_lineribbon.html
   ## https://mjskay.github.io/ggdist/articles/lineribbon.html
   ## https://mjskay.github.io/ggdist/articles/lineribbon.html#lineribbon-gradients
   ## https://mjskay.github.io/ggdist/reference/scale_colour_ramp.html
   + ggdist::stat_lineribbon(
       ## "hdci" yields the highest-density _continuous_ interval, also
       ## known as the shortest probability interval. *Note:* If the
       ## distribution is multimodal, this may not actually be the
       ## highest-density interval (there may be a higher-density
       ## *discontinuous* interval, which can be found using "hdi").
       # mapping = ggplot2::aes(fill_ramp = base::rev(ggplot2::after_stat(.width))),
       point_interval = ggdist::mode_hdci, 
       .width = c(0.68, 0.95, 1),
       linewidth = 0.5,
       linetype = "solid",
       n = posterior_sample_size_subsamples_survfit, 
       step = "vh" 
       )
    
    ## https://ggplot2.tidyverse.org/reference/geom_ribbon.html
    + ggplot2::geom_ribbon(
        data = km_survfit_best_mod_fitted_data, 
        mapping = ggplot2::aes(
            x = time,
            y = estimate,
            ymin = ci_lb,
            ymax = ci_ub,
            ),
        inherit.aes = FALSE,
        fill = NA,
        colour = "#4DC3D6",
        linetype = "solid",
        linewidth = 0.75,
        alpha = 0.85
        )
    
    + ggplot2::geom_line(
        data = km_survfit_best_mod_fitted_data, 
        mapping = ggplot2::aes(
            x = time,
            y = estimate,
            colour = model
            ),
        inherit.aes = FALSE,
        # colour = "#4DC3D6",
        linetype = "solid",
        linewidth = 0.75,
        alpha = 0.85
        )
    
    ## https://scales.r-lib.org/reference/label_percent.html
    ## https://thomasadventure.blog/posts/ggplot2-percentage-scale/
    + ggplot2::scale_y_continuous(
        breaks = c(0.80, 0.85, 0.90, 0.95, 1.00), 
        labels = scales::label_percent(scale = 100, accuracy = 1, suffix = "%")
        )
    
    + ggplot2::labs(
        x = "\nElapsed Time (Scaled to Minutes) From Start of Energy Conservation Event", 
        y = latex2exp::TeX(input = "Standardised Survival (Override-Free) Probability $S(T)$", output = "expression")
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
        panel.grid.minor = ggplot2::element_blank(),
        
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
            
        name = "Probability Density w/in HDI: ",
        values = c("#FDB366", "#D6604D", "#4393C3"),
        breaks = c("1", "0.95", "0.68"),
        labels = c("100%", "95%", "68%"),
        na.value = "#A7A7A7"
        )
    
    + ggplot2::scale_colour_manual(
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
            
        name = "",
        values = c("#4DC3D6"),
        breaks = c("KM"),
        labels = c("Kaplan-Meir Survival Curve + 95% Frequentist Confidence Interval"),
        na.value = "#A7A7A7"
        )
    
    + ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, ncol = 4, byrow = TRUE))
    )


## Save the Visualisation
## https://ggplot2.tidyverse.org/reference/ggsave.html
## https://www.tidyverse.org/blog/2021/02/svglite-2-0-0/
## Need EMF? https://sscc.wisc.edu/sscc/pubs/dvr/saving-plots.html
ggplot2::ggsave(
    filename = "survfit_best_mod_plot.svg",
    plot = survfit_best_mod_plot,
    device = svglite::svglite,
    width = 10,
    height = 7,
    units = "in",
    scale = 1,
    dpi = 600,
    bg = "transparent"
    )


## https://cran.r-project.org/web/packages/export/index.html
## https://github.com/tomwenseleers/export
# export::graph2bitmap(
#     file = "survfit_best_mod_plot.png",
#     x = survfit_best_mod_plot,
#     type = "PNG",
#     font = "Helvetica",
#     scaling = 125, ## scale width & height by a certain percentage.
#     width = 10, ## Inches
#     height = 7,
#     cairo = TRUE,
#     dpi = 1200,
#     bg = "transparent"
# )






## Basic Non-Parametric Curve for Baseline Hazard Rate -------------------------
## https://devinincerti.com/2019/06/18/parametric_survival.html
kernel_basehaz_best_mod_fitted_data = muhaz::muhaz(
    times = model_objects[[best_mod_loo]][["data"]][["hold_survival_time_granular"]], 
    delta = model_objects[[best_mod_loo]][["data"]][["hold_overridden"]], 
    bw.method = "local",
    kern = "epanechnikov", ## https://bookdown.org/egarpor/NP-UC3M/kde-i-kde.html
    min.time = 0,
    max.time = base::max(model_objects[[best_mod_loo]][["data"]][["hold_survival_time_granular"]]),
    n.min.grid = 100, ## Number of points in the minimisation grid.
    n.est.grid = time_points_base_haz ## Number of points for interpolation
    )

kernel_basehaz_best_mod_fitted_data = tibble::tibble(
    model = "LocalBand",
    estimand = "hazard_rate",
    time = kernel_basehaz_best_mod_fitted_data[["est.grid"]],
    estimate = kernel_basehaz_best_mod_fitted_data[["haz.est"]],
    ci_lb = 0, ## muhaz::muhaz is not inferential; there are no confidence bands. Use zero vs. NA to avoid ggplot complaining.
    ci_ub = 0
    )






## Model-Based Curve for Baseline Hazard Rate ----------------------------------
## https://github.com/stan-dev/rstanarm/blob/af555ab1c21221b9db504f5dc9b4ed3097e09704/R/plots.R#L186
basehaz_timeline = base::seq(
    from = base::min(model_objects[[best_mod_loo]][["entrytime"]]),
    to = base::max(model_objects[[best_mod_loo]][["eventtime"]]),
    by = (
        base::max(model_objects[[best_mod_loo]][["eventtime"]]) - base::min(model_objects[[best_mod_loo]][["entrytime"]])
        )/time_points_base_haz 
    )

## This function is pulled from the stan_surv source code. RUN: "stanarm:::evaluate_basehaz"
## The result from "rstanarm:::evaluate_basehaz" is a matrix wherein the rows are posterior
## draws w/ length == posterior_sample_size and the columns are the time points, with 
## ncols == number of time points.
## See also "rstanarm:::evaluate_log_basehaz", "rstanarm:::log_basehaz_weibull", "rstanarm:::linear_predictor"
## https://github.com/stan-dev/rstanarm/blob/590ba0aaab632c2f4dd18ce4e24f8fb0d1c4c510/R/plots.R#L230
posterior_basehaz_best_mod = rstanarm:::evaluate_basehaz(
    times = basehaz_timeline,
    basehaz = model_objects[[best_mod_loo]][["basehaz"]],
    aux = rstanarm:::extract_pars.stansurv(model_objects[[best_mod_loo]])[["aux"]],
    intercept = rstanarm:::extract_pars.stansurv(model_objects[[best_mod_loo]])[["alpha"]]
    )
base::colnames(posterior_basehaz_best_mod) = basehaz_timeline
posterior_basehaz_best_mod = tibble::as_tibble(posterior_basehaz_best_mod)


posterior_basehaz_best_mod_long = (
    posterior_basehaz_best_mod
    %>% tibble::as_tibble()
    %>% tibble::rownames_to_column(var = "draw")
    ## https://tidyr.tidyverse.org/articles/pivot.html
    %>% tidyr::pivot_longer(
        cols = base::as.character(basehaz_timeline), 
        names_to = "time",
        values_to = "basehaz_estimate",
        values_drop_na = TRUE
        )
    %>% dplyr::rename(estimate = basehaz_estimate)
    %>% dplyr::mutate(time = base::as.numeric(time))
)






## Visualisation: Model-Based vs. Non-Parametric Baseline Hazard ---------------
## As the Weibull Shape Parameter (Gamma) approaches zero, the baseline hazard 
## approaches infinity. Thus, time == zero, will result in a baseline hazard
## rate that is infinite. This leads to a warning from ggplot about a dropped row.
## https://en.wikipedia.org/wiki/Weibull_distribution#Density_function
## https://communities.sas.com/t5/Statistical-Procedures/Generate-survival-time-using-Weibull-distribution/m-p/682659#M32824
basehaz_best_mod_plot = (
    ggplot2::ggplot(
        data = posterior_basehaz_best_mod_long, 
        mapping = ggplot2::aes(
            x = time,
            y = estimate
            )
        )
    
   ## https://mjskay.github.io/ggdist/reference/stat_lineribbon.html
   ## https://mjskay.github.io/ggdist/articles/lineribbon.html
   ## https://mjskay.github.io/ggdist/articles/lineribbon.html#lineribbon-gradients
   ## https://mjskay.github.io/ggdist/reference/scale_colour_ramp.html
   + ggdist::stat_lineribbon(
       ## "hdci" yields the highest-density _continuous_ interval, also
       ## known as the shortest probability interval. *Note:* If the
       ## distribution is multimodal, this may not actually be the
       ## highest-density interval (there may be a higher-density
       ## *discontinuous* interval, which can be found using "hdi").
       # mapping = ggplot2::aes(fill_ramp = base::rev(ggplot2::after_stat(.width))),
       point_interval = ggdist::mode_hdci, 
       .width = c(0.68, 0.95, 1),
       linewidth = 0.75,
       linetype = "solid",
       n = posterior_sample_size, 
       step = "vh" 
       )
    
    + ggplot2::geom_line(
        data = kernel_basehaz_best_mod_fitted_data, 
        mapping = ggplot2::aes(
            x = time,
            y = estimate,
            colour = model,
            ),
        inherit.aes = FALSE,
        # colour = "#4DC3D6",
        linetype = "solid",
        linewidth = 0.75,
        alpha = 0.85
        )
    
    + ggplot2::lims(y = c(0, 0.003))
    
    + ggplot2::labs(
        x = "\nElapsed Time (Scaled to Minutes) From Start of Energy Conservation Event", 
        y = latex2exp::TeX(input = "Baseline Hazard Rate (Instant. Override Prob.) $h_{0}(T_{i})$", output = "expression")
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
            
        name = "Probability Density w/in HDI: ",
        values = c("#FDB366", "#D6604D", "#4393C3"),
        breaks = c("1", "0.95", "0.68"),
        labels = c("100%", "95%", "68%"),
        na.value = "#A7A7A7"
        )
    
    + ggplot2::scale_colour_manual(
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
            
        name = "",
        values = c("#4DC3D6"),
        breaks = c("LocalBand"),
        labels = c("Non-Parametric Hazard Estimate"),
        na.value = "#A7A7A7"
        )
    
    + ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, ncol = 4, byrow = TRUE))
)


## Save the Visualisation
## https://ggplot2.tidyverse.org/reference/ggsave.html
## https://www.tidyverse.org/blog/2021/02/svglite-2-0-0/
## Need EMF? https://sscc.wisc.edu/sscc/pubs/dvr/saving-plots.html
ggplot2::ggsave(
    filename =  "basehaz_best_mod_plot.svg",
    plot = basehaz_best_mod_plot,
    device = svglite::svglite,
    width = 10,
    height = 7,
    units = "in",
    scale = 1,
    dpi = 600,
    bg = "transparent"
    )


## https://cran.r-project.org/web/packages/export/index.html
## https://github.com/tomwenseleers/export
# export::graph2bitmap(
#     file = "basehaz_best_mod_plot.png",
#     x = basehaz_best_mod_plot,
#     type = "PNG",
#     font = "Helvetica",
#     scaling = 125, ## scale width & height by a certain percentage.
#     width = 10, ## Inches
#     height = 7,
#     cairo = TRUE,
#     dpi = 1200,
#     bg = "transparent"
# )






## Fitted Values: Calculation of Results for Textual Presentation --------------
## https://mjskay.github.io/ggdist/reference/point_interval.html
base::print("Textual Results — Posterior Probabilties for Points Along Overall Standardised Survival Curve:")
(
    ppc_survfit_standardised_best_mod_long
    %>% dplyr::select(-draw, -model, -estimand)
    %>% dplyr::group_by(time)
    %>% ggdist::point_interval(
          estimate,
          .width = 0.95,
          .point = ggdist::Mode,
          .interval = ggdist::hdci,
          .simple_names = TRUE
        )
    %>% dplyr::select(-.point, -.interval)
    %>% dplyr::filter(
        dplyr::between(time, 24.5, 25.5) | dplyr::between(time, 49.5, 61.5) | dplyr::between(time, 99.5, 100.5) | dplyr::between(time, 150, 151) 
        )
) %>% print(n = 500)
