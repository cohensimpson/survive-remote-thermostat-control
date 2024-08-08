## Overall Survival Curves Given Atmospheric Temperature -----------------------
## https://github.com/stan-dev/rstanarm/issues/280#issuecomment-605539720






## Number of Timepoints for Plotting -------------------------------------------
## Number of points between 0 and 150 minutes.
time_points_survfit = 150 

## Number of draws used for plotting (which can take long + use lots of memory)
posterior_sample_size_subsamples_survfit = 500 






## Weather Combinations for Fitted Values: -------------------------------------
## Let "rstanarm:::posterior_survfit.stansurv" do the hard lifting by simply building up the 
## scenarios for fitted values using its "newdata" arg + a loop through data frames created
## by filtering the big dataset used for model fitting based on the desired prediction scenarios.
newdata_for_fitted_values_weather = (
    model_objects[[best_mod_loo]][["data"]]
    %>% dplyr::ungroup()
    %>% dplyr::select(model_objects[[best_mod_loo]][["formula"]][["allvars"]], starting_temperature, mean_2m_temperature_tminusoneF)
    ## https://ggplot2.tidyverse.org/reference/cut_interval.html
    %>% dplyr::mutate(
            ## Note, some combinations of HVAC mode, internal temperature, and 
            ## external temperature are not seen in the data.
            ## RUN:
                # base::table(
                #     internal_temp = ggplot2::cut_width(
                #         x = model_objects[[best_mod_loo]][["data"]]$starting_temperature, 
                #         width = 3.5, ordered_result = TRUE
                #         ),
                #     external_temp = ggplot2::cut_width(
                #         x = model_objects[[best_mod_loo]][["data"]]$mean_2m_temperature_tminusoneF, 
                #         width = 20, ordered_result = TRUE
                #         )
                #     )
            ## Std. dev of internal temperature approx. 4.
            starting_temperature_interval = ggplot2::cut_width(
                x = starting_temperature, width = 3.5, ordered_result = TRUE
                ),
            ## Std. dev of external temperature approx. 20.
            mean_2m_temperature_tminusoneF_interval = ggplot2::cut_width(
                x = mean_2m_temperature_tminusoneF, width = 20, ordered_result = TRUE
                ),
            
            ## https://forcats.tidyverse.org/reference/fct_cross.html
            condition = forcats::fct_cross(
                starting_temperature_interval, mean_2m_temperature_tminusoneF_interval, 
                sep = ":", keep_empty = TRUE
                )
        )
    )


## Restrict the weather combinations under consideration to those that are most-frequent.
conditions_int_temp = c("(64.8,68.2]", "(68.2,71.8]", "(71.8,75.2]", "(75.2,78.8]", "(78.8,82.2]") 
conditions_ext_temp = c("(10,30]", "(30,50]", "(50,70]", "(70,90]", "(90,110]")
newdata_for_fitted_values_weather = dplyr::filter(
    .data = newdata_for_fitted_values_weather, 
    starting_temperature_interval %in% conditions_int_temp, 
    mean_2m_temperature_tminusoneF_interval %in% conditions_ext_temp
    )

weather_conditions_for_fitted_values = (
    forcats::fct_count(newdata_for_fitted_values_weather[["condition"]], prop = TRUE) 
    %>% dplyr::filter(n > 0) ## n == count of observations in a given condition
    %>% dplyr::select(f) ## f == condition/factor level
    )[["f"]] %>% base::as.character()






## Obtained Fitted/Predicted Values --------------------------------------------
posterior_predictions_weather_best_mod_conditions = base::list()
for (fit_val_cond in weather_conditions_for_fitted_values){
    
    ## https://github.com/stan-dev/rstanarm/blob/feature/survival/R/posterior_survfit.R
    ## https://github.com/stan-dev/rstanarm/blob/590ba0aaab632c2f4dd18ce4e24f8fb0d1c4c510/R/posterior_survfit.R#L364
    posterior_predictions_weather_best_mod = rstanarm:::posterior_survfit.stansurv(
        object = model_objects[[best_mod_loo]],
        type = "surv", ## c("haz", "loghaz")
        newdata = newdata_for_fitted_values_weather %>% dplyr::filter(condition == fit_val_cond),
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
        standardise = TRUE, ## Here we want to average across fitted values for records that meet the filtering condition.
        
        ## A scalar between 0 and 1 specifying the width to use for the uncertainty interval 
        ## for the predictions. For example prob = 0.95 (the default) means that the 
        ## 2.5th and 97.5th percentiles will be provided.
        prob = 0.95,
        draws = posterior_sample_size_subsamples_survfit,
        ## "return_matrix = TRUE" -> Return Underlying Data 
        ## https://discourse.mc-stan.org/t/survival-models-in-rstanarm/3998/119
        return_matrix = TRUE,
        seed = random_seed
        )
    
    ## "return_matrix = TRUE" results in a list of data frames with a length equal to 
    ## "epoints". Thus, each data frame is for each interpolated time for the fitted values.
    ## The rows of each data frame are each posterior prediction, the number of which is given by "draws".
    ## Owing to "standardise = TRUE", these are the average fitted value across the rows of "newdata".
    ## Here, each row of "newdata" is one of the observations in the analytic sample that meets the filtering condition.
    ## Rename the list of data frames of fitted values using the times.
    base::names(posterior_predictions_weather_best_mod) = base::unlist(
        base::lapply(
            X = posterior_predictions_weather_best_mod, 
            FUN = function(df){base::unique(base::attr(df, which = "time"))}
            )
        )
    
    ## Unify/stack the list dataframes with predictions for each time point in long format.
    posterior_predictions_weather_best_mod = (
        base::lapply(
            X = posterior_predictions_weather_best_mod, 
            FUN = function(df){tibble::as_tibble(df) %>% tibble::rownames_to_column(var = "draw")}
            ) 
        ## https://dplyr.tidyverse.org/reference/bind.html
        %>% bind_rows(.id = "time")
        %>% dplyr::rename(surv_prob_estimate = standardised_survprob)
        %>% dplyr::mutate(
            condition = fit_val_cond,
            time = base::as.numeric(time)
            )
    )
    
    ## Save the Results
    posterior_predictions_weather_best_mod_conditions[[fit_val_cond]] = posterior_predictions_weather_best_mod 
}


## Prepare outputs of "rstanarm:::posterior_survfit.stansurv" to be plotted using ggplot.
posterior_predictions_weather_best_mod_conditions_stacked = (
    posterior_predictions_weather_best_mod_conditions
    %>% dplyr::bind_rows()
    ## https://tidyr.tidyverse.org/reference/separate_wider_delim.html
    %>% tidyr::separate_wider_delim(
        condition, 
        delim = ":", 
        names = c("starting_temperature_interval", "mean_2m_temperature_tminusoneF_interval"), 
        cols_remove = FALSE
        )
    %>% dplyr::mutate(
        ## https://forcats.tidyverse.org/reference/fct_expand.html
        condition = forcats::fct_expand(
            condition, 
            base::as.character(
                forcats::fct_unique(newdata_for_fitted_values_weather[["condition"]])
                )
            ),
            
        starting_temperature_interval = forcats::fct_expand(
            starting_temperature_interval, 
            base::as.character(
                forcats::fct_unique(newdata_for_fitted_values_weather[["starting_temperature_interval"]])
                )
            ),
        starting_temperature_interval = forcats::fct_relevel(
            starting_temperature_interval, 
            base::as.character(
                forcats::fct_unique(newdata_for_fitted_values_weather[["starting_temperature_interval"]])
                )
            ),
            
        mean_2m_temperature_tminusoneF_interval = forcats::fct_expand(
            mean_2m_temperature_tminusoneF_interval, 
            base::as.character(
                forcats::fct_unique(newdata_for_fitted_values_weather[["mean_2m_temperature_tminusoneF_interval"]])
                )
            ),
        mean_2m_temperature_tminusoneF_interval = forcats::fct_relevel(
            mean_2m_temperature_tminusoneF_interval, 
            base::as.character(
                forcats::fct_unique(newdata_for_fitted_values_weather[["mean_2m_temperature_tminusoneF_interval"]])
                )
            )
        )
    )






## Visualisation of Fitted Values ----------------------------------------------
posterior_predictions_weather_best_mod_plot = (

    ggplot2::ggplot(
        data = posterior_predictions_weather_best_mod_conditions_stacked, 
        ggplot2::aes(
            x = time,
            y = surv_prob_estimate,
            group = starting_temperature_interval,
            fill = starting_temperature_interval
            )
        )
    
    ## https://mjskay.github.io/ggdist/reference/stat_lineribbon.html
    ## https://mjskay.github.io/ggdist/articles/lineribbon.html
    ## https://mjskay.github.io/ggdist/reference/scale_colour_ramp.html
    + ggdist::stat_lineribbon(
        ## "hdci" yields the highest-density _continuous_ interval, also
        ## known as the shortest probability interval. *Note:* If the
        ## distribution is multimodal, this may not actually be the
        ## highest-density interval (there may be a higher-density
        ## *discontinuous* interval, which can be found using "hdi").
        ## https://easystats.github.io/bayestestR/articles/credible_interval.html
        # mapping = ggplot2::aes(fill_ramp = base::rev(ggplot2::after_stat(.width))),
        point_interval = ggdist::mode_hdci,
        .width = c(0.95),
        linewidth = 0.25,
        linetype = "solid",
        # alpha = 0.25,
        n = posterior_sample_size_subsamples_survfit, 
        step = "vh" 
        )
    
    ## https://teunbrand.github.io/ggh4x/reference/facet_nested_wrap.html
    ## https://teunbrand.github.io/ggh4x/reference/facet_wrap2.html
    ## https://teunbrand.github.io/ggh4x/reference/facet_nested.html
    ## https://teunbrand.github.io/ggh4x/reference/facet_grid2.html
    ## https://ggplot2.tidyverse.org/reference/labeller.html
    ## https://stackoverflow.com/a/56519489
    ## https://uliniemann.com/blog/2022-02-21-math-annotations-in-ggplot2-with-latex2exp/
    + ggh4x::facet_grid2(
        . ~ mean_2m_temperature_tminusoneF_interval,
        scales = "fixed", 
        labeller = ggplot2::labeller(
            .default = ggplot2::label_value,
            mean_2m_temperature_tminusoneF_interval = c(
                "(10,30]"  =  "(10\u2109,30\u2109]", 
                "(30,50]"  =  "(30\u2109,50\u2109]", 
                "(50,70]"  =  "(50\u2109,70\u2109]", 
                "(70,90]"  =  "(70\u2109,90\u2109]", 
                "(90,110]" =  "(90\u2109,110\u2109]"
                )
            )
        )
    
    ## https://scales.r-lib.org/reference/label_percent.html
    ## https://thomasadventure.blog/posts/ggplot2-percentage-scale/
    + ggplot2::scale_y_continuous(limits = c(0.60, 1.00), labels = scales::label_percent(scale = 100, accuracy = 2, suffix = "%"))

    + ggplot2::labs(
        x = "\nElapsed Time (Scaled to Minutes) From Start of Energy Conservation Event", 
        y = latex2exp::TeX(input = "Standardised Survival (Override-Free) Probability $S(T)$", output = "expression"),
        subtitle = "External Temp. 60 Minutes Prior to Hour of Event Start:"
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
        plot.margin = ggplot2::margin(t = 0.25, r = 0.25, b = 0.25, l = 0.25, unit = "cm"),
        
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
            
        name = "Internal Temp. at Event Start: ",
        values = c("#4393C3", "#4DC3D6", "#A7A7A7", "#EECC66", "#D6604D"),
        breaks = c("(64.8,68.2]", "(68.2,71.8]", "(71.8,75.2]", "(75.2,78.8]", "(78.8,82.2]"),
        labels = c("(64.8\u2109,68.2\u2109]", "(68.2\u2109,71.8\u2109]", "(71.8\u2109,75.2\u2109]", "(75.2\u2109,78.8\u2109]", "(78.8\u2109,82.2\u2109]"),
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
            
        name = "Internal Temp. at Event Start: ",
        values = c("#4393C3", "#4DC3D6", "#A7A7A7", "#EECC66", "#D6604D"),
        breaks = c("(64.8,68.2]", "(68.2,71.8]", "(71.8,75.2]", "(75.2,78.8]", "(78.8,82.2]"),
        labels = c("(64.8\u2109,68.2\u2109]", "(68.2\u2109,71.8\u2109]", "(71.8\u2109,75.2\u2109]", "(75.2\u2109,78.8\u2109]", "(78.8\u2109,82.2\u2109]"),
        na.value = "#A7A7A7"
        )
    
    ## https://aosmith.rbind.io/2020/07/09/ggplot2-override-aes/
    + ggplot2::guides(fill = ggplot2::guide_legend(override.aes = base::list(linetype = 0, size = NA, shape = NA)))
    )


## Save the Visualisation
## https://ggplot2.tidyverse.org/reference/ggsave.html
## https://www.tidyverse.org/blog/2021/02/svglite-2-0-0/
## Need EMF? https://sscc.wisc.edu/sscc/pubs/dvr/saving-plots.html
ggplot2::ggsave(
    filename = "posterior_predictions_weather_best_mod_plot.svg",
    plot = posterior_predictions_weather_best_mod_plot,
    device = svglite::svglite,
    width = 13.5,
    height = 6,
    units = "in",
    scale = 1,
    dpi = 600,
    bg = "transparent"
    )


ggplot2::ggsave(
    filename = "posterior_predictions_weather_best_mod_plot_CNZ_website.svg",
    plot = (
        posterior_predictions_weather_best_mod_plot
        + ggplot2::labs(
            x = "\nElapsed Time (Scaled to Minutes) From Start of Energy Conservation Event", 
            y = "Average Survival (Override-Free) Probability S(T)\n(Line = Most-Likely Posterior Value; Ribbon = 95% Credible Interval)",
            subtitle = "External Temp. 60 Minutes Prior to Hour of Event Start:",
            title = "Temperature, Time, and Customers' Propensity to Endure Remote Thermostat Control"
            )
        + ggplot2::theme(plot.margin = ggplot2::margin(t = 1.5, r = 1.5, b = 1.5, l = 1.5, unit = "cm"))
        # + ggplot2::theme(axis.title.x = ggplot2::element_text(face = "bold"))
        # + ggplot2::theme(axis.title.y = ggplot2::element_text(face = "bold"))
        ),
    device = svglite::svglite,
    width = 13.5,
    height = 6.5,
    units = "in",
    scale = 1,
    dpi = 600,
    bg = "#F6F5F0" ## Pale Tan-ish Grey Mix
    )


## https://cran.r-project.org/web/packages/export/index.html
## https://github.com/tomwenseleers/export
# export::graph2vector(
#     file = "posterior_predictions_weather_best_mod_plot_CNZ_website",
#     x = (
#         posterior_predictions_weather_best_mod_plot
#         + ggplot2::labs(
#             x = "\nElapsed Time (Scaled to Minutes) From Start of Energy Conservation Event", 
#             y = "Average Survival (Override-Free) Probability S(T)\n(Line = Most-Likely Posterior Value; Ribbon = 95% Credible Interval)",
#             subtitle = "External Temp. 60 Minutes Prior to Hour of Event Start:",
#             title = "Temperature, Time, and Customers' Propensity to Endure Remote Thermostat Control"
#             )
#         # + ggplot2::theme(axis.title.x = ggplot2::element_text(face = "bold"))
#         # + ggplot2::theme(axis.title.y = ggplot2::element_text(face = "bold"))
#         ),
#     type = "PDF",
#     font = "Helvetica",
#     scaling = 125, ## scale width & height by a certain percentage.
#     width = 12, ## Inches
#     height = 5,
#     cairo = TRUE,
#     fallback_resolution = 2400,
#     bg = "#F6F5F0" ## Pale Tan-ish Grey Mix
#    )
  
  
# export::graph2bitmap(
#     file = "posterior_predictions_weather_best_mod_plot.png",
#     x = posterior_predictions_weather_best_mod_plot,
#     type = "PNG",
#     font = "Helvetica",
#     scaling = 125, ## scale width & height by a certain percentage.
#     width = 12, ## Inches
#     height = 5,
#     cairo = TRUE,
#     dpi = 1200,
#     bg = "transparent"
#   )






## Fitted Values: Calculation of Results for Textual Presentation --------------
## https://mjskay.github.io/ggdist/reference/point_interval.html
base::print("Textual Results — Posterior Probabilties for Points Along Temperature-Specific Standardised Survival Curve:")
(
    posterior_predictions_weather_best_mod_conditions_stacked
    %>% dplyr::select(-draw)
    %>% dplyr::group_by(time, starting_temperature_interval, mean_2m_temperature_tminusoneF_interval, condition)
    %>% dplyr::filter(
        starting_temperature_interval %in% c("(64.8,68.2]", "(68.2,71.8]", "(71.8,75.2]", "(78.8,82.2]"),
        mean_2m_temperature_tminusoneF_interval %in% c("(10,30]", "(90,110]")
        )
    %>% ggdist::point_interval(
          surv_prob_estimate,
          .width = 0.95,
          .point = ggdist::Mode,
          .interval = ggdist::hdci,
          .simple_names = TRUE
        )
    %>% dplyr::select(-.point, -.interval)
    %>% dplyr::filter(time >= 150)
    %>% dplyr::arrange(mean_2m_temperature_tminusoneF_interval)
) %>% print(n = 500)
