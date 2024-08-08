## Strictly Filter ZCTA Polygons to TX For Plotting ----------------------------
## On creation of this file, see: "TFlex_holds_mini_project_process_data_ERA5_geometry.R"
tx_zctas_geometry = base:::readRDS(file = "*tx_zctas_2020_geometry.rds")
tx_zctas_geometry = tx_zctas_geometry[tx_zctas_geometry[["ZCTA5CE20"]] %in% NSAPH_zip_zcta_crosswalk_2020[["zcta"]], 1] 






## Unite Hold Event Data With Tigris Boundaries For Plot -----------------------
## https://forum.posit.co/t/convert-geometry-column-with-lists-of-longitude-latitude-coordinates/67114/6
tx_chropleth_data = (
    hold_events_processed   
    %>% dplyr::select(zcta, events_thermostat_id)
    %>% dplyr::group_by(zcta, .drop = FALSE)
    %>% dplyr::summarise(
        n_events_thermostat_id = dplyr::n_distinct(events_thermostat_id, na.rm = TRUE)
        )
    
    ## https://dplyr.tidyverse.org/reference/mutate-joins.html
    %>% dplyr::right_join(
        y = tx_zctas_geometry,
        by = dplyr::join_by("zcta" == "ZCTA5CE20"),
        relationship = "one-to-one",
        copy = FALSE,
        suffix = c(".x", ".y")
        ) 
        
    ## https://r-spatial.github.io/sf/reference/st_as_sf.html
    %>% sf::st_as_sf()
    )






## Create Map Using Tigris Boundaries + ggplot ---------------------------------
## https://github.com/walkerke/tigris
## https://stackoverflow.com/a/76854006
## https://jtr13.github.io/cc19/different-ways-of-plotting-u-s-map-in-r.html
## https://walker-data.com/census-r/census-geographic-data-and-applications-in-r.html

tx_choropleth = (
    ## For "ggmagnify::geom_magnify", data must be included via "ggplot2::ggplot".
    ## https://github.com/hughjonesd/ggmagnify 
    ## https://github.com/hughjonesd/ggmagnify/issues/24
    ## https://github.com/hughjonesd/ggmagnify/issues/15
    ggplot2::ggplot(
        data = tx_chropleth_data, 
        mapping = ggplot2::aes(fill = n_events_thermostat_id, geometry = geometry)
        )
    
    ## Rasterise to reduce plot size.
    ## https://ggplot2.tidyverse.org/reference/ggsf.html
    ## https://ggplot2.tidyverse.org/reference/ggsf.html#geometry-aesthetic
    ## https://github.com/VPetukhov/ggrastr
    + ggrastr::rasterise(input = ggplot2::geom_sf(linewidth = 0.04), dpi = 900)
    
    ## Limits are given as latitude and longitude.
    + ggplot2::lims(y = c(24, 40), x = c(-107, -90))
    
    ## https://ggplot2.tidyverse.org/reference/theme.html
    ## https://www.statology.org/ggplot2-legend-size/
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
        
        text = ggplot2::element_text(size = 6),
        axis.text = ggplot2::element_text(size = 6),
        legend.title = ggplot2::element_text(size = 5.5), 
        legend.text = ggplot2::element_text(size = 5.5),
        strip.text.x = ggplot2::element_text(size = 6),
        strip.text.y = ggplot2::element_text(size = 6),
        
        axis.ticks = ggplot2::element_line(colour = "#A7A7A7", linetype = "solid", linewidth = 0.05),
        
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "vertical",
        legend.margin =  ggplot2::margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
        legend.key.size = grid::unit(x = 0.35, units = "cm"), ## https://www.statology.org/ggplot2-legend-size/
        legend.key.width = grid::unit(x = 0.75, "cm"),
        legend.spacing.y = grid::unit(x = 0.1, units = "cm")
        )
    
    ## https://ggplot2.tidyverse.org/reference/scale_brewer.html
    ## https://stackoverflow.com/questions/70003893/how-to-rescale-color-mapping-in-scale-color-distiller-ggplot2
    + ggplot2::scale_fill_distiller(
        name = "Number of OEUS Energy Conservation Events:",
        palette = "Spectral", 
        values = NULL,
        limits = c(0, 8000),
        labels = function(value){formattable::comma(value, big.mark = ",", digits = 0, format = "f")},
        direction = -1, 
        guide = "colourbar", 
        na.value = "#CDCDCD", 
        position = "bottom"
        )
    
    ## Dallas (East)/Fort Worth (West)
    + ggmagnify::geom_magnify(
        ## from = c(xmin, xmax, ymin, ymax)
        from = c(-97.7, -96.4, 32.25, 33.35),
        
        ## to = c(xmin, xmax, ymin, ymax)
        to = c(-96.5, -90.5, 34.5, 40),
        
        shape = "rect",
        linewidth = 0.15, 
        shadow = TRUE,
        colour = "black", 
        recompute = FALSE, proj = "corresponding", aspect = "fixed",
        expand = 0.1, 
        scale.inset = 1.05,
        proj.combine = FALSE
        )
    
    ## Houston
    + ggmagnify::geom_magnify(
        ## from = c(xmin, xmax, ymin, ymax)
        from = c(-96.2, -94.7, 29.1, 30.4),
        
        ## to = c(xmin, xmax, ymin, ymax)
        to = c(-106.5, -101, 24.1, 29.9),
        
        shape = "rect",
        linewidth = 0.15, 
        shadow = TRUE,
        colour = "black", 
        recompute = FALSE, proj = "corresponding", aspect = "fixed",
        expand = 0.1,
        scale.inset = 1.05,
        proj.combine = FALSE
        )
    )



## Save the Visualisation
## https://ggplot2.tidyverse.org/reference/ggsave.html
## https://www.tidyverse.org/blog/2021/02/svglite-2-0-0/
## Need EMF? https://sscc.wisc.edu/sscc/pubs/dvr/saving-plots.html
ggplot2::ggsave(
    filename =  "tx_choropleth.pdf",
    plot = tx_choropleth,
    # device = svglite::svglite,
    width = 6,
    height = 6,
    units = "in",
    scale = 0.85,
    dpi = 600,
    bg = "transparent"
)


## https://cran.r-project.org/web/packages/export/index.html
## https://github.com/tomwenseleers/export
# export::graph2vector(
#     file = "tx_choropleth.svg", 
#     x = tx_choropleth,
#     type = "SVG", 
#     font = "Helvetica",
#     scaling = 125, ## scale width & height by a certain percentage.
#     width = 6, ## Inches
#     height = 6,
#     cairo = TRUE,
#     fallback_resolution = 1200,
#     bg = "transparent"
# )

# export::graph2bitmap(
#     file = "tx_choropleth.png",
#     x = tx_choropleth,
#     type = "PNG",
#     font = "Helvetica",
#     scaling = 115, ## scale width & height by a certain percentage.
#     width = 6, ## Inches
#     height = 6,
#     cairo = TRUE,
#     dpi = 900,
#     bg = "transparent"
# )






## ZCTA Event Summary Statistics -----------------------------------------------
tx_chropleth_data %>% dplyr::select(zcta, n_events_thermostat_id) %>% tidyr::drop_na() %>% base::summary()
