## Load Roster of Hold Events --------------------------------------------------
## https://readr.tidyverse.org/reference/read_delim.html
## https://readr.tidyverse.org/reference/parse_datetime.html
## https://rdrr.io/r/base/strptime.html

## For details on roster construction, See: "TFlex_holds_mini_project.sql"
hold_events = readr::read_csv(
    file = "TFlex_roster_hold_events_any_length_as_of_May_30_2024 (2024-06-06).csv",
    col_names = TRUE,
    col_types = readr::cols(
        account_id = readr::col_character(), 
        account_number = readr::col_character(), 
        property_id = readr::col_character(), 
        market_supply_point_id = readr::col_character(),
        meter_point_id = readr::col_character(),
        esi_id = readr::col_character(),
        
        accountstatus_postcode = readr::col_character(),
        ESIID_postcode = readr::col_character(), 
        
        first_name = readr::col_character(), 
        last_name = readr::col_character(), 
        
        load_zone = readr::col_character(),
        service_provider = readr::col_character(),
        premise_type = readr::col_character(),
        
        date_of_earliest_tariff_contract_start = readr::col_datetime(),
        date_of_most_recent_tariff_contract_end = readr::col_datetime(),
        agreement_id = readr::col_character(),
        product_code = readr::col_character(),
        sales_subchannel = readr::col_character(),
        
        kraken_flex_device_id = readr::col_character(), ## Thermostat ID
        provider = readr::col_character(), ## Thermostat OEM
        
        number_of_holds_any_length_earlier_on_same_day = readr::col_integer(),
        date_earliest_hold_set = readr::col_datetime(),
        
        events_thermostat_id = readr::col_character(), ## Hold/Energy Conservation Event ID
        event_type = readr::col_character(),
        
        hold_event_report_date = readr::col_date(format = "%Y-%m-%d"),
        issued_start_at = readr::col_datetime(),
        hold_set_at = readr::col_datetime(),
        issued_end_at = readr::col_datetime(),
        hold_released_at = readr::col_datetime(),
        hold_overridden_at = readr::col_datetime(), ## NA/NULL whenever there is *no* override
        
        issued_duration = readr::col_integer(),
        actual_duration = readr::col_integer(),
        issued_event_length = readr::col_integer(),
        issued_event_length_granular = readr::col_double(),
        hold_survival_time = readr::col_integer(),
        hold_survival_time_granular = readr::col_double(),
        hold_overridden = readr::col_integer(), ## 0 = FALSE; 1 = TRUE 
        
        hvac_mode = readr::col_character(),
        
        starting_temperature = readr::col_double(),
        ending_temperature = readr::col_double(),
        desired_cool = readr::col_double(),
        hold_cool = readr::col_double(),
        desired_heat = readr::col_double(),
        hold_heat = readr::col_double(),
        
        octopus_tenure = readr::col_double(),
        hold_tenure = readr::col_double()
        ),
    na = c("", "NA", "null"),
    locale = readr::locale(decimal_mark = "."), ## https://stackoverflow.com/a/43096681
    col_select = NULL,
    show_col_types = TRUE,
    skip_empty_rows = FALSE
    )


## Slight pre-processing to facilitate joining of event data with other information.
hold_events = (
    hold_events
    
    ## Round down to the nearest hour within which a given hold event started.
    ## On "lubridate::floor_date", see: https://lubridate.tidyverse.org/reference/round_date.html
    %>% dplyr::mutate(
        hold_set_at_hour_utc = lubridate::floor_date(hold_set_at, unit = "hour")
        ) 
    
    ## Convert "ZIP code + 4 Digit" ESIID postcodes to 5-Digit ESIID ZIP codes
    ## On "ZIP + 4" vs. regular 5-Digit ZIPs, see: https://gis.stackexchange.com/a/236528
    ## Note, "ESIID_or_accountstatus_postcode" == ESIID postcode if non-missing, contact postcode if ESIID postcode is missing.
    ## Also, some ESIID postcodes are only/already 5-digit ZIP codes.
        ## RUN: "base::table(hold_events[["ESIID_or_accountstatus_postcode"]])"
    ## https://stringr.tidyverse.org/reference/str_length.html
    ## https://stringr.tidyverse.org/reference/str_sub.html
    %>% dplyr::mutate(
        ESIID_or_accountstatus_postcode_length = stringr::str_length(ESIID_or_accountstatus_postcode),
        postcode = stringr::str_sub(ESIID_or_accountstatus_postcode, start = 1, end = 5)
        ) 
    )


## There should be no duplicates. If there are, something has gone wrong with the SQL munging/joining.
base::paste(
    "Are the number of unique hold event IDs equal to the number of rows in the event roster dataframe (i.e., no duplicates)?", 
    base::length(base::unique(hold_events[["events_thermostat_id"]])) == base::nrow(hold_events)
    )






## Load Elec. Consumption For Customers In Hold Event Roster -------------------
## This data frame records quarter-hourly electricity consumption for 
## customers (account numbers) ever subjected to a hold as of May 30, 2024.
consumption_hold_events_participants = readr::read_csv(
    file = "TFlex_roster_hold_events_customer_quarter_hourly_kwh_since_Oct_2022 (2024-06-06).csv",
    col_names = TRUE,
    col_types = readr::cols(
        account_id = readr::col_character(),
        account_number = readr::col_character(),
        esi_id = readr::col_character(),
        consumption_start_utc = readr::col_datetime(),
        consumption_start_local = readr::col_datetime(),
        ## "consumption_kwh" == "actual_consumption_kwh" when not missing; otherwise, OEUS' "estimated_consumption_kwh"
        consumption_kwh = readr::col_double()
        ),
    na = c("", "NA", "null"),
    locale = readr::locale(decimal_mark = "."),
    col_select = NULL,
    show_col_types = TRUE,
    skip_empty_rows = FALSE
    )


consumption_hold_events_participants = (
    consumption_hold_events_participants
    %>% dplyr::mutate(
        ## https://lubridate.tidyverse.org/reference/force_tz.html
        ## "lubridate::force_tz" returns the same date time in a specific/different time zone. 
        consumption_start_local = lubridate::force_tz(time = consumption_start_local, tzone = "America/Chicago"),
        consumption_report_date_utc = lubridate::date(consumption_start_utc),
        
        ## Round down to the nearest hour within which a given consumption interval started.
        ## On "lubridate::floor_date", see: https://lubridate.tidyverse.org/reference/round_date.html
        consumption_start_hour_utc = lubridate::floor_date(consumption_start_utc, unit = "hour")
        )
    %>% dplyr::group_by(.add = FALSE, .drop = FALSE, account_number, consumption_start_hour_utc)
    %>% dplyr::arrange(.by_group = TRUE, consumption_start_utc)
    %>% dplyr::select(account_number, consumption_start_hour_utc, consumption_kwh)
    
    ## https://dplyr.tidyverse.org/reference/summarise.html
    %>% dplyr::summarise(
        total_hourly_consumption = base::sum(consumption_kwh, na.rm = TRUE)
        )
    ## https://dplyr.tidyverse.org/reference/lead-lag.html
    %>% dplyr::mutate(
        total_hourly_consumption_tminusone = dplyr::lag(total_hourly_consumption, n = 1, order_by = consumption_start_hour_utc)
        )
    %>% dplyr::ungroup()
    )






## NSAPH ZIP Code to ZCTA Crosswalk (2021) -------------------------------------
## https://github.com/NSAPH-Data-Processing/zip2zcta_master_xwalk
## https://doi.org/10.7910/DVN/HYNJSZ
NSAPH_zip_zcta_crosswalk_2020 = readr::read_csv(
    file = "NSAPH_zip_zcta_crosswalk/zip2zcta_master_xwalk.csv",
    col_names = TRUE
    ) %>% dplyr::filter(state == "TX", census_edition == "ZCTA5CE20")






## HUD + USPS ZIP Code to FIPS County Crosswalk Files (2021) -------------------
## FIPS -> Federal Information Processing Standard
## https://www.huduser.gov/portal/periodicals/cityscpe/vol20num2/ch16.pdf
## https://github.com/hantswilliams/hud_usps_crosswalk_files
## https://readxl.tidyverse.org
HUD_zip_county_crosswalk_2021 = (
    readxl::read_xlsx(path = "williams_HUD_usps_crosswalk_files_2021Q4/ZIP_COUNTY_122021.xlsx")
    
    ## https://dplyr.tidyverse.org/reference/group_by.html
    %>% dplyr::group_by(zip, .drop = FALSE)
    
    ## "res_ratio": Ratio of residential addresses in a ZIP code subsumed by a geographic entity (e.g., a county).
    ## Thus, multiple ZIP codes can "belong" to/comprise one FIPS county.  
    ## Here, use the FIPS code for the counties comprised by the great proportion of ZIP codes.  
    %>% dplyr::filter(res_ratio == base::max(res_ratio))
    %>% dplyr::ungroup()
    %>% dplyr::filter(usps_zip_pref_state == "TX")
    )






## International Energy Conservation Code Climate Zones ------------------------
## https://doi.org/10.2172/1893981
## https://www.jm.com/en/blog/2021/march/understanding-the-iecc-s-new-climate-zone-map/
## https://codes.iccsafe.org/content/IECC2021P2
## https://gist.github.com/marsha5813/b0279b4b0261634de358fca46e61e6f3
IECC_BA_climate_zones_2021 = readr::read_csv(
    file = "IECC_BA_climate_zones/marshall_iecc_ba_2021.csv",
    col_names = TRUE,
    col_types = readr::cols(
        fips = readr::col_character(), 
        State = readr::col_character(), 
        County = readr::col_character(),
        IECC15 = readr::col_integer(),
        IECC21 = readr::col_integer(),
        BA15 = readr::col_character(), 
        BA21 = readr::col_character(),
        Moisture15 = readr::col_character(), 
        Moisture21 = readr::col_character()
        ),
    na = c("", "NA", "null"),
    locale = readr::locale(decimal_mark = "."), 
    col_select = NULL,
    show_col_types = TRUE,
    skip_empty_rows = FALSE
    ) %>% dplyr::filter(State == "Texas")






## Load Texas ZCTA Geometry (SpatialPolygon Format) ----------------------------
## On creation of this file, see: "TFlex_holds_mini_project_process_data_ERA5_geometry.R"
tx_zctas_geometry_polygon = base:::readRDS(file = "*tx_zctas_2020_geometry_polygon.rds")






## Load ECMWF ReAnalysis 5 (ERA5) SpatialRaster Obtained Via KrigR -------------
## On creation of the following files, see: "TFlex_holds_mini_project_process_data_ERA5_geometry.R"
## https://rspatial.github.io/terra/reference/rast.html
tx_2m_temperature_ERA5_rasters = terra::rast(x = "*tx_2m_temperature_ERA5_Oct_1_2022_May_30_2024_raw_NetCDF.nc")
tx_2m_dewpoint_temperature_ERA5_rasters = terra::rast(x = "*tx_2m_dewpoint_temperature_ERA5_Oct_1_2022_May_30_2024_raw_NetCDF.nc")


## Map timestamps for each temperature vector in raster (i.e., t2m for a given hour) to its char. name.
## NOTE, ERA5 data use UTC timestamps: https://opendata.stackexchange.com/a/15587
## https://rspatial.github.io/terra/reference/time.html
tx_temperature_ERA5_timestamps = terra:::time(tx_2m_temperature_ERA5_rasters)
tx_temperature_ERA5_varnames = base::paste0("mean.", terra::names(tx_2m_temperature_ERA5_rasters))
base::names(tx_temperature_ERA5_varnames) = tx_temperature_ERA5_timestamps


tx_2m_dewpoint_temperature_ERA5_timestamps = terra:::time(tx_2m_dewpoint_temperature_ERA5_rasters)
tx_2m_dewpoint_temperature_ERA5_varnames = base::paste0("mean.", terra::names(tx_2m_dewpoint_temperature_ERA5_rasters))
base::names(tx_2m_dewpoint_temperature_ERA5_varnames) = tx_2m_dewpoint_temperature_ERA5_timestamps


## Change Coordinate Reference System (CRS) To Match CRS Used by Tigris/US Census Bureau
## https://walker-data.com/census-r/census-geographic-data-and-applications-in-r.html#coordinate-reference-systems
## https://rspatial.github.io/terra/reference/crs.html
## https://stackoverflow.com/a/30360918; https://gis.stackexchange.com/a/111227
terra::crs(tx_2m_temperature_ERA5_rasters) = "+proj=longlat +datum=NAD83 +no_defs"
terra::crs(tx_2m_dewpoint_temperature_ERA5_rasters) = "+proj=longlat +datum=NAD83 +no_defs"


## Extract ERA5 Data from Raster
## https://isciences.gitlab.io/exactextractr/reference/exact_extract.html
tx_temperature_ERA5_df = exactextractr::exact_extract(
    x = tx_2m_temperature_ERA5_rasters, 
    y = tx_zctas_geometry_polygon, 
    append_cols = base::names(tx_zctas_geometry_polygon), ## Include columns from "y".
    fun = "mean", ## "mean." prepended to the name of ERA5 variable names "2m_temperature".
    progress = TRUE,
    include_cell = FALSE,
    include_xy = FALSE,
    full_colnames = TRUE
    )

tx_dewpoint_temperature_ERA5_df = exactextractr::exact_extract(
    x = tx_2m_dewpoint_temperature_ERA5_rasters, 
    y = tx_zctas_geometry_polygon, 
    append_cols = base::names(tx_zctas_geometry_polygon),
    fun = "mean", ## "mean." prepended to the name of ERA5 variable names "2m_dewpoint_temperature".
    progress = TRUE,
    include_cell = FALSE,
    include_xy = FALSE,
    full_colnames = TRUE
    )


## Convert/Tidy Extracted ERA5 Data
tx_temperature_ERA5_df = (
    
    tx_temperature_ERA5_df 
    
    ## Convert to tibble
    %>% tibble::tibble() 
    
    ## Use the mapping/named list from above to make the timestamps the column names.
    %>% dplyr::rename(tidyselect::any_of(tx_temperature_ERA5_varnames)) 
    
    ## https://tidyr.tidyverse.org/articles/pivot.html
    %>% tidyr::pivot_longer(
        cols = base::names(tx_temperature_ERA5_varnames), 
        names_to = "time", 
        values_to = "mean_2m_temperature",
        values_drop_na = TRUE
        )
    %>% dplyr::mutate(
        time = lubridate::as_datetime(time, tz = "UTC"), ## https://opendata.stackexchange.com/a/15587
        day = lubridate::as_date(time)
        ) 
    %>% dplyr::group_by(.add = FALSE, .drop = TRUE, ZCTA5CE20)
    %>% dplyr::arrange(.by_group = TRUE, time)
    %>% dplyr::mutate(
        ## https://dplyr.tidyverse.org/reference/lead-lag.html
        mean_2m_temperature_tminusone = dplyr::lag(mean_2m_temperature, n = 1, order_by = time)
        ) 
    )

tx_dewpoint_temperature_ERA5_df = (
    
    tx_dewpoint_temperature_ERA5_df 
    
    ## Convert to tibble
    %>% tibble::tibble() 
    
    ## Use the mapping/named list from above to make the timestamps the column names.
    %>% dplyr::rename(tidyselect::any_of(tx_2m_dewpoint_temperature_ERA5_varnames)) 
    
    ## https://tidyr.tidyverse.org/articles/pivot.html
    %>% tidyr::pivot_longer(
        cols = base::names(tx_2m_dewpoint_temperature_ERA5_varnames), 
        names_to = "time", 
        values_to = "mean_2m_dewpoint_temperature",
        values_drop_na = TRUE
        )
    %>% dplyr::mutate(
        time = lubridate::as_datetime(time, tz = "UTC"), ## https://opendata.stackexchange.com/a/15587
        day = lubridate::as_date(time)
        ) 
    %>% dplyr::group_by(.add = FALSE, .drop = TRUE, ZCTA5CE20)
    %>% dplyr::arrange(.by_group = TRUE, time)
    %>% dplyr::mutate(
        ## https://dplyr.tidyverse.org/reference/lead-lag.html
        mean_2m_dewpoint_temperature_tminusone = dplyr::lag(mean_2m_dewpoint_temperature, n = 1, order_by = time)
        ) 
    )






## Load "ziptz" Data on Time Zones For US ZIP Codes (May 2024) -----------------
## https://github.com/infused/ziptz/tree/master
## https://github.com/infused/ziptz/tree/master/data

## https://gist.github.com/jwolfson/72bc7d7fd8d339955b38
## https://cran.r-project.org/web/packages/RSQLite/vignettes/RSQLite.html
## https://github.com/r-dbi/RSQLite

## Note, "America/Chicago" and "US/Central" are equivalent.
## https://github.com/nodatime/nodatime/issues/1342#issuecomment-481979335
## https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
## https://www.zeitverschiebung.net/en/timezone/america--chicago
## https://dba.stackexchange.com/a/94892

zipcode_timezones_database = RSQLite::dbConnect(
    drv = RSQLite::dbDriver("SQLite"), 
    dbname = "*ziptz_python_master_database_May2024 (2024-05-22).db"
    )
zipcode_timezones = RSQLite::dbReadTable(zipcode_timezones_database, "zip_codes")
RSQLite::dbDisconnect(conn = zipcode_timezones_database)






## Merge Datasets --------------------------------------------------------------
hold_events = dplyr::left_join(
    x = hold_events,
    y = dplyr::select(
        .data = consumption_hold_events_participants, 
        account_number, consumption_start_hour_utc,
        total_hourly_consumption, total_hourly_consumption_tminusone
        ),
    by = dplyr::join_by(
        account_number, 
        "hold_set_at_hour_utc" == "consumption_start_hour_utc"
        ),
    relationship = "many-to-one",
    copy = FALSE,
    suffix = c(".x", ".y")
    )


hold_events = dplyr::left_join(
    x = hold_events,
    y = dplyr::select(.data = NSAPH_zip_zcta_crosswalk_2020, zip, zcta),
    by = dplyr::join_by("postcode" == "zip"),
    relationship = "many-to-one",
    copy = FALSE,
    suffix = c(".x", ".y")
    ) 


hold_events = dplyr::left_join(
    x = hold_events,
    y = dplyr::select(.data = HUD_zip_county_crosswalk_2021, zip, county),
    by = dplyr::join_by("postcode" == "zip"),
    relationship = "many-to-one",
    copy = FALSE,
    suffix = c(".x", ".y")
    ) 


hold_events = dplyr::left_join(
    x = hold_events,
    y = dplyr::select(
        .data = IECC_BA_climate_zones_2021, 
        fips, County, IECC21, BA21, Moisture21
        ),
    by = dplyr::join_by("county" == "fips"),
    relationship = "many-to-one", 
    copy = FALSE,
    suffix = c(".x", ".y")
    ) %>% dplyr::rename(county_fips = County)


hold_events = dplyr::left_join(
    x = hold_events,
    y = dplyr::select(
        .data = tx_temperature_ERA5_df, 
        ZCTA5CE20, day, time, mean_2m_temperature, mean_2m_temperature_tminusone
        ),
    by = dplyr::join_by(
        "zcta" == "ZCTA5CE20", 
        "hold_event_report_date" == "day", 
        "hold_set_at_hour_utc" == "time"
        ),
    relationship = "many-to-one",  
    copy = FALSE,
    suffix = c(".x", ".y")
    )


hold_events = dplyr::left_join(
    x = hold_events,
    y = dplyr::select(
        .data = tx_dewpoint_temperature_ERA5_df, 
        ZCTA5CE20, day, time, mean_2m_dewpoint_temperature, mean_2m_dewpoint_temperature_tminusone
        ),
    by = dplyr::join_by(
        "zcta" == "ZCTA5CE20", 
        "hold_event_report_date" == "day", 
        "hold_set_at_hour_utc" == "time"
        ),
    relationship = "many-to-one",  
    copy = FALSE,
    suffix = c(".x", ".y")
    )
    

hold_events = dplyr::left_join(
    x = hold_events,
    y = dplyr::select(.data = zipcode_timezones, zip_code, time_zone, observes_dst, offset),
    by = dplyr::join_by("postcode" == "zip_code"),
    relationship = "many-to-one",  
    copy = FALSE,
    suffix = c(".x", ".y")
    )






## Transform Data --------------------------------------------------------------
## Create IECC Granular Climate Zones 
## https://doi.org/10.2172/1893981
## https://tidyr.tidyverse.org/reference/unite.html
hold_events = tidyr::unite(
    data = hold_events,
    col = "climate_zone_IECC", 
    c(IECC21, Moisture21), 
    sep = "", 
    remove = FALSE,
    na.rm = TRUE
    )
hold_events[["climate_zone_IECC"]][hold_events[["climate_zone_IECC"]] == ""] = NA


## Rescale Correlates in Preparation for Model Fitting
## https://mc-stan.org/docs/stan-users-guide/efficiency-tuning.html#standardizing-predictors-and-outputs
hold_events = dplyr::mutate(
    .data = hold_events,
    
    ## https://forcats.tidyverse.org/reference/fct_relevel.html
    ## https://dplyr.tidyverse.org/reference/case_when.html
    ## https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/scale
    hvac_mode = forcats::fct_relevel(hvac_mode, "COOL"), ## "COOL" == Ref
    climate_zone_IECC = forcats::fct_relevel(climate_zone_IECC, "2A"),
    ERCOT_load_zone = forcats::fct_relevel(load_zone, "LZ_HOUSTON"),
    
    starting_temperature = dplyr::case_when((starting_temperature %in% c(-500, -500.2)) ~ NA, .default = starting_temperature),
    starting_temperature_Z = base::scale(starting_temperature, center = TRUE, scale = TRUE)[, 1], ## base::scale -> one column matrix
    
    ## 2m_temperature measured in Kelvin
    ## Convert to degrees Celsius (°C) by subtracting 273.15.
    ## https://en.wikipedia.org/wiki/Kelvin
    ## https://en.wikipedia.org/wiki/Conversion_of_scales_of_temperature
    ## https://codes.ecmwf.int/grib/param-db/167
    mean_2m_temperature_tminusoneF = (((mean_2m_temperature_tminusone - 273.15) * (9/5)) + 32),
    mean_2m_temperature_tminusoneF_Z = base::scale(mean_2m_temperature_tminusoneF, center = TRUE, scale = TRUE)[, 1],
    
    ## 2m_dewpoint measured in Kelvin
    ## Convert to degrees Celsius (°C) by subtracting 273.15.
    ## https://en.wikipedia.org/wiki/Dew_point
    ## https://www.weather.gov/arx/why_dewpoint_vs_humidity
    ## https://learnmetrics.com/dew-point-calculator-chart-formula/
    ## https://www.wral.com/story/1201076/
    ## https://earthscience.stackexchange.com/a/7934
    ## https://codes.ecmwf.int/grib/param-db/168
    mean_2m_dewpoint_temperature_tminusoneF = (((mean_2m_dewpoint_temperature_tminusone - 273.15) * (9/5)) + 32),
    mean_2m_dewpoint_temperature_tminusoneF_Z = base::scale(mean_2m_dewpoint_temperature_tminusoneF, center = TRUE, scale = TRUE)[, 1],
    
    desired_cool = dplyr::case_when((desired_cool %in% c(400)) ~ NA, .default = desired_cool),
    desired_cool_Z = base::scale(desired_cool, center = TRUE, scale = TRUE)[, 1],
    
    desired_heat = dplyr::case_when((desired_heat %in% c(-400)) ~ NA, .default = desired_heat),
    desired_heat_Z = base::scale(desired_heat, center = TRUE, scale = TRUE)[, 1],
    
    ## Fundamental Customer Characteristics
    number_of_holds_any_length_earlier_on_same_day_Z = base::scale(number_of_holds_any_length_earlier_on_same_day, center = TRUE, scale = TRUE)[, 1],
    hold_tenure_Z = base::scale(hold_tenure, center = TRUE, scale = TRUE)[, 1],
    octopus_tenure_Z = base::scale(octopus_tenure, center = TRUE, scale = TRUE)[, 1],
    total_hourly_consumption_tminusone_Z = base::scale(total_hourly_consumption_tminusone, center = TRUE, scale = TRUE)[, 1],
    
    ## Temporal Variables
    ## https://lubridate.tidyverse.org/articles/lubridate.html
    ## https://lubridate.tidyverse.org/reference/hour.html
    ## https://lubridate.tidyverse.org/reference/round_date.html
    ## https://stackoverflow.com/questions/10862056/rounding-time-to-nearest-quarter-hour
    time_zone = dplyr::case_when((time_zone != "America/Chicago") ~ NA, .default = time_zone),
    
    ## https://lubridate.tidyverse.org/reference/with_tz.html
    ## https://stackoverflow.com/a/21753157
    ## https://stackoverflow.com/a/36598956
    ## https://stackoverflow.com/questions/33848563/with-tz-with-a-vector-of-timezones
    ## with_tz returns a date time as it would appear in a different time zone. 
    ## The actual moment of time measured does not change, just the time zone it is measured in.
    hold_set_at_local_time = lubridate::with_tz(time = hold_set_at, tzone = "America/Chicago"),
    
    hour_of_day = forcats::fct_relevel(base::paste0(lubridate::hour(hold_set_at_local_time), "H"), base::paste0(0:23, "H")),
    quarter_hour_of_day = forcats::fct_relevel(
        base::format(lubridate::round_date(hold_set_at_local_time, "15 minutes"), "%H:%M"), 
        base::as.character(forcats::fct_unique(base::format(lubridate::round_date(x = hold_set_at_local_time, unit = "15 minutes"), "%H:%M")))
        )
    )






## Filter Data For Model Fitting -----------------------------------------------
hold_events_processed = (
    hold_events 
    %>% dplyr::mutate(
        hold_policy = dplyr::case_when(
                issued_duration %in% c(0:3) ~ "0-3 Minute Holds (Off-Policy Lengths)",
                issued_duration %in% c(4:15) ~ "4-15 Minute Holds (Early 2023; Experimentation)",
                issued_duration %in% c(16:26) ~ "16-26 Minute Holds (Off-Policy Lengths)",
                issued_duration %in% c(27:29) ~ "27-29 Minute Holds (Status Quo From July 2023)",
                issued_duration %in% c(41:49) ~ "41-49 Minute Holds (Off-Policy Lengths)",
                issued_duration %in% c(56:59) ~ "56-59 Minute Holds (Special Events: April-May 2024 Price Spikes)",
                issued_duration %in% c(111:120) ~ "111-120 Minute Holds (Launch Regime; Late 2022-Early 2023)",
                issued_duration %in% c(146:149) ~ "146-149 Minute Holds (Special Events: October 14, 2023 Solar Eclipse)",
                .default = base::as.character(issued_duration)
            ),
        hold_policy = forcats::fct_relevel(
                hold_policy,
                "0-3 Minute Holds (Off-Policy Lengths)",
                "4-15 Minute Holds (Early 2023; Experimentation)",
                "16-26 Minute Holds (Off-Policy Lengths)",
                "27-29 Minute Holds (Status Quo From July 2023)",
                "41-49 Minute Holds (Off-Policy Lengths)",
                "56-59 Minute Holds (Special Events: April-May 2024 Price Spikes)",
                "111-120 Minute Holds (Launch Regime; Late 2022-Early 2023)",
                "146-149 Minute Holds (Special Events: October 14, 2023 Solar Eclipse)"
            ),
        ERCOT_load_zone = dplyr::case_when(
            ERCOT_load_zone %in% c("LZ_HOUSTON") ~ "Houston",
            ERCOT_load_zone %in% c("LZ_NORTH") ~ "North",
            ERCOT_load_zone %in% c("LZ_SOUTH") ~ "South",
            ERCOT_load_zone %in% c("LZ_WEST") ~ "West",
            .default = base::as.character(ERCOT_load_zone)
            )
        )
    %>% dplyr::select(
              account_number
            , postcode
            , zcta
            , county_fips
            , ERCOT_load_zone
            , kraken_flex_device_id
            , events_thermostat_id
            , hold_event_report_date
            , issued_start_at
            , hold_set_at
            , issued_end_at
            , hold_released_at
            , issued_event_length_granular
            , hold_survival_time_granular
            , hold_overridden
            , hold_policy
            , hvac_mode
            , ending_temperature
            , starting_temperature
            , starting_temperature_Z
            , mean_2m_temperature_tminusoneF
            , mean_2m_temperature_tminusoneF_Z
            , mean_2m_dewpoint_temperature_tminusoneF
            , mean_2m_dewpoint_temperature_tminusoneF_Z
            , climate_zone_IECC
            , desired_cool
            , desired_cool_Z
            , desired_heat
            , desired_heat_Z
            , hour_of_day
            , quarter_hour_of_day
            , number_of_holds_any_length_earlier_on_same_day
            , number_of_holds_any_length_earlier_on_same_day_Z
            , date_earliest_hold_set
            , hold_tenure
            , hold_tenure_Z
            , date_of_earliest_tariff_contract_start
            , octopus_tenure
            , octopus_tenure_Z
            , total_hourly_consumption
            , total_hourly_consumption_tminusone
            , total_hourly_consumption_tminusone_Z
        )
    %>% tidyr::drop_na(
          -hold_overridden
        , -ending_temperature
        )
    # %>% dplyr::group_by(account_number, .drop = FALSE)
    )






## Create Table of Summary Statistics ------------------------------------------
## https://www.pipinghotdata.com/posts/2021-07-14-polished-summary-tables-in-r-with-gtsummary/
## https://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html

## Simple Descriptives for Table Note
median_overrides_per_account = stats::median(
        (
            hold_events_processed 
            %>% dplyr::group_by(account_number)
            %>% dplyr::select(account_number, hold_overridden)
            %>% dplyr::summarise(overrides = base::sum(hold_overridden))
        )[["overrides"]]
    ) %>% formattable::digits(digits = 0, format = "f")

median_thermostats_per_account = stats::median(
        (
            hold_events_processed 
            %>% dplyr::group_by(account_number)
            %>% dplyr::select(account_number, kraken_flex_device_id)
            %>% dplyr::summarise(thermostats = dplyr::n_distinct(kraken_flex_device_id))
        )[["thermostats"]]
    ) %>% formattable::digits(digits = 0, format = "f")

median_events_per_account = stats::median(
        (
            hold_events_processed 
            %>% dplyr::group_by(account_number)
            %>% dplyr::select(account_number, events_thermostat_id)
            %>% dplyr::summarise(events = dplyr::n_distinct(events_thermostat_id))
        )[["events"]]
    ) %>% formattable::digits(digits = 0, format = "f")

median_events_per_calendar_date = stats::median(
        (
            hold_events_processed 
            %>% dplyr::group_by(hold_event_report_date)
            %>% dplyr::select(hold_event_report_date, events_thermostat_id)
            %>% dplyr::summarise(events = dplyr::n_distinct(events_thermostat_id))
        )[["events"]]
    ) %>% formattable::digits(digits = 0, format = "f")

median_accounts_per_zcta = stats::median(
        (
            hold_events_processed 
            %>% dplyr::group_by(zcta)
            %>% dplyr::select(zcta, account_number)
            %>% dplyr::summarise(accounts = dplyr::n_distinct(account_number))
        )[["accounts"]]
    ) %>% formattable::digits(digits = 0, format = "f")

median_accounts_per_county = stats::median(
        (
            hold_events_processed 
            %>% dplyr::group_by(county_fips)
            %>% dplyr::select(county_fips, account_number)
            %>% dplyr::summarise(accounts = dplyr::n_distinct(account_number))
        )[["accounts"]]
    ) %>% formattable::digits(digits = 0, format = "f")


## Finish Off the Table By Generating the Latex Output (Formatting Finished in Latex Source)
(   
    hold_events_processed
    
    # %>% dplyr::ungroup()
    %>%  dplyr::select(
          hold_survival_time_granular 
        , hold_overridden 
        , hold_policy
        , starting_temperature 
        , mean_2m_temperature_tminusoneF 
        , mean_2m_dewpoint_temperature_tminusoneF
        , desired_cool 
        , desired_heat 
        , hvac_mode
        # , climate_zone_IECC
        # , hour_of_day
        , number_of_holds_any_length_earlier_on_same_day 
        , hold_tenure
        , octopus_tenure 
        , total_hourly_consumption_tminusone
        , ERCOT_load_zone
        )
    %>% dplyr::mutate(
            hold_overridden = dplyr::case_when(
                (hold_overridden == 1) ~ "Yes", 
                (hold_overridden == 0) ~ "No (Num. Conservation Events w/ Censored Times)"
                ),
            hvac_mode = forcats::fct_recode(
                hvac_mode,
                Cool = "COOL", 
                Heat = "HEAT", 
                Auto = "AUTO", 
                Off = "OFF"
                )#,
            # climate_zone_IECC = forcats::fct_recode(
            #     forcats::fct_relevel(.f = climate_zone_IECC, "1A"), 
            #     `1A Very Hot Humid` = "1A",
            #     `2A Hot Humid` = "2A",
            #     `2B Hot Dry` = "2B",
            #     `3A Warm Humid` = "3A",
            #     `3B Warm Dry` = "3B"
            #     )
        )
    %>% gtsummary::tbl_summary(
        by = NULL,
        digits = base::list(
            gtsummary::everything() ~ 2
            , gtsummary::all_categorical() ~ c(0, 2) ## https://stackoverflow.com/a/60643785
            , hold_survival_time_granular ~ c(2)
            ),
        statistic = base::list(
            gtsummary::all_continuous() ~ "{mean} ({sd}) [{min}, {p5}, {median}, {p95}, {max}]",
            gtsummary::all_categorical() ~ "{n} ({p}%)"
            ),
        label = base::list(
              hold_survival_time_granular ~ "Actual Event Duration (Minutes)"
            , hold_overridden ~ "Hold Overridden"
            , hold_policy ~ "Hold Policy (OEUS Issued Event Duration)"
            , starting_temperature ~ "Internal Temperature ($^{\\circ}$F) at Event Start"
            , mean_2m_temperature_tminusoneF ~ "Near-Surface External Temperature ($^{\\circ}$F) 60 Min. Prior to Event Hour"
            , mean_2m_dewpoint_temperature_tminusoneF ~ "Near-Surface External Dewpoint ($^{\\circ}$F) 60 Min. Prior to Event Hour"
            , desired_cool ~ "Desired Internal Temperature ($^{\\circ}$F) - Cooling HVAC Trigger"
            , desired_heat ~ "Desired Internal Temperature ($^{\\circ}$F) - Heating HVAC Trigger"
            , hvac_mode ~ "HVAC Operating Mode at Event Start"
            # , climate_zone_IECC ~ "IECC Climate Zone - FIPS County" 
            # , hour_of_day ~ "Hour of Day of Event"
            , number_of_holds_any_length_earlier_on_same_day ~ "Number of Holds (Any Length) Earlier On Event Day"
            , hold_tenure ~ "Hold Tenure at Event Issuance - Years"
            , octopus_tenure ~ "OEUS Tenure at Event Issuance - Years"
            , total_hourly_consumption_tminusone ~ "Total Electricity Consumption (kWh) 60 Min. Prior to Event Hour"
            , ERCOT_load_zone ~ "ERCOT Grid Load Zone"
            )
        )
    
    ## https://www.danieldsjoberg.com/gtsummary/reference/modify.html
    %>% gtsummary::modify_caption(
        glue::glue("{dplyr::n_distinct(hold_events_processed[['account_number']]) %>% formattable::comma(digits = 0, format = 'f')} Customer Account Numbers (Median Conservation Events Per Account: {median_events_per_account}; Median Overrides Per Account: {median_overrides_per_account}); {formattable::comma(dplyr::n_distinct(hold_events_processed[['kraken_flex_device_id']]), digits = 0)} Thermostat IDs (Median Thermostats Per Account: {median_thermostats_per_account}); {dplyr::n_distinct(hold_events_processed[['hold_event_report_date']])} Calendar Days of Events (Median Conservation Events Per Date: {median_events_per_calendar_date}); {dplyr::n_distinct(hold_events_processed[['zcta']])} ZCTAs (Median Accounts Per ZCTA: {median_accounts_per_zcta}); {dplyr::n_distinct(hold_events_processed[['county_fips']])} FIPS Counties (Median Accounts Per County: {median_accounts_per_county}); Events issued during {base::length(fct_unique(hold_events[['quarter_hour_of_day']]))} of the 96 quarter-hours of the day.")
        )
    
    ## https://www.danieldsjoberg.com/gtsummary/articles/rmarkdown.html
    ## https://bookdown.org/yihui/rmarkdown-cookbook/kable.html
    ## https://bookdown.org/yihui/rmarkdown-cookbook/kableextra.html
    %>% gtsummary::as_kable(
        label = "tbl:summary-stats",
        format = "latex", 
        booktabs = FALSE, 
        linesep = "", 
        vline = "", 
        digits = 2, 
        align = "cc", 
        row.names = FALSE, 
        format.args = base::list(big.mark = ",", scientific = FALSE), 
        escape = TRUE
        )
    ) ## Copy output to Latex Source.






## Table of Event Counts Per Temperature Combination ---------------------------
## Finish Off the Table By Generating the Latex Output (Formatting Finished in Latex Source)
(
    tibble::tibble(
        starting_temperature_interval = ggplot2::cut_width(
            x = hold_events_processed[["starting_temperature"]], 
            width = 3.5, ordered_result = TRUE
            ), 
        mean_2m_temperature_tminusoneF_interval = ggplot2::cut_width(
            x = hold_events_processed[["mean_2m_temperature_tminusoneF"]], 
            width = 20, ordered_result = TRUE
            )
        )
    ## https://www.danieldsjoberg.com/gtsummary/reference/tbl_cross.html
    %>% gtsummary::tbl_cross(
        row = starting_temperature_interval,
        col = mean_2m_temperature_tminusoneF_interval,
        label = base::list(
            starting_temperature_interval ~ "Int. Temp. ($^\\circ$F) At Event Start",
            mean_2m_temperature_tminusoneF_interval ~ "Ext. Temp. ($^\\circ$F) 60 Min. Prior to Event Hour"
            ),
        percent = c("none"),
        margin = c("row" ,"column"),
        margin_text = "Total"
        )
    
    ## https://www.danieldsjoberg.com/gtsummary/reference/modify.html
    %>% gtsummary::modify_caption(
        glue::glue("Count of energy conservation events in analytic sample for various combinations of ranges of internal temperature at event start and external near-surface temperature during the 60 minutes prior to the hour during which a conservation event began.")
        )
    
    ## https://www.danieldsjoberg.com/gtsummary/articles/rmarkdown.html
    ## https://bookdown.org/yihui/rmarkdown-cookbook/kable.html
    ## https://bookdown.org/yihui/rmarkdown-cookbook/kableextra.html
    %>% gtsummary::as_kable(
        label = "tbl:event-temperature-combinations",
        format = "latex", 
        booktabs = FALSE, 
        linesep = "", 
        vline = "", 
        digits = 2, 
        align = "lccccccccc", 
        row.names = FALSE, 
        format.args = base::list(big.mark = ",", scientific = FALSE), 
        escape = FALSE
        )
    ) ## Copy output to Latex Source.
