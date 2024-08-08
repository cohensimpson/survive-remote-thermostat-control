## Import Necessary Modules For Interfacing w/ ECMWF API -----------------------
library(readr)
library(dplyr)
library(KrigR)
library(tigris)






## Download Texas ZCTAs --------------------------------------------------------
## NSAPH ZIP Code to ZCTA Crosswalk (2021)
## https://github.com/NSAPH-Data-Processing/zip2zcta_master_xwalk
## https://doi.org/10.7910/DVN/HYNJSZ
NSAPH_zip_zcta_crosswalk_2020 = readr::read_csv(
    file = "NSAPH_zip_zcta_crosswalk/zip2zcta_master_xwalk.csv",
    col_names = TRUE
    ) %>% dplyr::filter(state == "TX", census_edition == "ZCTA5CE20")






## Download Texas Polygons/Geometry For ZCTAs ----------------------------------
## https://bookdown.org/rdpeng/RProgDA/mapping.html

## Tigris only provides state-specific ZCTA shapefiles for 2000 and 2010. 
## However, one can filter the 2020 shapefiles based on a list of ZCTAs.
## Oddly, one needs the first two digits for ranges of ZCTAs.
## Note, ZCTAs themselves are based on the U.S. Census, thus infrequently updated.
tx_zctas_geometry = tigris::zctas(
    year = 2020, 
    starts_with = base::unique(
        base::as.numeric(
            base::substring(NSAPH_zip_zcta_crosswalk_2020[["zcta"]], 1, 2)
            )
        ), 
    state = NULL,
    cb = FALSE, 
    class = "sf" ## Used for ggplot2 visualisations.
    )

tx_zctas_geometry_polygon = tigris::zctas(
    year = 2020, 
    starts_with = base::unique(
        base::as.numeric(
            base::substring(NSAPH_zip_zcta_crosswalk_2020[["zcta"]], 1, 2)
            )
        ), 
    state = NULL,
    cb = FALSE, 
    class = "sp"  ## Class == sp::SpatialPolygonsDataFrame (Required by KrigR)
    )






## Save Texas Polygons/Geometry For ZCTAs --------------------------------------
base:::saveRDS(tx_zctas_geometry, file = "tx_zctas_2020_geometry.rds", compress = FALSE)
base:::saveRDS(tx_zctas_geometry_polygon, file = "tx_zctas_2020_geometry_polygon.rds", compress = FALSE)






## Download ECMWF ReAnalysis v5 (ERA5) Data Via KrigR ---------------------------
## https://earthscience.stackexchange.com/a/23425
## https://www.erikkusch.com/courses/krigr/
## https://iopscience.iop.org/article/10.1088/1748-9326/ac48b3/pdf
## https://www.erikkusch.com/courses/krigr/download/


tx_temperature_ERA5_krigr = KrigR::download_ERA(
    ## https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels
    ## https://confluence.ecmwf.int/pages/viewpage.action?pageId=74764925
    DataSet = "era5",
    
    ## ERA5-Land has higher spatial resolution, but patchy/missing data for Texas in February, 2023
    ## https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land
    ## DataSet = "era5-land", 
    
    Type = "reanalysis", ## https://climate.copernicus.eu/climate-reanalysis
    
    ## Temperature of air 2m above the surface of land, sea or in-land waters. 
    ## 2m temperature is calculated by interpolating between the lowest model 
    ## level and the Earth's surface, taking account of the atmospheric conditions. 
    ## Temperature measured in kelvin can be converted to degrees Celsius (°C) by subtracting 273.15.
    ## https://confluence.ecmwf.int/display/CKB/ERA5:%202%20metre%20temperature
    ## https://confluence.ecmwf.int/display/TIGGE/Surface+air+temperature?src=contextnavpagetreemode
    Variable = "2m_temperature", ## Instantaneous variable measured in Kelvin (K)
    PrecipFix = FALSE,
    
    FUN = "mean", ## raster calculation argument
    DateStart = "2022-10-01",
    DateStop = "2024-05-30", 
    TResolution = "hour",
    TStep = 1,
    Extent = tx_zctas_geometry_polygon,
    Dir = base::getwd(), ## "KrigR::download_ERA" will generate multiple temporary data files, aggregate into one, and then delete.
    FileName = "tx_2m_temperature_ERA5_Oct_1_2022_May_30_2024_raw_NetCDF",
    API_User = API_User,
    API_Key = API_Key,
    TryDown = 20,
    SingularDL = FALSE  ## Pull data in monthly tranches
    )



tx_dewpoint_temperature_ERA5_krigr = KrigR::download_ERA(
    ## https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels
    ## https://confluence.ecmwf.int/pages/viewpage.action?pageId=74764925
    DataSet = "era5",
    
    ## ERA5-Land has higher spatial resolution, but patchy/missing data for Texas in February, 2023
    ## https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land
    ## DataSet = "era5-land", 
    
    Type = "reanalysis", ## https://climate.copernicus.eu/climate-reanalysis
    
    ## Temperature to which the air, at 2 metres above the surface of the Earth, 
    ## would have to be cooled for saturation to occur. It is a measure of the 
    ## humidity of the air. Combined with temperature, it can be used to calculate 
    ## the relative humidity. 2m dew point temperature is calculated by interpolating 
    ## between the lowest model level and the Earth's surface, taking account of 
    ## the atmospheric conditions. This parameter has units of kelvin (K). Temperature 
    ## measured in kelvin can be converted to degrees Celsius (°C) by subtracting 273.15.
    ## https://confluence.ecmwf.int/display/TIGGE/Surface+air+dew+point+temperature?searchId=GJOC1SVN8
    Variable = "2m_dewpoint_temperature", ## Instantaneous variable measured in Kelvin (K)
    PrecipFix = FALSE,
    
    FUN = "mean",
    DateStart = "2022-10-01",
    DateStop = "2024-05-30", 
    TResolution = "hour",
    TStep = 1,
    Extent = tx_zctas_geometry_polygon,
    Dir = base::getwd(), 
    FileName = "tx_2m_dewpoint_temperature_ERA5_Oct_1_2022_May_30_2024_raw_NetCDF",
    API_User = API_User,
    API_Key = API_Key,
    TryDown = 20,
    SingularDL = FALSE
    )
