library(arrow)
library(tidyverse)
library(dataRetrieval)

## Define constants
WQP_COUNTY_CODE <- "US:53:067"
WQP_ORG_ID <- "THURSTONCOUNTY"
DEFAULT_TOD <- "00:00:00"
STREAM_SITE_TYPES <- c("Other-Surface Water", "River/Stream", "Pipe, Unspecified Source", "Spring", "River/Stream Perennial", "Seep", "River/Stream Intermittent", "Storm Sewer", "Estuary", "River/Stream Ephemeral")

## Fetch all sites in Thurston County.

thurston_lake_sites <- readWQPdata(organization='THURSTONCOUNTY',
                                     service='Station',
                                     Project='Ambient_Water_Quality_Lakes')

# Select only the site ids to be used in next data fetch
# This list of ids will likely not change very often so the previous steps may not be necessary each time only to update site id list
thurston_lake_sites_ids <- thurston_lake_sites$MonitoringLocationIdentifier

## Now fetch all water quality data for the selected sites
# Takes roughly 1 minute to download
wqp_data <- readWQPdata(
  siteid = thurston_lake_sites_ids
)

unique(wqp_data$CharacteristicName)

## Fetch other metadata from thurston_lake_sites to combine with hec_data
gid_ref <- thurston_lake_sites %>%
  select(c(MonitoringLocationIdentifier,
           MonitoringLocationDescriptionText,
           MonitoringLocationName,
           LatitudeMeasure,
           LongitudeMeasure))

gid_ref$gid <- str_match(gid_ref$MonitoringLocationDescriptionText, "GData ID:\\s*(.*?)\\s*;")

gid_ref$gid <- as.numeric(gid_ref$gid[,2])

## Grab necessary columns for HEC format
hec_data <- wqp_data %>%
  select(
    MonitoringLocationIdentifier, 
      ActivityStartDate, 
      ActivityStartTime.Time,
      Depth=ActivityDepthHeightMeasure.MeasureValue,
      Depth_Unit=ActivityDepthHeightMeasure.MeasureUnitCode,
      ResultMeasureValue,
      ResultMeasure.MeasureUnitCode,
      CharacteristicName,
      ResultAnalyticalMethod.MethodIdentifier,
      DetectionQuantitationLimitMeasure.MeasureValue,
      ActivityMediaSubdivisionName,
      MeasureQualifierCode, 
      ActivityTypeCode,
      ResultDepthHeightMeasure.MeasureValue
  )


## Merging hec_data with gid_ref
hec_data_merged <- merge(hec_data, gid_ref, by = "MonitoringLocationIdentifier")

unique(hec_data_merged$CharacteristicName)

## Cleaning of hec_data_merged dataframe

hec_data_merged$MonitoringLocationIdentifier <- str_remove(hec_data_merged$MonitoringLocationIdentifier, "THURSTONCOUNTY-")
hec_data_merged$DT <- paste(hec_data_merged$ActivityStartDate, hec_data_merged$ActivityStartTime.Time, sep = " ")

hec_data_merged$ResultMeasureValue <- as.numeric(hec_data_merged$ResultMeasureValue)
hec_data_merged$DetectionQuantitationLimitMeasure.MeasureValue <- as.numeric(hec_data_merged$DetectionQuantitationLimitMeasure.MeasureValue)
hec_data_merged$SITE_CODE <- hec_data_merged$MonitoringLocationIdentifier

hec_data_merged$sample_utc_offset <- NA
hec_data_merged$pql <- NA
hec_data_merged$lab_batch <- NA


hec_final <- hec_data_merged %>%
  select(c(
    SITE_CODE,
    Metro_ID= MonitoringLocationIdentifier,
    date_time=DT,
    depth_m=Depth,
    value=ResultMeasureValue,
    unit=ResultMeasure.MeasureUnitCode,
    parameter=CharacteristicName,
    method=ResultAnalyticalMethod.MethodIdentifier,
    mdl=DetectionQuantitationLimitMeasure.MeasureValue,
    matrix=ActivityMediaSubdivisionName,
    qualifier=MeasureQualifierCode,
    dup=ActivityTypeCode,
    gid,
    SITE_NAME=MonitoringLocationName,
    LAT=LatitudeMeasure,
    LON=LongitudeMeasure,
    sample_utc_offset,
    pql,
    lab_batch#,
  #  ResultDepthHeightMeasure.MeasureValue
  )) %>%
  mutate(parameter=dplyr::case_when(
    parameter %in%c('Total Phosphorus, mixed forms','Phosphorus') ~ 'Total Phosphorus',
    parameter=='Dissolved oxygen (DO)'&unit=='%' ~ 'Dissolved Oxygen (saturation)',
    parameter=='Dissolved oxygen (DO)' ~ 'Dissolved Oxygen',
    parameter=='Escherichia coli' ~ 'E. coli',
    parameter=="Temperature, water" ~ "Water Temperature (°C)",
    parameter=='Chlorophyll' ~ 'Chlorophyll a',
    parameter %in% c("Total Nitrogen, mixed forms","Nitrogen, mixed forms (NH3), (NH4), organic, (NO2) and (NO3)") ~ 'Total Nitrogen',
    parameter %in%c("Secchi, Horizontal Distance","Light attenuation at measurement depth")~'Secchi Depth',
    T ~ parameter
  ))




hec_final$date_time <- str_replace(hec_final$date_time, "NA", "00:00:00")

hec_final$date_time <- ymd_hms(hec_final$date_time)

hec_final <- hec_final[!is.na(hec_final$date_time),]


## Transform data to format used in Herrera All Stream Data Dump 4 12 2023.csv
#TODO drop this once converstion to parquet is working
write_parquet(hec_final, "inputs/wqp_data.parquet")
#write.csv(hec_final, file = "wqp_data.csv")

