library(arrow)
library(tidyverse)
library(dataRetrieval)
library(lubridate)

## Define constants
WQP_COUNTY_CODE <- "US:53:067"
WQP_ORG_ID <- "THURSTONCOUNTY"
DEFAULT_TOD <- "00:00:00"
STREAM_SITE_TYPES <- c(
  "Other-Surface Water", "River/Stream", "Pipe, Unspecified Source", "Spring",
  "River/Stream Perennial", "Seep", "River/Stream Intermittent", "Storm Sewer",
  "Estuary", "River/Stream Ephemeral"
)

## Fetch all sites in Thurston County.
thurston_lake_sites <- readWQPdata(
  organization = WQP_ORG_ID,
  service = "Station",
  Project = "Ambient_Water_Quality_Lakes"
)

# Select only the site ids to be used in next data fetch
thurston_lake_sites_ids <- thurston_lake_sites$MonitoringLocationIdentifier

## Now fetch all water quality data for the selected sites
# Takes roughly 1 minute to download
wqp_data <- readWQPdata(siteid = thurston_lake_sites_ids)

## Fetch other metadata from thurston_lake_sites to combine with hec_data
gid_ref <- thurston_lake_sites |>
  select(
    MonitoringLocationIdentifier,
    MonitoringLocationDescriptionText,
    MonitoringLocationName,
    LatitudeMeasure,
    LongitudeMeasure
  ) |>
  mutate(
    # Extract the GData ID value between "GData ID:" and ";", avoiding look-behind
    # Use a capturing group and take the first capture.
    gid = stringr::str_match(
      MonitoringLocationDescriptionText,
      "GData ID:\\s*([^;\\s]+)"
    )[, 2],
    gid = suppressWarnings(as.numeric(gid))
  )

## Grab necessary columns for HEC format
hec_data <- wqp_data |>
  select(
    MonitoringLocationIdentifier,
    ActivityStartDate,
    ActivityStartTime.Time,
    Depth = ActivityDepthHeightMeasure.MeasureValue,
    Depth_Unit = ActivityDepthHeightMeasure.MeasureUnitCode,
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
hec_data_merged <- hec_data |>
  mutate(MonitoringLocationIdentifier = as.character(MonitoringLocationIdentifier)) |>
  left_join(
    gid_ref |> mutate(MonitoringLocationIdentifier = as.character(MonitoringLocationIdentifier)),
    by = "MonitoringLocationIdentifier"
  )

## Cleaning of hec_data_merged dataframe
hec_data_merged <- hec_data_merged |>
  mutate(
    # Only remove the leading prefix to avoid accidental internal matches
    MonitoringLocationIdentifier = stringr::str_remove(
      MonitoringLocationIdentifier, "^THURSTONCOUNTY-"
    ),
    # Ensure missing times are set BEFORE pasting, avoiding literal "NA" strings
    ActivityStartTime.Time = coalesce(ActivityStartTime.Time, DEFAULT_TOD),
    DT = paste(ActivityStartDate, ActivityStartTime.Time, sep = " "),
    ResultMeasureValue = suppressWarnings(as.numeric(ResultMeasureValue)),
    DetectionQuantitationLimitMeasure.MeasureValue =
      suppressWarnings(as.numeric(DetectionQuantitationLimitMeasure.MeasureValue)),
    SITE_CODE = MonitoringLocationIdentifier,
    sample_utc_offset = NA,
    pql = NA,
    lab_batch = NA
  )

#looking at the Hicks Lake, for example, it looks like they reported it as feet, 
#but it is actually in m (based on the observed TP, which are extemely high and nothing at depth is shown)
#need to make an assumption here.
# hec_data_merged %>%
#   filter(grepl('Phosphorus',CharacteristicName)) %>%
#   select(MonitoringLocationIdentifier,ActivityStartDate,
#   Depth,Depth_Unit) %>%
#   distinct() %>%
#   ggplot(aes(x=as_date(ActivityStartDate),y=Depth,
# col=Depth_Unit))+
#     geom_point()+
#   facet_wrap(~MonitoringLocationIdentifier,scales='free_y')+
#   scale_y_reverse()
#there is no change in the depth values shown when the unit shifts.
#therefore, this is likely an issue of incorrect depth input.

hec_final <- hec_data_merged |>
  #not actually feet, stop converting
  #mutate(Depth=if_else(Depth_Unit=='ft',0.3048*Depth,Depth)) |>
  select(
    SITE_CODE,
    Metro_ID = MonitoringLocationIdentifier,
    date_time = DT,
    depth_m = Depth,
    value = ResultMeasureValue,
    unit = ResultMeasure.MeasureUnitCode,
    parameter = CharacteristicName,
    method = ResultAnalyticalMethod.MethodIdentifier,
    mdl = DetectionQuantitationLimitMeasure.MeasureValue,
    matrix = ActivityMediaSubdivisionName,
    qualifier = MeasureQualifierCode,
    dup = ActivityTypeCode,
    gid,
    SITE_NAME = MonitoringLocationName,
    LAT = LatitudeMeasure,
    LON = LongitudeMeasure,
    sample_utc_offset,
    pql,
    lab_batch
  ) |>
  mutate(
    date_time=ymd_hms(date_time,quiet=T),,
    parameter = dplyr::case_when(
      parameter %in% c("Total Phosphorus, mixed forms", "Phosphorus") ~ "Total Phosphorus",
      parameter == "Dissolved oxygen (DO)" & unit == "%" ~ "Dissolved Oxygen (saturation)",
      parameter == "Dissolved oxygen (DO)" ~ "Dissolved Oxygen",
      parameter == "Escherichia coli" ~ "E. coli",
      parameter == "Temperature, water" ~ "Water Temperature (°C)",
      parameter == "Chlorophyll" ~ "Chlorophyll a",
      parameter %in% c(
        "Total Nitrogen, mixed forms",
        "Nitrogen, mixed forms (NH3), (NH4), organic, (NO2) and (NO3)"
      ) ~ "Total Nitrogen",
      parameter %in% c(
        "Secchi, Horizontal Distance",
        "Light attenuation at measurement depth"
      ) ~ "Secchi Depth",
      TRUE ~ parameter
    )
  )

## Transform data to format used in Herrera All Stream Data Dump 4 12 2023.csv
write_parquet(hec_final, "inputs/wqp_data_forDash.parquet")

