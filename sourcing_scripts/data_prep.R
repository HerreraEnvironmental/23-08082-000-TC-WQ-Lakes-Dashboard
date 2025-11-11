library(arrow)
library(tidyverse)
library(readxl)

## Source external lakes data from WQP
source('sourcing_scripts/WQP_data_query.R')

## Produced in sourcing_scripts/WQP_data_query.
wqp_data <- read_parquet('inputs/wqp_data.parquet')

## Set lake sites
lake_sites <- wqp_data |>
  select(SITE_CODE, SITE_NAME, LAT, LON) |>
  distinct() |>
  arrange(SITE_NAME)

write_parquet(lake_sites, 'outputs/lake_sites.parquet')

## Prepare lake water quality data
lakes_wq_dat <- wqp_data |>
  select(
    SITE_CODE,
    DateTime = date_time,
    parameter,
    value,
    unit,
    depth = depth_m,
    dup,
    mdl,
    pql,
    qualifier
  ) |>
  mutate(
    unit = trimws(unit),
    qualifier = trimws(qualifier),
    nonDetectFlag = grepl('U', qualifier) | grepl('<', value),
    value = as.numeric(gsub('<', '', value)),
    mdl = ifelse(mdl == 0, max(mdl[parameter == parameter]), mdl),
    pql = ifelse(pql < mdl, mdl, pql),
    newResultValue = ifelse(nonDetectFlag, mdl, value),
    newResultValue = ifelse(parameter == 'Turbidity' & newResultValue <= 0, 0.01, newResultValue),
    Year = year(DateTime),
    Month = month(DateTime),
    WaterYear = ifelse(Month >= 10, Year + 1, Year),
    FakeDate = as.Date(paste(2000, Month, day(DateTime), sep = '-')),
    WY_FakeDate = as.Date(if_else(Month >= 10, FakeDate - years(1), FakeDate))
  )

# Centralize grouping columns
group_cols <- c(
  'SITE_CODE', 'DateTime', 'depth', 'dup', 'Year', 'Month',
  'WaterYear', 'FakeDate', 'WY_FakeDate'
)

# Use reframe() to avoid summarise warning while preserving rows/values
lakes_wq_dat <- lakes_wq_dat |>
  bind_rows(
    #.,
    lakes_wq_dat |>
      group_by(across(all_of(group_cols))) |>
      reframe(
        value = newResultValue[parameter == 'Total Nitrogen'] /
          newResultValue[parameter == 'Total Phosphorus'],
        newResultValue = value,
        qualifier = paste(
          unique(c(
            qualifier[parameter == 'Total Nitrogen'],
            qualifier[parameter == 'Total Phosphorus']
          )),
          collapse = ' '
        ),
        nonDetectFlag = any(
          nonDetectFlag[parameter == 'Total Nitrogen'],
          nonDetectFlag[parameter == 'Total Phosphorus']
        ),
        parameter = 'N:P Ratio'
      ),
    lakes_wq_dat |>
      group_by(across(all_of(group_cols))) |>
      reframe(
        value = newResultValue[parameter == 'Chlorophyll a'] /
          newResultValue[parameter == 'Pheophytin a'],
        newResultValue = value,
        qualifier = paste(
          unique(c(
            qualifier[parameter == 'Chlorophyll a'],
            qualifier[parameter == 'Pheophytin a']
          )),
          collapse = ' '
        ),
        nonDetectFlag = any(
          nonDetectFlag[parameter == 'Chlorophyll a'],
          nonDetectFlag[parameter == 'Pheophytin a']
        ),
        parameter = 'Chl-a:Pheo-a Ratio'
      )
  ) |>
  mutate(qualifier = trimws(gsub('NA', '', qualifier)))

write_parquet(lakes_wq_dat, 'outputs/lakes_wq_dat.parquet')

sites_list <- setNames(
  lake_sites$SITE_CODE,
  paste0(lake_sites$SITE_NAME, ' (', lake_sites$SITE_CODE, ')')
)
parm_list <- unique(lakes_wq_dat$parameter)
years_list <- sort(unique(lakes_wq_dat$Year), T)

saveRDS(sites_list, 'outputs/sites_list.RDS')
saveRDS(parm_list, 'outputs/parm_list.RDS')
saveRDS(years_list, 'outputs/years_list.RDS')

unique(lakes_wq_dat$depth)
unique(lakes_wq_dat$dup)
unique(lakes_wq_dat$qualifier)