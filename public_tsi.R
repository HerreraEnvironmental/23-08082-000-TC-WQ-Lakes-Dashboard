library(tidyverse)

## This is a modified TSI calculation that includes the most recent growing 
## period (June to October) for the TSI values; at least 3 of the 6 months of data.


lakes_wq_dat<-readRDS('outputs/lakes_wq_dat.RDS')

tsi_calc_recent <-function(data,epi_depth=5,startMonth=6,endMonth=10){
  data |>
    dplyr::filter(parameter %in% c('Secchi Depth','Water transparency','Total Phosphorus','Chlorophyll a')&
                    Month>=startMonth&Month<=endMonth&
                    depth<=epi_depth) |>
    dplyr::group_by(Year,parameter) |>
    filter(n_distinct(Month) >= 3) |>
    dplyr::summarise(SummerMean=mean(newResultValue,na.rm=T), .groups = "drop") |>
    dplyr::ungroup() |>
    dplyr::mutate(TSI=round(ifelse(
      parameter=='Total Phosphorus', 14.42*log(SummerMean*1000) +4.15,
      ifelse(parameter=='Chlorophyll a', 9.81*log(SummerMean) + 30.6,
             ifelse( parameter=='Secchi Depth'|parameter=='Water transparency' , 60- 14.41*log(SummerMean),
                     NA))),1)
    )
  }

lakes_tsi_data_recent <- lakes_wq_dat |>
  group_by(SITE_CODE)|>
  tidyr::nest()|>
  mutate(TSI_out=purrr::map(.x=data,.f=~tsi_calc_recent(.x)))|>
  select(-data)|>
  tidyr::unnest(TSI_out)

write_csv(lakes_tsi_data_recent, "public_dashboard_outputs/lakes_tsi_data_recent.csv")