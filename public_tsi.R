library(tidyverse)

lakes_wq_dat<-readRDS('outputs/lakes_wq_dat.RDS')

tsi_calc<-function(data,epi_depth=5,startMonth=6,endMonth=10){
  data |>
    dplyr::filter(parameter %in% c('Secchi Depth','Water transparency','Total Phosphorus','Chlorophyll a')&
                    Month>=startMonth&Month<=endMonth&
                    depth<=epi_depth) |>
    dplyr::group_by(Year,parameter) |>
    dplyr::summarise(SummerMean=mean(newResultValue,na.rm=T),.groups='drop') |>
    dplyr::ungroup() |>
    dplyr::mutate(TSI=round(ifelse(
      parameter=='Total Phosphorus', 14.42*log(SummerMean*1000) +4.15,
      ifelse(parameter=='Chlorophyll a', 9.81*log(SummerMean) + 30.6,
             ifelse( parameter=='Secchi Depth'|parameter=='Water transparency' , 60- 14.41*log(SummerMean),
                     NA))), 1)
    )
}

lakes_tsi_data <- lakes_wq_dat |>
  group_by(SITE_CODE)|>
  tidyr::nest()|>
  mutate(TSI_out=purrr::map(.x=data,.f=~tsi_calc(.x)))|>
  select(-data)|>
  tidyr::unnest(TSI_out)

## modifications ---------------------------------------------
tsi_calc2 <-function(data,epi_depth=5,startMonth=6,endMonth=10){
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

lakes_tsi_data2 <- lakes_wq_dat |>
  group_by(SITE_CODE)|>
  tidyr::nest()|>
  mutate(TSI_out=purrr::map(.x=data,.f=~tsi_calc2(.x)))|>
  select(-data)|>
  tidyr::unnest(TSI_out)
