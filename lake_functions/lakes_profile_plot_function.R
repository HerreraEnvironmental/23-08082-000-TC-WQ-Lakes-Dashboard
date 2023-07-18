tc_lakes_data<-WQX_PULL(startYear = 2000,endYear = 2022,project='Ambient_Water_Quality_Lakes') |>
  wqx_cleanup()

tc_lake_sites<-wqx_siteInfo(project='Ambient_Water_Quality_Lakes') |>
  transmute(
    SITE_CODE=MonitoringLocationIdentifier,
    SITE_NAME=MonitoringLocationName,
    LAT=as.numeric(LatitudeMeasure),
    LON=as.numeric(LongitudeMeasure)
  )
library(vegabrite)
#in Vega??
lake_profile_plot<-function(data,parm,month,year,profile_log=F,maxDepth){
  
  profile_data<-data |>
    dplyr::filter(parameter==parm&
           Month %in% month)
  select_year_data<-profile_data |>
    dplyr::filter(Year==year)
  
  points<-vl_chart() %>%
    vl_add_data(values=profile_data)|>
    vl_mark_point(filled=T,size=50,color='grey') |>
    vl_encode_color(field='Month:O')
  
  line<-vl_chart() %>%
    vl_add_data(values=select_year_data)|>
    vl_mark_point(filled=T,size=50,color='black') |>
    vl_mark_line()|>
    vl_encode_color(field='Month:O')
  
  vl_layer(points,line) |>
    vl_encode_x("value:Q",
                scale=list(type=ifelse(profile_log,'log','linear')),
                title=parm) |>
    vl_encode_y("depth:Q",title='Depth (m)',
                scale=list(reverse=T)) |>
    vl_encode_tooltip(field='parameter:N') |>
    vl_encode_tooltip(field='DateTime:T') |>
    vl_encode_tooltip(field='depth:Q') |>
    vl_encode_tooltip(field='value:Q') |>
    vl_add_properties(width=400,height=400)
}
# WQX_PULL(startYear = 2000,endYear = 2022,project='Ambient_Water_Quality_Lakes',
#          site_list = c('THURSTONCOUNTY-EH-BLADEL000')) |>
#   wqx_cleanup() %>%
#   lake_profile_plot(parm='Water Temperature (°C)',month=6,year=2019)
