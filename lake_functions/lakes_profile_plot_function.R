# tc_lakes_data<-WQX_PULL(startYear = 2000,endYear = 2022,project='Ambient_Water_Quality_Lakes') |>
#   wqx_cleanup()
# 
# tc_lake_sites<-wqx_siteInfo(project='Ambient_Water_Quality_Lakes') |>
#   transmute(
#     SITE_CODE=MonitoringLocationIdentifier,
#     SITE_NAME=MonitoringLocationName,
#     LAT=as.numeric(LatitudeMeasure),
#     LON=as.numeric(LongitudeMeasure)
#   )
library(vegabrite)
#in Vega??
lake_profile_plot<-function(data,parm,month,year,profile_log=F,maxDepth){
  
  profile_data<-data |>
    dplyr::filter(parameter==parm&
           Month>=month[1]&Month<=month[2]) |>
    dplyr::arrange(depth)
  
  points<-vl_chart() %>%
    vl_mark_point(filled=T,size=50,color='grey') 
  
  select_points<-vl_chart() |>
    vl_mark_point(filled=T,size=100,color='black')
  
  select_line<-vl_chart() |>
    vl_mark_line(color='black') 
  
  select_chart<-vl_layer(select_points,select_line) |>
    vl_filter(paste0('datum.Year==',year))
  
  
  vl_layer(points,select_chart) |>
    vl_add_data(values=profile_data)|>
    vl_encode_x("value:Q",
                scale=list(type=ifelse(profile_log,'log','linear')),
                title=parm) |>
    vl_encode_y("depth:Q",title='Depth (m)',
                scale=list(reverse=T,domain=c(0,maxDepth))) |>
    vl_encode_order(field='depth:Q') |>
    vl_encode_tooltip(field='parameter:N') |>
    vl_encode_tooltip(field='DateTime:T') |>
    vl_encode_tooltip(field='depth:Q') |>
    vl_encode_tooltip(field='value:Q') |>
    vl_add_properties(width=250,height=250) |>
    vl_facet(field='Month',type='ordinal',columns = 3)
  }


# WQX_PULL(startYear = 2000,endYear = 2022,project='Ambient_Water_Quality_Lakes',
#          site_list = c('THURSTONCOUNTY-EH-BLADEL000')) |>
#   wqx_cleanup() %>%
#   lake_profile_plot(parm='Water Temperature (°C)',month=6:9,year=2019,maxDepth = 10)
