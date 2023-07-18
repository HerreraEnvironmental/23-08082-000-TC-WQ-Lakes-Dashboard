### lake data pulls

WQX_PULL<-function(site_list=NULL,parameter=NULL,waterYear=T,
                   startYear=NULL,endYear=NULL,project='Ambient_Water_Quality_Streams',organization='THURSTONCOUNTY'){
  if(!is.null(startYear)){
  startDate=as.Date(paste(ifelse(waterYear,startYear-1,startYear),
                          ifelse(waterYear,10,1),
                          1,sep='-'))
  } else startDate=NULL
  if(!is.null(endYear)){
  endDate=as.Date(paste(endYear,
                        ifelse(waterYear,9,12),
                        ifelse(waterYear,30,31),sep='-'))
  } else endDate=NULL
  
  arg_1<-list(
    'project'=project,
    'startDateLo'=startDate,
    'startDateHi'=endDate,
   # 'siteid'=site_list,
    'organization'=organization,
    'characteristicName'=parameter
  )
  
  dataRetrieval::readWQPdata(arg_1,siteid=site_list)
}

wqx_siteInfo<-function(project=NULL,organization=NULL,site_list=NULL){
  dataRetrieval::readWQPdata(
    list(
      'project'=project,
      'siteid'=site_list,
      'organization'=organization,
      'service'='Station'
    )
  )
}

wqx_cleanup<-function(wqx_out){
  wqx_out<-wqx_out |>
    transmute(SITE_CODE=MonitoringLocationIdentifier,
              DateTime=as.Date(ActivityStartDate),
              parameter=CharacteristicName,
              depth=ActivityDepthHeightMeasure.MeasureValue,
              #note it appears that TC lake data are in m, despite the UnitCode being ft
              depth_unit= ActivityDepthHeightMeasure.MeasureUnitCode,
              fraction=ResultSampleFractionText,
              value=ResultMeasureValue,
              unit=ResultMeasure.MeasureUnitCode,
              qualifier=MeasureQualifierCode,
              nonDetectFlag=grepl('U',qualifier),
              newResultValue=value,
              #newResultValue=ifelse(nonDetectFlag,pql,value),
              newResultValue=ifelse(parameter=='Turbidity'&newResultValue<=0,0.01,newResultValue),
              Year=lubridate::year(DateTime),
              Month=lubridate::month(DateTime),
              WaterYear=ifelse(Month>=10,Year+1,Year),
              FakeDate=as.Date(paste(2000,Month,lubridate::day(DateTime),sep='-')),
              WY_FakeDate=as.Date(if_else(Month>=10,FakeDate-lubridate::years(1),FakeDate))) |>
    #fix differences in parameter names
    mutate(parameter=dplyr::case_when(
       parameter=='Phosphorus' ~ 'Total Phosphorus',
       parameter=='Dissolved oxygen (DO)'&unit=='%' ~ 'Dissolved Oxygen (saturation)',
       parameter=='Dissolved oxygen (DO)' ~ 'Dissolved Oxygen',
       parameter=='Escherichia coli' ~ 'E. coli',
       parameter=="Temperature, water" ~ "Water Temperature (°C)",
       parameter=='Chlorophyll' ~ 'Chlorophyll a',
      parameter=="Nitrogen, mixed forms (NH3), (NH4), organic, (NO2) and (NO3)" ~ 'Total Nitrogen',
      parameter=="Light attenuation at measurement depth"~'Secchi Depth',
      T ~ parameter
    )) |>
    arrange(SITE_CODE,DateTime,parameter,desc(depth))
  return(wqx_out)
}

# WQX_PULL(startYear = 2000,endYear = 2022,project='Ambient_Water_Quality_Lakes') |>
#    wqx_cleanup() %>%
#   select(parameter) %>%
#   unique()
# 
# wqx_siteInfo(project='Ambient_Water_Quality_Lakes') %>%
#   transmute(
#     SITE_CODE=MonitoringLocationIdentifier,
#     SITE_NAME=MonitoringLocationName,
#     LAT=as.numeric(LatitudeMeasure),
#     LON=as.numeric(LongitudeMeasure)
#   )
