#lake TSI function

tsi_calc<-function(data,epi_depth=5,startMonth=6,endMonth=10){
  data |>
    dplyr::filter(parameter %in% c('Secchi Depth','Total Phosphorus','Chlorophyll a')&
                    Month>=startMonth&Month<=endMonth&
                    depth<=epi_depth) |>
    dplyr::group_by(Year,parameter) |>
    dplyr::summarise(SummerMean=mean(newResultValue,na.rm=T),.groups='drop') |>
    dplyr::ungroup() |>
    dplyr::mutate(TSI=round(ifelse(
      parameter=='Total Phosphorus', 14.42*log(SummerMean*1000) +4.15,
     ifelse(parameter=='Chlorophyll a', 9.81*log(SummerMean) + 30.6,
    ifelse( parameter=='Secchi Depth' , 60- 14.41*log(SummerMean),
     NA))),1)
    )
}

tsi_plot<-function(data,...){
  tsi_data<-tsi_calc(data,...)
    
  tsi_lines<-data.frame(
    bottom=0,
    oligo.meso=40,
    meso.eutro=60,
    top=80
  )
  
  oligo<-vl_chart() %>%
    vl_mark_rect(opacity = .2,color='blue') %>%
    vl_encode_y2(field='bottom') %>%
    vl_encode_y(field='oligo.meso:Q')

  meso<-vl_chart() %>%
    vl_mark_rect(opacity = .2,color='yellow') %>%
    vl_encode_y2(field='oligo.meso') %>%
    vl_encode_y(field='meso.eutro:Q') 
  
  eutro<-vl_chart() %>%
    vl_mark_rect(opacity = .2,color='green') %>%
    vl_encode_y(field='top:Q') %>%
    vl_encode_y2(field='meso.eutro') 
  
  tsi_rect<-vl_layer(oligo,meso,eutro)%>%
    vl_add_data(values=tsi_lines)

  
  tsi_plot<-vl_chart() %>%
    vl_add_data(values=tsi_data)|>
    vl_mark_point(filled=T,size=100) |>
    vl_mark_line()|>
    vl_encode_color(field='parameter:N')|>
    vl_encode_y("TSI:Q",
                title='Trophic State Index',
                scale=list(domain=list(20,80))) |>
    vl_encode_x("Year:Q",title='',
                scale=list(domain=c(min(tsi_data$Year)-3,lubridate::year(Sys.Date()))),
                axis=list(format='c')) |>
    vl_encode_tooltip(field='parameter:N') |>
    vl_encode_tooltip(field='Year:Q') |>
    vl_encode_tooltip(field='TSI:Q') |>
    vl_add_properties(width=600,height=400) 
 #tsi_plot 
  #vl_layer(oligo_meso_line,tsi_plot)
  vl_layer(tsi_rect,tsi_plot)
}


# WQX_PULL(startYear = 2000,endYear = 2022,project='Ambient_Water_Quality_Lakes',
#          site_list = c('THURSTONCOUNTY-EH-BLADEL000')) |>
#   wqx_cleanup() %>%
#   tsi_plot(epi_depth=5)

