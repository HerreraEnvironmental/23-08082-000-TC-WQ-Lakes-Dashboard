#server lakes

library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(shiny)
library(leaflet)
library(bslib)
library(DT)
library(shinyWidgets)
library(purrr)
library(tidyr)
library(rkt)


log10_minor_break = function (...){
  function(x) {
    minx         = floor(min(log10(x), na.rm=T))-1;
    maxx         = ceiling(max(log10(x), na.rm=T))+1;
    n_major      = maxx-minx+1;
    major_breaks = seq(minx, maxx, by=1)
    minor_breaks = 
      rep(log10(seq(1, 9, by=1)), times = n_major)+
      rep(major_breaks, each = 9)
    return(10^(minor_breaks))
  }
}
# Data Load ---------------------------------------------------------------
source('wqx_data_query_functions.R',local=T)
source('lake_functions/lake_site_map.R',local=T)
source('lake_functions/lake_trends.R',local=T)
source('lake_functions/lakes_profile_plot_function.R',local=T)
source('lake_functions/tsi_calc_plot.R',local=T)
source('lake_functions/tsi_map.R',local=T)
 source('functions/trend_summary_and_plot.R',local=T)

 source('functions/trend_shiny_functions.R',local=T)


lakes_list<-wqx_siteInfo(project='Ambient_Water_Quality_Lakes') %>%
  transmute(
    SITE_CODE=MonitoringLocationIdentifier,
    SITE_NAME=MonitoringLocationName,
    LAT=as.numeric(LatitudeMeasure),
    LON=as.numeric(LongitudeMeasure)
  )

lakes_wq_dat<- WQX_PULL(project='Ambient_Water_Quality_Lakes') %>%
  wqx_cleanup()
lakes_tsi_data<-lakes_wq_dat %>%
  group_by(SITE_CODE) %>%
  tidyr::nest() %>%
  mutate(TSI_out=map(.x=data,.f=~tsi_calc(.x))) %>%
  select(-data) %>%
  tidyr::unnest(TSI_out)

years_list<-sort(unique(lakes_wq_dat$Year))

parm_list<-c('Chlorophyll a','Total Phosphorus','Secchi Depth',
             "Water Temperature (°C)",'Dissolved Oxygen','Specific conductance','pH',
             'Nitrate + Nitrite','Ammonia-nitrogen','Total Nitrogen',
             'Alkalinity, carbonate','Pheophytin')

server<-function(input,output,session){
  #OPENER TAB
  output$map<-renderLeaflet({
    site_map(lakes_list)
  })
  #TSI MAP TAB
  annual_tsi<-reactive({
    lakes_tsi_data %>%
      filter(Year==input$tsi_sum_year)
  })
  
  output$tsi_map<-renderLeaflet({
    tsi_map(lakes_list,annual_tsi(),input$tsi_sum_year,plotParm=input$tsi_map_parm)
  })
  output$tsi_summary_plot<-renderPlotly({
    tsi_summary_plot(annual_tsi())
  })
  
  
  #TRENDS TAB
  trend_summary<-reactive({
    trend_summary_func(lakes_wq_dat,input)
  })
  
  output$trend_summary_map<-renderLeaflet({
    trend_summary_map(trend_summary(),streams_sites,input)
  })
  
  output$trend_summary_trend_plot<-renderPlotly({
    trend_summary_trend_plot(lakes_wq_dat,input)
  })
  
  output$trend_summary_plot<-renderPlotly({
    trend_summary_plot(trend_summary(),input)
  })
  
  observeEvent(input$trend_summary_map_marker_click, {
    p <- input$trend_summary_map_marker_click
    if(!is.null(p$id)){
      updateSelectInput(session, "trend_summary_site", 
                        selected =p$id)
    }
  })
  
  output$trend_summary_table<-renderText({
    paste('')
    
  })
  
  output$trends_download <- downloadHandler(
    filename = function() {
      paste('trends-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(trend_summary() %>% select(SITE_CODE,parameter,StartYear,EndYear,Season,CorrectedForAutocorrelation,
                                           `p-value`=p,`Slope (units/years)`=Slope),
                con,row.names = F)
    }
  )
  
 
  #Individual  Site Water Quality 
 
  
  dataSubset<-reactive({
   lakes_wq_dat %>%
      filter(SITE_CODE==input$main_site)
  })
  
  output$data_plot<-renderPlotly({
    withinYear_plot(dataSubset(),input)
  })
  
  output$trend_plot<-renderPlotly({
    trend_plot(dataSubset(),input)
  })
  
  output$trend_text<-renderUI({
    trend_text(dataSubset(),input)
  })
  
  
  #DAta Download TAb
  dataout_data<-reactive({
    lakes_wq_dat %>%
      filter(SITE_CODE %in% input$main_site4&
               parameter==input$params_out&
               Year>=input$years_out[1]&Year<=input$years_out[2])%>% 
      select(SITE_CODE,SITE_NAME,DateTime,parameter,depth,value,unit,mdl)
  })
  
  output$data_view_table <- renderDT({
    if(is.null(input$params_out)){
    } else{
      datatable(head(dataout_data(),20), 
                escape = FALSE,
                options = list(
                  scrollX = TRUE,
                  dom = 't',
                  autoWidth = TRUE
                ),
                rownames= FALSE)
    }
  })
  
  
  output$downloadData <- downloadHandler(
    
    filename = function() { 
      paste("ThurstonCoWQData_", as.character(Sys.Date()), ".csv", sep="")
    },
    content = function(file) {
      write.csv(dataout_data() ,
                file, row.names = F)
    }
  )  
  
  
  
  ##updates for tabs
  observe({
    updateSelectInput(session,
                      'main_site',
                      selected = input$main_site
    )
  })
  
  observe({
    updateSelectInput(session,
                      'main_site2',
                      selected = input$main_site
    )
  })
  
  observe({
    updateSliderInput(session,
                      'trend_years',
                      min=min(dataSubset()$Year),
                      max=max(dataSubset()$Year),
                      value=c(min(dataSubset()$Year),
                              max(dataSubset()$Year))
    )
  })
  observe({
    updateSelectInput(session,
                      'trend_parm',
                      choices=parm_list[
                        parm_list %in% (lakes_wq_dat %>% filter(SITE_CODE==input$main_site) %>% 
                                          pull(parameter) %>% unique())]
    )
  })
  observe({
    updateSelectInput(session,
                      'data_year2',
                      choices=lakes_wq_dat %>% filter(SITE_CODE==input$main_site) %>% pull(Year) %>% unique()
    )
  })
  
  observe({
    updateSelectInput(session,
                      'data_year',
                      choices = sort(unique(dataSubset()$Year),T)
    )
  })
  
  
  observe({
    updateSelectInput(session,
                      'data_parm',
                      choices=parm_list[
                        parm_list %in% (lakes_wq_dat %>% filter(SITE_CODE==input$main_site) %>% pull(parameter) %>% unique())]
    )
  })
  # MAP 1 updates all variables
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    updateSelectInput(session, "main_site", 
                      selected = click$id)
    updateSelectInput(session, "main_site2", 
                      selected = click$id)
    updateSelectInput(session, "main_site3", 
                      selected = click$id)
    updateSelectInput(session, "main_site4", 
                      selected = click$id)
    updateSelectInput(session, "main_site5", 
                      selected = click$id)
  })
  # MAP 2 updates all variables
  observeEvent(input$wqi_map_marker_click, {
    click <- input$wqi_map_marker_click
    updateSelectInput(session, "main_site", 
                      selected = click$id)
    updateSelectInput(session, "main_site2", 
                      selected = click$id)
    updateSelectInput(session, "main_site3", 
                      selected = click$id)
    updateSelectInput(session, "main_site4", 
                      selected = click$id)
    updateSelectInput(session, "main_site5", 
                      selected = click$id)
  })
  # Dropdown 1 updates all variables
  observeEvent(input$main_site, {
    updateSelectInput(session, "main_site2",
                      selected = input$main_site)
    updateSelectInput(session, "main_site3",
                      selected = input$main_site)
    updateSelectInput(session, "main_site4",
                      selected = input$main_site)
    updateSelectInput(session, "main_site5",
                      selected = input$main_site)
  })
  # Dropdown 2 updates all variables
  observeEvent(input$main_site2, {
    updateSelectInput(session, "main_site",
                      selected = input$main_site2)
    updateSelectInput(session, "main_site3",
                      selected = input$main_site2)
    updateSelectInput(session, "main_site4",
                      selected = input$main_site2)
    updateSelectInput(session, "main_site5",
                      selected = input$main_site2)
  })
  
}