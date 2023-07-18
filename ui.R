#ui_Lakes

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

source('wqx_data_query_functions.R',local=T)

lakes_list<-wqx_siteInfo(project='Ambient_Water_Quality_Lakes') %>%
  transmute(
    SITE_CODE=MonitoringLocationIdentifier,
    SITE_NAME=MonitoringLocationName,
    LAT=as.numeric(LatitudeMeasure),
    LON=as.numeric(LongitudeMeasure)
  )

sites_list<-lakes_list$SITE_CODE
names(sites_list)<-lakes_list$SITE_NAME

years_list<-rev(1971:year(Sys.Date()))

# User Interface ----------------------------------------------------------

parm_list<-c('Chlorophyll a','Total Phosphorus','Secchi Depth',
             "Water Temperature (°C)",'Dissolved Oxygen','Specific conductance','pH',
             'Nitrate + Nitrite','Ammonia-nitrogen','Total Nitrogen',
             'Alkalinity, carbonate','Pheophytin')

ui<-tagList(
  tags$head(tags$link(includeScript("func.js"))),
  tags$head(tags$style("a{cursor:pointer;}")),
  
  #UI
  navbarPage(
    theme = bs_theme(version = 4, bootswatch = "yeti"),
    paste0('Thurston County Lakes Water Quality Data Dashboard - BETA (', Sys.Date(),')'),
    tabPanel('Map',value='map',
             fluidRow(column(12,h1("Water Quality Station Map"))),
             fluidRow(column(12, hr())),
             fluidRow(column(8,h5("Below are all stations, active and inactive, used to monitor water quality in Thurston County. To learn more about a station, click on the icon and follow the prompts to various data tabs."))),
             fluidRow(column(12, br())),
             fluidRow(column(12,leafletOutput('map',height=700,width=1600))),
             fluidRow(column(12, br())),
    ),
    tabPanel('Summary of Trophic State Index',value='sum_tsi',
             column(12,h1("Summary of Trophic State Index")),
             column(12, hr()),
             fluidRow(column(8,leafletOutput('tsi_map',height=800,width=1200)),
                      column(4, 
                             selectInput('tsi_sum_year','Select Year to Highlight',years_list),
                             selectInput('tsi_map_parm','Select Trophic State Index to Map',
                                         c('Chlorophyll a','Total Phosphorus','Secchi Depth')),
                             plotlyOutput('tsi_summary_plot')
                      )),
             fluidRow(column(12, br()))
    ),
    
    tabPanel('Summary of Water Quality Trends - Surface Only',value='trends',
             column(12,h1("Water Quality Trends")),
             column(12, hr()),
             sidebarLayout(
               sidebarPanel(width = 3,
                            p('Explore trends across the landscape. Click on a site on the map to view the long-term',
                              'dataset for that site. You may select individual water quality parameters and set the period',
                              'of analysis. You may also correct for serial autocorrelation in the Mann-Kendall Trend test and',
                              ' select individual seasons for analysis.'),
                            selectInput('trend_summary_parm','Select Parameter for Table and Plot',
                                        parm_list),
                            sliderInput('trend_summary_years','Select Year Range for Trend',
                                        value=c(min(years_list),max(years_list)),
                                        min=min(years_list),max=max(years_list),
                                        step=1,sep=''),
                            checkboxInput('rktAuto','Correct for Autocorrelation? (requires 10+ years data)?'),
                            selectInput('rktSeason','Select Seasons for Mann-Kendall Test',
                                        c('All','Winter (Jan-Mar)'='winter','Spring (Apr-Jun)'='spring',
                                          'Summer (Jul-Sep)'='summer','Fall (Oct-Dec)'='fall')),
                            materialSwitch(inputId = "trend_summary_log_scale", label = "Log-scale?", status = "default",value=F),
                            downloadButton('trends_download',label='Download Trend Statistics')
               ),
               mainPanel(width = 9,
                         mainPanel(
                           leafletOutput('trend_summary_map'),
                           fluidRow(h2('Trend for Selected Site'),
                                    column(2),
                                    pickerInput('trend_summary_site','Select Site',sites_list)),
                           plotlyOutput('trend_summary_trend_plot')
                         )
               )),
             fluidRow(column(12, br()))
    ), 
    tabPanel('TSI',value='tsi',
             column(12,h1("Trophic State Index")),
             column(12, hr()),
             fluidRow(column(12,sidebarLayout(
               sidebarPanel(width=3,
                            pickerInput('main_site','Select Site',sites_list),
                            selectInput('tsi_year','Select Year to Highlight',years_list),
                            sliderInput('tsi_trend_years','Select Year Range for Trend',value=c(min(years_list),max(years_list)),
                                        min=min(years_list),max=max(years_list),
                                        step=1,sep='')
               )
               ,
               mainPanel(width=9,
                         fluidRow(plotlyOutput('tsi_annual'))
                         
               ))
               
             )),
             fluidRow(column(12, br()))
    ),
    
    tabPanel('Data Visualization and Trends',value='all_data',
             column(12,h1("All Data Viewer")),
             column(12, hr()),
             sidebarLayout(
               sidebarPanel(width = 3,
                            pickerInput('main_site2','Select Site',sites_list, multiple = F),
                            selectInput('data_year','Select Year to Highlight',years_list),
                            selectInput('trend_parm','Select Parameter',parm_list),
                            sliderInput('trend_years','Select Year Range for Trend',value=c(2000,2020),
                                        min=2000,max=2020,
                                        step=1,sep=''),
                            sliderInput('trend_depths','Select Depth Range for Trend',value=c(0,10),
                                        min=0,max=10,
                                        step=0.5,sep=''),
                            checkboxInput('rktAuto_oneSite','Correct for Autocorrelation? (requires 10+ years data)?'),
                            selectInput('rktSeason_oneSite','Select Seasons for Mann-Kendall Test',
                                        c('All','Winter (Jan-Mar)'='winter','Spring (Apr-Jun)'='spring',
                                          'Summer (Jul-Sep)'='summer','Fall (Oct-Dec)'='fall')),
                            materialSwitch(inputId = "data_log_scale", label = "Log-scale?", status = "default",value=F),
                            hr(),
                            h2('Water Quality Criteria Comparison for Selected Year'),
                            tableOutput('wqc_site')
                            
               ),
               mainPanel(width = 9,
                         plotlyOutput('data_plot'),
                         plotlyOutput('trend_plot'),
                         htmlOutput('trend_text')
               )),
             fluidRow(column(12, br()))
    ),
    
    tabPanel('Data Download', value = 'data_download',
             column(12,h1("Data Download")),
             column(12, hr()),
             sidebarLayout(
               sidebarPanel(width = 3,
                            pickerInput('main_site4','Select Site to Download',sites_list, multiple = T,
                                        options = pickerOptions(
                                          actionsBox = TRUE, 
                                          size = 10,
                                          selectedTextFormat = "count > 3"
                                        )),
                            pickerInput('params_out', "Select Parameter(s)", parm_list,selected=parm_list, multiple = TRUE,
                                        options = pickerOptions(
                                          actionsBox = TRUE, 
                                          size = 10,
                                          selectedTextFormat = "count > 3"
                                        )),
                            sliderInput('years_out','Select Year Range for Download', 
                                        value=c(min(years_list),max(years_list)),
                                        min=min(years_list),max=max(years_list),
                                        step=1,sep=''),
                            downloadButton('downloadData', "Download Data")
               ),
               mainPanel(width = 9,
                         h4("Data Preview"),
                         br(),
                         DTOutput('data_view_table')
               )),
             fluidRow(column(12, br()))
    )
    
    ,id='navbarpanel')
)
