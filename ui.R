# Load Data ---------------------------------------------------------------
lake_sites <- read_parquet("outputs/lake_sites.parquet")
lakes_list <- readRDS("outputs/sites_list.RDS")
years_list <- readRDS("outputs/years_list.RDS")
parm_list <- readRDS("outputs/parm_list.RDS")

# User Interface ----------------------------------------------------------
ui <-
  dashboardPage(
    title = paste0(
      "Thurston County Lakes Water Quality Data Dashboard - BETA (",
      Sys.Date(),
      ")"
    ),
    dashboardHeader(
      title = img(src = "TC2.png", height = 120),
      tags$li(
        class = "dropdown",
        tags$style(".main-header {max-height: 135px}"),
        tags$style(".main-header .logo {height: 135px}")
      ),
      tags$li(
        a(
          strong("ABOUT"),
          height = 50,
          href = "https://github.com/HerreraEnvironmental/23-08082-000-TC-WQ-Dashboard",
          title = "",
          target = "_blank"
        ),
        class = "dropdown"
      )
    ),
    dashboardSidebar(
      tags$style(".left-side, .main-sidebar {padding-top: 135px}"),
      tags$style(HTML(".sidebar-menu li a { font-size: 16px; }")),
      width = 300,
      sidebarMenu(
        menuItem(
          "Map",
          tabName = "map",
          icon = icon("map-marker", lib = "glyphicon")
        ),
        menuItem(
          text = "Summary of Trophic State Index",
          tabName = "sum_tsi",
          icon = icon("tint", lib = "glyphicon")
        ),
        menuItem(
          text = "Summary of Water Quality Trends ",
          tabName = "trends",
          icon = icon("tint", lib = "glyphicon")
        ),
        menuItem(
          text = "Lake Trophic State Index",
          tabName = "tsi",
          icon = icon("tint", lib = "glyphicon")
        ),
        menuItem(
          text = "Data Visualization and Trends",
          tabName = "all_data",
          icon = icon("signal", lib = "glyphicon")
        ),
        menuItem(
          text = "Data Download",
          tabName = "data_download",
          icon = icon("download-alt", lib = "glyphicon")
        ),
        menuItem(
          text = "Disclaimer",
          tabName = "disclaimer",
          icon = icon("info-sign", lib = "glyphicon")
        )
      )
    ),
    dashboardBody(
      # JS: open tab helper
      tags$script(HTML("
        var openTab = function(tabName){
          $('a', $('.sidebar')).each(function() {
            if (this.getAttribute('data-value') == tabName) {
              this.click()
            };
          });
        }
      ")),
      # Head: CSS and styles consolidated
      tags$head(
        tags$link(
          rel = "stylesheet",
          type = "text/css",
          href = "radar_style.css"
        ),
        tags$style("a { cursor: pointer; }"),
        tags$style(HTML(
          '
          .myClass {
            font-size: 32px;
            line-height: 50px;
            text-align: left;
            font-family: "Helvetica Neue", Helvetica, Arial, sans-serif;
            padding: 0 15px;
            overflow: hidden;
            color: white;
          }
          '
        )),
        tags$style(HTML(".vscomp-dropbox-container { z-index: 99999 !important; }")),
        tags$style(HTML(".vscomp-dropbox { position: absolute; top: 0px !important; }"))
      ),
      includeCSS("www/radar_style.css"),
      # Banner injection
      tags$script(HTML("
        $(document).ready(function() {
          $('header').find('nav')
            .append('<span class=\\'myClass\\'> Thurston County Lakes Water Quality Data Dashboard - BETA </span>');
        })
      ")),
      tabItems(
        tabItem(
          tabName = "map",
          column(12, h2("Map of Lake Monitoring Stations")),
          column(12, hr()),
          fluidRow(
            column(
              8,
              h5(
                "Below are all stations, active and inactive, used to monitor water quality in Thurston County. ",
                "To learn more about a station, click on the icon and follow the prompts to various data tabs."
              )
            )
          ),
          fluidRow(column(12, br())),
          fluidRow(column(12, leafletOutput("map", height = 700, width = "100%"))),
          fluidRow(column(12, br()))
        ),
        tabItem(
          tabName = "sum_tsi",
          column(12, h2("Summary of Trophic State Index")),
          column(12, hr()),
          fluidRow(
            column(8, leafletOutput("tsi_map", height = 800, width = "100%")),
            column(
              4,
              p(paste(
                "Lakes are commonly classified into three trophic states based on their amount of nutrients and algae (Carlson, 1977).",
                "Oligotrophic lakes have the lowest amount of nutrients and are often mountain lakes or lakes in",
                "undisturbed forests. Eutrophic lakes have the highest amount of nutrients and can be naturally",
                "productive but are often highly altered lakes and may have frequent algal blooms. Mesotrophic lakes",
                "have moderate amounts of nutrients and are common in lowland western Washington, especially in",
                "areas with some development along the shoreline and in the watershed"
              )),
              p("Carlson, R.E. 1977. A trophic state index for lakes. Limnology and Oceanography. 22:361-369."),
              selectInput("tsi_sum_year", "Select Year to Highlight", years_list),
              selectInput(
                "tsi_map_parm",
                "Select Trophic State Index to Map",
                c("Chlorophyll a", "Total Phosphorus", "Secchi Depth")
              ),
              plotlyOutput("tsi_summary_plot")
            )
          ),
          fluidRow(column(12, br()))
        ),
        tabItem(
          tabName = "trends",
          column(12, h2("Summary of Water Quality Trends - Surface Only")),
          column(12, hr()),
          sidebarLayout(
            sidebarPanel(
              width = 3,
              p(
                "Explore trends across the landscape. Click on a site on the map to view the long-term",
                "dataset for that site. You may select individual water quality parameters and set the period",
                "of analysis. You may also correct for serial autocorrelation in the Mann-Kendall Trend test and",
                " select individual seasons for analysis."
              ),
              selectInput(
                "trend_summary_parm",
                "Select Parameter for Table and Plot",
                parm_list
              ),
              sliderInput(
                "trend_summary_years",
                "Select Year Range for Trend",
                value = c(min(years_list), max(years_list)),
                min = min(years_list),
                max = max(years_list),
                step = 1,
                sep = ""
              ),
              checkboxInput(
                "rktAuto",
                "Correct for Autocorrelation? (requires 10+ years data)?"
              ),
              selectInput(
                "rktSeason",
                "Select Seasons for Mann-Kendall Test",
                c(
                  "All",
                  "Winter (Jan-Mar)" = "winter",
                  "Spring (Apr-Jun)" = "spring",
                  "Summer (Jul-Sep)" = "summer",
                  "Fall (Oct-Dec)" = "fall"
                )
              ),
              materialSwitch(
                inputId = "trend_summary_log_scale",
                label = "Log-scale?",
                status = "default",
                value = FALSE
              ),
              downloadButton("trends_download", label = "Download Trend Statistics")
            ),
            mainPanel(
              width = 9,
              mainPanel(
                leafletOutput("trend_summary_map", width = "100%"),
                fluidRow(
                  h2("Trend for Selected Site"),
                  column(2),
                  pickerInput("trend_summary_site", "Select Site", lakes_list)
                ),
                vegawidgetOutput("trend_summary_trend_plot"),
                htmlOutput("trend_summary_text")
              )
            )
          ),
          fluidRow(column(12, br()))
        ),
        tabItem(
          tabName = "tsi",
          column(
            12,
            h2("Lake Trophic State Index"),
            a("Return to Map", href = "#", onclick = 'openTab("map")')
          ),
          column(12, hr()),
          fluidRow(
            column(
              12,
              sidebarLayout(
                sidebarPanel(
                  width = 3,
                  p(paste(
                    "Lakes are commonly classified into three trophic states based on their amount of nutrients and algae (Carlson, 1977).",
                    "Oligotrophic lakes have the lowest amount of nutrients and are often mountain lakes or lakes in",
                    "undisturbed forests. Eutrophic lakes have the highest amount of nutrients and can be naturally",
                    "productive but are often highly altered lakes and may have frequent algal blooms. Mesotrophic lakes",
                    "have moderate amounts of nutrients and are common in lowland western Washington, especially in",
                    "areas with some development along the shoreline and in the watershed"
                  )),
                  p("Carlson, R.E. 1977. A trophic state index for lakes. Limnology and Oceanography. 22:361-369."),
                  pickerInput("main_site", "Select Site", lakes_list)
                ),
                mainPanel(
                  width = 9,
                  fluidRow(vegawidgetOutput("tsi_annual")),
                  htmlOutput("tsi_trend_text")
                )
              )
            )
          ),
          fluidRow(column(12, br()))
        ),
        tabItem(
          tabName = "all_data",
          column(
            12,
            h2("Data Visualization and Trends"),
            a("Return to Map", href = "#", onclick = 'openTab("map")')
          ),
          column(12, hr()),
          sidebarLayout(
            sidebarPanel(
              width = 3,
              pickerInput("main_site2", "Select Site", lakes_list, multiple = FALSE),
              selectInput("data_year", "Select Year to Highlight", years_list),
              selectInput("trend_parm", "Select Parameter", parm_list),
              sliderInput(
                "data_month",
                "Select Month(s) for Profile Plots",
                min = 1,
                max = 12,
                step = 1,
                value = c(6, 9)
              ),
              sliderInput(
                "trend_years",
                "Select Year Range for Trend",
                value = c(2000, 2020),
                min = 2000,
                max = 2020,
                step = 1,
                sep = ""
              ),
              sliderInput(
                "trend_depths",
                "Select Depth Range for Trend",
                value = c(0, 10),
                min = 0,
                max = 10,
                step = 0.5,
                sep = ""
              ),
              checkboxInput(
                "rktAuto_oneSite",
                "Correct for Autocorrelation? (requires 10+ years data)?"
              ),
              selectInput(
                "rktSeason_oneSite",
                "Select Seasons for Mann-Kendall Test",
                c(
                  "All",
                  "Winter (Jan-Mar)" = "winter",
                  "Spring (Apr-Jun)" = "spring",
                  "Summer (Jul-Sep)" = "summer",
                  "Fall (Oct-Dec)" = "fall"
                )
              ),
              materialSwitch(
                inputId = "data_log_scale",
                label = "Log-scale?",
                status = "default",
                value = FALSE
              ),
              hr()
            ),
              mainPanel(
              width = 9,
              hr(),
              tabsetPanel(
                tabPanel(
                  "Monthly Lake Water Quality Profiles",
                  h2("Monthly Lake Water Quality Profiles"),
                  fluidRow(column(12, vegawidgetOutput("data_plot")))
                ),
                tabPanel(
                  "Long-term Trend",
                  h2("Long-term Trend"),
                  fluidRow(
                    column(12, vegawidgetOutput("trend_plot")),
                    htmlOutput("trend_text")
                  )
                )
              )
            )
          ),
          fluidRow(column(12, br()))
        ),
        tabItem(
          tabName = "data_download",
          column(
            12,
            h2("Data Download"),
            a("Return to Map", href = "#", onclick = 'openTab("map")')
          ),
          column(12, hr()),
          sidebarLayout(
            sidebarPanel(
              width = 3,
              virtualSelectInput(
                "main_site4",
                "Select Site to Download",
                lakes_list,
                multiple = TRUE,
                search = TRUE
              ),
              virtualSelectInput(
                "params_out",
                "Select Parameter(s)",
                parm_list,
                selected = parm_list,
                multiple = TRUE,
                search = TRUE
              ),
              sliderInput(
                "years_out",
                "Select Year Range for Download",
                value = c(min(years_list), max(years_list)),
                min = min(years_list),
                max = max(years_list),
                step = 1,
                sep = ""
              ),
              downloadButton("downloadData", "Download Data")
            ),
            mainPanel(
              width = 9,
              h4("Data Preview"),
              br(),
              DTOutput("data_view_table")
            )
          ),
          fluidRow(column(12, br()))
        )
      )
    )
  )