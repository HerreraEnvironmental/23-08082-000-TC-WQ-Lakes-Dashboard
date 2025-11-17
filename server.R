log10_minor_break <- function(...) {
  function(x) {
    minx <- floor(min(log10(x), na.rm = TRUE)) - 1
    maxx <- ceiling(max(log10(x), na.rm = TRUE)) + 1
    n_major <- maxx - minx + 1
    major_breaks <- seq(minx, maxx, by = 1)
    minor_breaks <-
      rep(log10(seq(1, 9, by = 1)), times = n_major) +
      rep(major_breaks, each = 9)
    10^(minor_breaks)
  }
}

# Data Load ---------------------------------------------------------------
source("lake_functions/lake_site_map.R", local = TRUE)
source("lake_functions/lake_trends.R", local = TRUE)
source("lake_functions/lakes_profile_plot_function.R", local = TRUE)
source("lake_functions/tsi_calc_plot.R", local = TRUE)
source("lake_functions/tsi_map.R", local = TRUE)
source("lake_functions/trend_summary_and_plot.R", local = TRUE)
source("lake_functions/trend_shiny_functions.R", local = TRUE)

lake_sites <- read_parquet("outputs/lake_sites.parquet")
lakes_list <- readRDS("outputs/sites_list.RDS")
lakes_wq_dat <- read_parquet("outputs/lakes_wq_dat.parquet")
years_list <- readRDS("outputs/years_list.RDS")
parm_list <- readRDS("outputs/parm_list.RDS")

lakes_tsi_data <- lakes_wq_dat |>
  dplyr::group_by(SITE_CODE) |>
  tidyr::nest() |>
  dplyr::mutate(TSI_out = purrr::map(.x = data, .f = ~ tsi_calc(.x))) |>
  dplyr::select(-data) |>
  tidyr::unnest(TSI_out)

server <- function(input, output, session) {
  # Helpers ---------------------------------------------------------------
  update_all_site_inputs <- function(value) {
    updateSelectInput(session, "main_site", selected = value)
    updateSelectInput(session, "main_site2", selected = value)
    updateSelectInput(session, "main_site3", selected = value)
    updateSelectInput(session, "main_site4", selected = value)
    updateSelectInput(session, "main_site5", selected = value)
  }

  observe_site_click <- function(click_input_id) {
    observeEvent(input[[click_input_id]], {
      click <- input[[click_input_id]]
      if (!is.null(click$id)) {
        update_all_site_inputs(click$id)
      }
    })
  }

  observe_site_select <- function(source_id, targets) {
    observeEvent(input[[source_id]], {
      lapply(targets, function(tgt) {
        updateSelectInput(session, tgt, selected = input[[source_id]])
      })
    }, ignoreInit = TRUE)
  }

  # OPENER TAB ------------------------------------------------------------
  output$map <- renderLeaflet({
    site_map(lake_sites)
  })

  # TSI MAP TAB -----------------------------------------------------------
  updateSelectInput(session, "tsi_sum_year", 
  choices = sort(unique(lakes_tsi_data$Year),decreasing = T),
  selected=    max(lakes_tsi_data$Year))
  
  annual_tsi <- reactive({
    lakes_tsi_data |>
      dplyr::filter(Year == input$tsi_sum_year)
  })

  output$tsi_map <- renderLeaflet({
    tsi_map(lake_sites, annual_tsi(), input$tsi_sum_year, plotParm = input$tsi_map_parm)
  })

  output$tsi_summary_plot <- renderPlotly({
    tsi_summary_plot(annual_tsi())
  })


  # TRENDS TAB ------------------------------------------------------------
  trend_summary <- reactive({
    trend_summary_func(lakes_wq_dat |>
                         dplyr::filter(depth <= 1), input)
  })

  output$trend_summary_map <- renderLeaflet({
    trend_summary_map(trend_summary(), lake_sites, input)
  })

  output$trend_summary_trend_plot <- renderVegawidget({
    lake_trends_plot(
      lakes_wq_dat |>
        dplyr::filter(SITE_CODE == input$trend_summary_site & depth <= 1),
      site = input$trend_summary_site,
      parm = input$trend_summary_parm,
      rktSeason = input$rktSeason,
      startYear = input$trend_summary_years[1],
      endYear = input$trend_summary_years[2],
      minDepth = 0,
      maxDepth = 1,
      rktAuto = input$rktAuto,
      logPlot = input$trend_summary_log_scale
    )
  })

  output$trend_summary_text <- renderUI({
    lake_trends(
      lakes_wq_dat |>
        dplyr::filter(SITE_CODE == input$trend_summary_site & depth <= 1),
      site = input$trend_summary_site,
      parm = input$trend_summary_parm,
      rktSeason = input$rktSeason,
      startYear = input$trend_summary_years[1],
      endYear = input$trend_summary_years[2],
      minDepth = 0,
      maxDepth = 1,
      rktAuto = input$rktAuto
    )
  })

  observeEvent(input$trend_summary_map_marker_click, {
    p <- input$trend_summary_map_marker_click
    if (!is.null(p$id)) {
      updateSelectInput(session, "trend_summary_site", selected = p$id)
    }
  })

  output$trend_summary_table <- renderText({
    paste("")
  })

  output$trends_download <- downloadHandler(
    filename = function() {
      paste("trends-", Sys.Date(), ".csv", sep = "")
    },
    content = function(con) {
      write.csv(
        trend_summary() |>
          dplyr::select(
            SITE_CODE, parameter, StartYear, EndYear, Season, CorrectedForAutocorrelation,
            `p-value` = p, `Slope (units/years)` = Slope
          ),
        con,
        row.names = FALSE
      )
    }
  )

  # TSI -------------------------------------------------------------------
  output$tsi_annual <- renderVegawidget({
    tsi_plot(
      data = lakes_wq_dat |>
        dplyr::filter(SITE_CODE == input$main_site),
      epi_depth = 5
    )
  })

  output$tsi_trend_text <- renderUI({
    tsi_trend_text(
      tsi_trend_summary = tsi_trend_summary_func(lakes_tsi_data),
      input
    )
  })

  # Individual Site Water Quality ----------------------------------------
  dataSubset <- reactive({
    lakes_wq_dat |>
      dplyr::filter(SITE_CODE == input$main_site)
  })

  output$data_plot <- renderVegawidget({
    lake_profile_plot(
      dataSubset(),
      parm = input$trend_parm,
      month = input$data_month,
      year = input$data_year,
      profile_log = input$data_log_scale,
      maxDepth = max(dataSubset()$depth, na.rm = TRUE)
    )
  })

  output$trend_plot <- renderVegawidget({
    lake_trends_plot(
      dataSubset(),
      site = input$main_site,
      parm = input$trend_parm,
      rktSeason = input$rktSeason_oneSite,
      startYear = input$trend_years[1],
      endYear = input$trend_years[2],
      minDepth = input$trend_depths[1],
      maxDepth = input$trend_depths[2],
      rktAuto = input$rktAuto_oneSite,
      logPlot = input$data_log_scale
    )
  })

  output$trend_text <- renderUI({
    lake_trends(
      dataSubset(),
      site = input$main_site,
      parm = input$trend_parm,
      rktSeason = input$rktSeason_oneSite,
      startYear = input$trend_years[1],
      endYear = input$trend_years[2],
      minDepth = input$trend_depths[1],
      maxDepth = input$trend_depths[2],
      rktAuto = input$rktAuto_oneSite
    )
  })

  # Data Download Tab -----------------------------------------------------
  dataout_data <- reactive({
    lakes_wq_dat |>
      dplyr::filter(
        SITE_CODE %in% input$main_site4 &
          parameter %in% input$params_out &
          Year >= input$years_out[1] & Year <= input$years_out[2]
      ) |>
      dplyr::select(SITE_CODE, DateTime, parameter, depth, value, unit, qualifier)
  })

  output$data_view_table <- renderDT({
    if (is.null(input$params_out)) {
      # no-op
    } else {
      datatable(
        head(dataout_data(), 20),
        escape = FALSE,
        options = list(
          scrollX = TRUE,
          dom = "t",
          autoWidth = FALSE
        ),
        rownames = FALSE
      )
    }
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("ThurstonCoWQData_", as.character(Sys.Date()), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dataout_data(), file, row.names = FALSE)
    }
  )

  # Updates for tabs ------------------------------------------------------
  observe({
    updateSelectInput(session, "main_site", selected = input$main_site)
  })

  observe({
    updateSelectInput(session, "main_site2", selected = input$main_site)
  })

  observe({
    updateSliderInput(
      session,
      "trend_years",
      min = min(dataSubset()$Year),
      max = max(dataSubset()$Year),
      value = c(min(dataSubset()$Year), max(dataSubset()$Year))
    )
  })

  observe({
    updateSliderInput(
      session,
      "trend_depths",
      min = min(dataSubset()$depth, na.rm = TRUE),
      max = max(dataSubset()$depth, na.rm = TRUE),
      value = c(
        min(dataSubset()$depth, na.rm = TRUE),
        max(dataSubset()$depth, na.rm = TRUE)
      )
    )
  })

  observe({
    updateSelectInput(
      session,
      "trend_parm",
      choices = parm_list[
        parm_list %in% (
          lakes_wq_dat |>
            dplyr::filter(SITE_CODE == input$main_site) |>
            dplyr::pull(parameter) |>
            unique()
        )
      ]
    )
  })

  observe({
    updateSelectInput(
      session,
      "data_year2",
      choices = lakes_wq_dat |>
        dplyr::filter(SITE_CODE == input$main_site) |>
        dplyr::pull(Year) |>
        unique()
    )
  })

  observe({
    updateSelectInput(
      session,
      "data_year",
      choices = sort(unique(dataSubset()$Year), TRUE)
    )
  })

  observe({
    updateSelectInput(
      session,
      "data_parm",
      choices = parm_list[
        parm_list %in% (
          lakes_wq_dat |>
            dplyr::filter(SITE_CODE == input$main_site) |>
            dplyr::pull(parameter) |>
            unique()
        )
      ]
    )
  })

  # Map click updates all site selects
  observe_site_click("map_marker_click")
  observe_site_click("wqi_map_marker_click")

  # Dropdown updates keep site selects in sync
  all_site_ids <- c("main_site", "main_site2", "main_site3", "main_site4", "main_site5")
  observe_site_select("main_site", setdiff(all_site_ids, "main_site"))
  observe_site_select("main_site2", setdiff(all_site_ids, "main_site2"))
}