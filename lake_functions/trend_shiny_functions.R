## Trend Plot and text summary for water quality data

# Helper: filter by selected season (no change if "All")
.season_filter <- function(df, season) {
  if (season == "winter")  return(df |> dplyr::filter(Month >= 1  & Month <= 3))
  if (season == "spring")  return(df |> dplyr::filter(Month >= 4  & Month <= 6))
  if (season == "summer")  return(df |> dplyr::filter(Month >= 7  & Month <= 9))
  if (season == "fall")    return(df |> dplyr::filter(Month >= 10 & Month <= 12))
  df
}

# Helper: add horizontal criteria lines depending on parameter
.add_criteria_lines <- function(p, parm, use_group) {
  if (parm == "Water Temperature (°C)") {
    temp_criteria <- wqc_finder(unique(use_group), parm)
    return(p + geom_hline(yintercept = temp_criteria))
  }
  if (parm == "Dissolved Oxygen") {
    do_criteria <- wqc_finder(unique(use_group), parm)
    return(p + geom_hline(yintercept = do_criteria))
  }
  if (parm == "pH") {
    return(p + geom_hline(yintercept = c(6.5, 8.5)))
  }
  if (parm == "Fecal Coliform") {
    return(p + geom_hline(yintercept = c(100, 200)))
  }
  if (parm == "E. coli") {
    return(p + geom_hline(yintercept = c(100, 320)))
  }
  p
}

trend_plot <- function(dataSubset, input) {
  dataSubset <- .season_filter(dataSubset, input$rktSeason_oneSite)

  trendplot <- dataSubset |>
    ggplot(aes(x = DateTime, y = value)) +
    geom_point(data = ~ dplyr::filter(.x, WaterYear == input$data_year), col = "red", size = 4) +
    geom_point() +
    geom_smooth(
      data = ~ dplyr::filter(
        .x,
        WaterYear >= input$trend_years[1] & WaterYear <= input$trend_years[2]
      ),
      se = FALSE
    ) +
    theme_bw() +
    scale_y_continuous(input$trend_parm)

  trendplot <- .add_criteria_lines(trendplot, input$trend_parm, dataSubset$AquaticLifeUse)

  if (input$data_log_scale) {
    trendplot <- trendplot +
      scale_y_log10(
        input$trend_parm,
        breaks = 10^(-4:4),
        minor_breaks = log10_minor_break()
      )
  }

  ggplotly(trendplot)
}

trend_text <- function(dataSubset, input) {
  dataSubset <- .season_filter(dataSubset, input$rktSeason_oneSite)

  trend_out <- dataSubset |>
    dplyr::filter(
      WaterYear >= input$trend_years[1] &
        WaterYear <= input$trend_years[2]
    ) |>
    with(rkt::rkt(WaterYear, newResultValue, Month,
                  correct = input$rktAuto_oneSite, rep = "a"))

  trend_unit <- unique(dataSubset$unit)[1]
  p.value <- ifelse(input$rktAuto_oneSite, trend_out$sl.corrected, trend_out$sl)

  if (is.na(p.value)) {
    return(HTML(paste0(
      "<u>Mann-Kendall Trend Test:</u><br/>",
      "There are not enough data to evaluate trends."
    )))
  }

  sigStatement <- paste0(
    ifelse(p.value <= 0.05, "a  significant trend", "insufficient evidence of a trend"),
    " (p",
    ifelse(p.value < 0.001, "<0.001)", paste0("=", round(p.value, 3), ")"))
  )

  slopeStatement <- ifelse(
    p.value <= 0.05,
    paste("The trend slope is", round(trend_out$B, 4), trend_unit, "per year"),
    ""
  )

  HTML(paste0(
    "<u>Mann-Kendall Trend Test:</u><br/>",
    "Between water years <b>", input$trend_years[1], "</b> and <b>", input$trend_years[2], "</b>",
    ifelse(
      input$rktSeason_oneSite == "All",
      "",
      paste0(" (", input$rktSeason_oneSite, ")")
    ),
    " at ", input$main_site2, ", there is <b>", sigStatement, "</b>",
    " in <b>", input$trend_parm, "</b><br/>",
    slopeStatement
  ))
}