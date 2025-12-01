## Lake Trends tab
# Selects depth range to measure

lake_trends_plot <- function(
  data,
  site,
  parm,
  rktSeason,
  startYear,
  endYear,
  minDepth,
  maxDepth,
  rktAuto,
  logPlot
) {
  temp_trend_data <- data |>
    dplyr::filter(parameter == parm)

  unit_s = paste(unique(temp_trend_data$unit),collapse = '; ')

  if (rktSeason == "winter") {
    temp_trend_data <- temp_trend_data |> dplyr::filter(Month >= 1 & Month <= 3)
  }
  if (rktSeason == "spring") {
    temp_trend_data <- temp_trend_data |> dplyr::filter(Month >= 4 & Month <= 6)
  }
  if (rktSeason == "summer") {
    temp_trend_data <- temp_trend_data |> dplyr::filter(Month >= 7 & Month <= 9)
  }
  if (rktSeason == "fall") {
    temp_trend_data <- temp_trend_data |> dplyr::filter(Month >= 10 & Month <= 12)
  }

  dots <- vl_chart() |>
    vl_encode_color(field = "depth:Q") |>
    vl_encode_y(
      "value:Q",
      title = paste0(parm,'(',unit_s,')'),
      scale = list(type = ifelse(logPlot, "log", "linear"))
    ) |>
    vl_mark_point(filled = TRUE, size = 100) |>
    vl_encode_tooltip(field = "parameter:N") |>
    vl_encode_tooltip(field = "DateTime:T") |>
    vl_encode_tooltip(field = "depth:Q") |>
    vl_encode_tooltip(field = "value:Q") |>
    vl_encode_tooltip(field = "unit:N") 

  line <- vl_chart() |>
    vl_filter(
      filter = paste0(
        "datum.Year>=", startYear,
        "&datum.Year<=", endYear,
        "&datum.depth>=", minDepth,
        "&datum.depth<=", maxDepth
      )
    ) |>
    vl_mark_line(color = "firebrick") |>
    vl_loess(loess = "value", on = "DateTime", bandwidth = 1) |>
    vl_encode_y(
      "value:Q",
      title = paste0(parm,'(',unit_s,')'),
      scale = list(type = ifelse(logPlot, "log", "linear"))
    )

  vl_layer(dots, line) |>
    vl_add_data(values = temp_trend_data) |>
    vl_encode_x("DateTime:T", title = "") |>
    vl_add_properties(width = 600, height = 400)
}

lake_trends <- function(
  data,
  site,
  parm,
  rktSeason,
  startYear,
  endYear,
  minDepth,
  maxDepth,
  rktAuto
) {
  temp_trend_data <- data |>
    dplyr::filter(
      parameter == parm,
      Year >= startYear,
      Year <= endYear,
      depth >= minDepth,
      depth <= maxDepth
    )

  if (rktSeason == "winter") {
    temp_trend_data <- temp_trend_data |> dplyr::filter(Month >= 1 & Month <= 3)
  }
  if (rktSeason == "spring") {
    temp_trend_data <- temp_trend_data |> dplyr::filter(Month >= 4 & Month <= 6)
  }
  if (rktSeason == "summer") {
    temp_trend_data <- temp_trend_data |> dplyr::filter(Month >= 7 & Month <= 9)
  }
  if (rktSeason == "fall") {
    temp_trend_data <- temp_trend_data |> dplyr::filter(Month >= 10 & Month <= 12)
  }

  trend_out <- temp_trend_data |>
    with(rkt::rkt(Year, newResultValue, Month, correct = rktAuto, rep = "a"))

  trend_unit <- unique(temp_trend_data$unit)[1]

  p.value <- ifelse(rktAuto, trend_out$sl.corrected, trend_out$sl)

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

  shiny::HTML(
    paste0(
      "<u>Mann-Kendall Trend Test:</u>", "<br/>",
      "Between water years <b>", startYear, "</b> and <b>", endYear, "</b>",
      ifelse(
        is.na(p.value),
        ", there is inadequate data to conduct a trend test.",
        paste0(
          ifelse(rktSeason == "All", "", paste0(" (", rktSeason, ")")),
          " at ", site, ", there is ",
          "<b>", sigStatement, "</b>",
          " in <b>", parm, "</b><br/>",
          slopeStatement,
          "<br/>",
          "Note values at depths ", minDepth, " to ", maxDepth,
          " m are averaged for each month with no weighting for volume."
        )
      )
    )
  )
}