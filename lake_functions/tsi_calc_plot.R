## Lake TSI function

tsi_calc <- function(
  data,
  epi_depth = 5,
  startMonth = 6,
  endMonth = 10
) {
  data |>
    dplyr::filter(
      parameter %in% c("Secchi Depth", "Water transparency", "Total Phosphorus", "Chlorophyll a"),
      Month >= startMonth,
      Month <= endMonth,
      depth <= epi_depth
    ) |>
    dplyr::group_by(Year, parameter) |>
    dplyr::summarise(SummerMean = mean(newResultValue, na.rm = TRUE), .groups = "drop") |>
    dplyr::ungroup() |>
    dplyr::mutate(
      TSI = round(
        ifelse(
          parameter == "Total Phosphorus", 14.42 * log(SummerMean * 1000) + 4.15,
          ifelse(
            parameter == "Chlorophyll a", 9.81 * log(SummerMean) + 30.6,
            ifelse(
              parameter == "Secchi Depth" | parameter == "Water transparency",
              60 - 14.41 * log(SummerMean),
              NA
            )
          )
        ),
        1
      )
    )
}

tsi_plot <- function(data, ...) {
  tsi_data <- tsi_calc(data, ...)

  tsi_lines <- data.frame(
    bottom = 0,
    oligo.meso = 40,
    meso.eutro = 50,
    top = 80
  )

  oligo <- vl_chart() |>
    vl_mark_rect(opacity = 0.2, color = "blue") |>
    vl_encode_y2(field = "bottom") |>
    vl_encode_y(field = "oligo.meso:Q")

  meso <- vl_chart() |>
    vl_mark_rect(opacity = 0.2, color = "yellow") |>
    vl_encode_y2(field = "oligo.meso") |>
    vl_encode_y(field = "meso.eutro:Q")

  eutro <- vl_chart() |>
    vl_mark_rect(opacity = 0.2, color = "green") |>
    vl_encode_y(field = "top:Q") |>
    vl_encode_y2(field = "meso.eutro")

  tsi_rect <- vl_layer(oligo, meso, eutro) |>
    vl_add_data(values = tsi_lines)

  tsi_plot <- vl_chart() |>
    vl_add_data(values = tsi_data) |>
    vl_mark_point(filled = TRUE, size = 100) |>
    vl_mark_line() |>
    vl_encode_color(field = "parameter:N") |>
    vl_encode_y(
      "TSI:Q",
      title = "Trophic State Index",
      scale = list(domain = list(20, 80))
    ) |>
    vl_encode_x(
      "Year:Q",
      title = "",
      scale = list(domain = c(min(tsi_data$Year) - 3, lubridate::year(Sys.Date()))),
      axis = list(format = "c")
    ) |>
    vl_encode_tooltip(field = "parameter:N") |>
    vl_encode_tooltip(field = "Year:Q") |>
    vl_encode_tooltip(field = "TSI:Q") |>
    vl_add_properties(width = 600, height = 400)

  vl_layer(tsi_rect, tsi_plot)
}

tsi_trend_summary_func <- function(tsi_data) {
  tsi_data |>
    dplyr::group_by(SITE_CODE, parameter) |>
    tidyr::nest() |>
    dplyr::mutate(
      MK_Out = purrr::map(
        .x = data,
        .f = ~ {
          mk_out <- with(.x, rkt::rkt(Year, TSI, rep = "a"))
          tibble::tibble(
            p = mk_out$sl,
            Slope = mk_out$B
          ) |>
            dplyr::mutate(
              Statement = ifelse(
                is.na(Slope),
                "Test Not Run - insufficient data",
                ifelse(
                  p > 0.05 | Slope == 0,
                  "No Significant Trend",
                  ifelse(Slope > 0, "Increasing Trend", "Decreasing Trend")
                )
              )
            )
        }
      )
    ) |>
    dplyr::select(-data) |>
    tidyr::unnest(MK_Out) |>
    dplyr::mutate(
      Statement = factor(
        Statement,
        levels = rev(c(
          "Decreasing Trend",
          "Test Not Run - insufficient data",
          "No Significant Trend",
          "Increasing Trend"
        ))
      ),
      StartYear = min(tsi_data$Year[SITE_CODE == tsi_data$SITE_CODE & parameter == tsi_data$parameter]),
      EndYear = max(tsi_data$Year[SITE_CODE == tsi_data$SITE_CODE & parameter == tsi_data$parameter]),
      sigStatement = paste0(
        ifelse(p <= 0.05, "a  significant trend", "insufficient evidence of a trend"),
        " (p", ifelse(p < 0.001, "<0.001)", paste0("=", round(p, 3), ")"))
      ),
      slopeStatement = ifelse(
        p <= 0.05,
        paste("The trend slope is", round(Slope, 4), "units per year"),
        ""
      )
    )
}

tsi_trend_text <- function(tsi_trend_summary, input) {
  tsi_trend_out <- tsi_trend_summary |>
    dplyr::filter(SITE_CODE == input$main_site)

  HTML(paste0(
    "For TSI - ", tsi_trend_out$parameter, ", there is ",
    ifelse(
      is.na(tsi_trend_out$p),
      "inadequate data to evalute a trend",
      paste0(tsi_trend_out$sigStatement, "<br/>", tsi_trend_out$slopeStatement)
    ),
    "<br/>"
  ))
}