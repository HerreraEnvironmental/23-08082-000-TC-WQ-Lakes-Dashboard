## Summary of Water quality trends for a selected parameter

# Helper: filter by selected season (no change if "All")
.season_filter <- function(df, season) {
  if (season == "winter") return(df |> filter(Month >= 1 & Month <= 3))
  if (season == "spring") return(df |> filter(Month >= 4 & Month <= 6))
  if (season == "summer") return(df |> filter(Month >= 7 & Month <= 9))
  if (season == "fall")   return(df |> filter(Month >= 10 & Month <= 12))
  df
}

trend_summary_func <- function(data, input) {
  temp_trend_data <- data |>
    filter(
      WaterYear >= input$trend_summary_years[1] &
        WaterYear <= input$trend_summary_years[2] &
        parameter == input$trend_summary_parm
    )

  temp_trend_data <- .season_filter(temp_trend_data, input$rktSeason)

  temp_trend_data |>
    group_by(SITE_CODE, parameter) |>
    nest() |>
    mutate(
      MK_Out = map(
        .x = data,
        .f = ~ {
          mk_out <- with(
            .x,
            rkt::rkt(
              WaterYear,
              newResultValue,
              Month,
              correct = input$rktAuto,
              rep = "a"
            )
          )
          tibble(
            p = ifelse(input$rktAuto, mk_out$sl.corrected, mk_out$sl),
            Slope = mk_out$B
          ) |>
            mutate(
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
    select(-data) |>
    unnest(MK_Out) |>
    mutate(
      Statement = factor(
        Statement,
        levels = rev(c(
          "Decreasing Trend",
          "Test Not Run - insufficient data",
          "No Significant Trend",
          "Increasing Trend"
        ))
      ),
      # Note: preserves original intent/logic for start/end years
      StartYear = max(
        input$trend_summary_years[1],
        min(temp_trend_data$WaterYear[SITE_CODE == temp_trend_data$SITE_CODE])
      ),
      EndYear = min(
        input$trend_summary_years[2],
        max(temp_trend_data$WaterYear[SITE_CODE == temp_trend_data$SITE_CODE])
      ),
      Season = input$rktSeason,
      CorrectedForAutocorrelation = input$rktAuto
    )
}

trend_summary_map <- function(trend_summary, sites, input) {
  pal_trend <- colorFactor(
    c("blue", "darkgrey", "lightgrey", "red"),
    levels = c(
      "Decreasing Trend",
      "Test Not Run - insufficient data",
      "No Significant Trend",
      "Increasing Trend"
    )
  )

  trend_summary |>
    left_join(sites) |>
    leaflet() |>
    addCircleMarkers(
      fillColor = ~pal_trend(Statement),
      fillOpacity = 0.9,
      weight = 1,
      color = "black",
      popup = ~ paste0(
        "<h6><b>", SITE_NAME, "<br></b></h6><br>",
        SITE_CODE, "<br>",
        "Season: ", input$rktSeason, "<br>",
        "Corrected for Autocorrelation? ", input$rktAuto, "<br>",
        Statement, " (p=", round(p, 4), ")", "<br>",
        StartYear, " to ", EndYear
      ),
      layerId = ~SITE_CODE,
      label = ~SITE_CODE
    ) |>
    addProviderTiles("Esri.NatGeoWorldMap") |>
    addLegend(
      pal = pal_trend,
      values = factor(c(
        "Decreasing Trend",
        "Test Not Run - insufficient data",
        "No Significant Trend",
        "Increasing Trend"
      )),
      title = "Long-term Trend",
      opacity = 1
    )
}

trend_summary_plot <- function(trend_summary, input) {
  plot <- trend_summary |>
    ggplot(aes(x = parameter, fill = Statement)) +
    geom_bar(position = "stack") +
    scale_fill_manual(values = c(
      "Decreasing Trend" = "blue",
      "Test Not Run - insufficient data" = "darkgrey",
      "No Significant Trend" = "lightgrey",
      "Increasing Trend" = "red"
    )) +
    theme_bw() +
    theme(legend.position = "bottom") +
    coord_flip() +
    xlab("")

  ggplotly(plot)
}