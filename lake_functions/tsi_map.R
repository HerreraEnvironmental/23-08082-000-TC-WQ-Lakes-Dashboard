## Map and summary plot of all tsi data

tsi_map <- function(
  lakes_list,
  annual_tsi,
  selectYear,
  plotParm = c("Chlorophyll a", "Total Phosphorus", "Secchi Depth")
) {
  plotParm <- plotParm[1]

  levels_vec <- c("Eutrophic", "Mesotrophic", "Oligotrophic")
  pal <- leaflet::colorFactor(
    palette = c("green", "yellow", "blue"),
    levels = levels_vec,
    na.color = "grey"
  )

  # Keep only the selected year and parameter
  mapped <- annual_tsi |>
    dplyr::filter(Year == selectYear, parameter == plotParm) |>
    dplyr::left_join(lakes_list, by = "SITE_CODE") |>
    dplyr::mutate(
      Category = dplyr::case_when(
        is.na(TSI) ~ NA_character_,
        TSI >= 60 ~ "Eutrophic",          # align with tsi_summary_plot
        TSI >= 40 ~ "Mesotrophic",
        TRUE ~ "Oligotrophic"
      ),
      Category = factor(Category, levels = levels_vec)
    )

  mapped |>
    leaflet::leaflet() |>
    leaflet::addCircleMarkers(
      fillColor = ~pal(Category),
      fillOpacity = 0.9,
      weight = 1,
      color = "black",
      popup = ~ paste0(
        "<h5><b>", SITE_NAME, "<br></b></h5>",
        "<hr>",
        "For ", selectYear, ", the TSI score was <b>", round(TSI, 0), " for ", plotParm,
        "</b>, and is considered <b>", Category, "</b>."
      ),
      layerId = ~SITE_CODE,
      label = ~SITE_CODE
    ) |>
    leaflet::addProviderTiles("Esri.NatGeoWorldMap") |>
    leaflet::addLegend(
      pal = pal,
      values = factor(levels_vec, levels = levels_vec),
      title = paste("Trophic State Index for", plotParm)
    )
}

tsi_summary_plot <- function(annual_tsi) {
  cols <- c(
    "Eutrophic" = "green",
    "Mesotrophic" = "yellow",
    "Oligotrophic" = "blue"
  )

  tsi_summary_plot <- annual_tsi %>%
    dplyr::mutate(
      Category = factor(
        ifelse(
          TSI >= 60, "Eutrophic",
          ifelse(TSI >= 40, "Mesotrophic", "Oligotrophic")
        ),
        levels = c("Eutrophic", "Mesotrophic", "Oligotrophic")
      )
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x = Category)) +
    ggplot2::geom_bar(stat = "count", ggplot2::aes(fill = Category)) +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::ylab("Number of stations") +
    ggplot2::scale_fill_manual(values = cols, drop = FALSE) +
    ggplot2::theme_bw() +
    ggplot2::facet_wrap(~parameter, ncol = 1)

  plotly::ggplotly(tsi_summary_plot)
}