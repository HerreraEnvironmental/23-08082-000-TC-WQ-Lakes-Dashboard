## Map and summary plot of all tsi data

tsi_map <- function(
  lakes_list,
  annual_tsi,
  selectYear,
  plotParm = c("Chlorophyll a", "Total Phosphorus", "Secchi Depth")
) {
  plotParm <- plotParm[1]

  pal <- colorFactor(
    c("green", "yellow", "blue", "grey"),
    levels = c("Eutrophic", "Mesotrophic", "Oligotrophic", NA)
  )

  selectIndex <- annual_tsi %>%
    dplyr::filter(parameter == plotParm) %>%
    dplyr::mutate(
      parameter = factor(parameter, levels = c("Chlorophyll a", "Total Phosphorus", "Secchi Depth"))
    )

  annual_tsi %>%
    tidyr::pivot_wider(
      id_cols = c(SITE_CODE),
      names_from = parameter,
      values_from = TSI,
      names_expand = TRUE
    ) %>%
    dplyr::left_join(selectIndex) %>%
    dplyr::left_join(lakes_list) %>%
    dplyr::mutate(
      Category = ifelse(
        TSI >= 50, "Eutrophic",
        ifelse(TSI >= 40, "Mesotrophic", "Oligotrophic")
      )
    ) %>%
    leaflet::leaflet() %>%
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
    ) %>%
    leaflet::addProviderTiles("Esri.NatGeoWorldMap") %>%
    leaflet::addLegend(
      pal = pal,
      values = factor(
        c("Eutrophic", "Mesotrophic", "Oligotrophic"),
        levels = c("Eutrophic", "Mesotrophic", "Oligotrophic")
      ),
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