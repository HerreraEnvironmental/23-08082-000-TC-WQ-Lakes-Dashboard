library(vegabrite)

lake_profile_plot <- function(
  data,
  parm,
  month,
  year,
  profile_log = FALSE,
  maxDepth
) {
  # Data subset
  profile_data <- data |>
    dplyr::filter(
      parameter == parm,
      Month >= month[1],
      Month <= month[2]
    ) |>
    dplyr::arrange(depth)

  scale_type <- ifelse(profile_log, "log", "linear")

  # Base points (all years)
  points <- vl_chart() |>
    vl_mark_point(filled = TRUE, size = 50, color = "grey")

  # Selected year (points + line)
  select_points <- vl_chart() |>
    vl_mark_point(filled = TRUE, size = 100, color = "black")

  select_line <- vl_chart() |>
    vl_mark_line(color = "black")

  select_chart <- vl_layer(select_points, select_line) |>
    vl_filter(paste0("datum.Year==", year))

  vl_layer(points, select_chart) |>
    vl_add_data(values = profile_data) |>
    vl_encode_x(
      "value:Q",
      scale = list(type = scale_type),
      title = parm
    ) |>
    vl_encode_y(
      "depth:Q",
      title = "Depth (m)",
      scale = list(reverse = TRUE, domain = c(0, maxDepth))
    ) |>
    vl_encode_order(field = "depth:Q") |>
    vl_encode_tooltip(field = "parameter:N") |>
    vl_encode_tooltip(field = "DateTime:T") |>
    vl_encode_tooltip(field = "depth:Q") |>
    vl_encode_tooltip(field = "value:Q") |>
    vl_add_properties(width = 250, height = 250) |>
    vl_facet(field = "Month", type = "ordinal", columns = 3)
}