data("steps", package = "tsn")
data("motivation", package = "tsn")

threshold_network <- tsn(steps, value = "steps", id = "id", time = "day", series = 536, unit = "state", visibility = "horizontal", discretization = "threshold", breaks = c(10000, 16500))
width_network <- tsn(steps, value = "steps", id = "id", time = "day", series = 536, unit = "state", visibility = "horizontal", discretization = "width")
quantile_network <- tsn(steps, value = "steps", id = "id", time = "day", series = 536, unit = "state", visibility = "horizontal", discretization = "quantile")
kde_network <- tsn(steps, value = "steps", id = "id", time = "day", series = 536, unit = "state", visibility = "horizontal", discretization = "kde")
kmeans_network <- tsn(steps, value = "steps", id = "id", time = "day", series = 536, unit = "state", visibility = "horizontal", discretization = "kmeans", seed = 1707)
gaussian_network <- tsn(steps, value = "steps", id = "id", time = "day", series = 536, unit = "state", visibility = "horizontal", discretization = "gaussian", seed = 1707)
hclust_network <- tsn(steps, value = "steps", id = "id", time = "day", series = 536, unit = "state", visibility = "horizontal", discretization = "hclust")
ordinal_network <- tsn(steps, value = "steps", id = "id", time = "day", series = 536, unit = "state", visibility = "horizontal", discretization = "ordinal", m = 3, tau = 1)
symbolic_network <- tsn(steps, value = "steps", id = "id", time = "day", series = 536, unit = "state", visibility = "horizontal", discretization = "symbolic")
change_points_network <- tsn(steps, value = "steps", id = "id", time = "day", series = 536, unit = "state", visibility = "horizontal", discretization = "change_points")
entropy_network <- tsn(steps, value = "steps", id = "id", time = "day", series = 536, unit = "state", visibility = "horizontal", discretization = "entropy")
magnitude_network <- tsn(steps, value = "steps", id = "id", time = "day", series = 536, unit = "state", visibility = "horizontal", discretization = "magnitude")
adaptive_magnitude_network <- tsn(steps, value = "steps", id = "id", time = "day", series = 536, unit = "state", visibility = "horizontal", discretization = "adaptive_magnitude")
percentile_magnitude_network <- tsn(steps, value = "steps", id = "id", time = "day", series = 536, unit = "state", visibility = "horizontal", discretization = "percentile_magnitude")
dtw_network <- tsn(steps, value = "steps", id = "id", time = "day", series = 536, unit = "state", visibility = "horizontal", discretization = "dtw")

networks <- list(
  threshold = threshold_network,
  width = width_network,
  quantile = quantile_network,
  kde = kde_network,
  kmeans = kmeans_network,
  gaussian = gaussian_network,
  hclust = hclust_network,
  ordinal = ordinal_network,
  symbolic = symbolic_network,
  change_points = change_points_network,
  entropy = entropy_network,
  magnitude = magnitude_network,
  adaptive_magnitude = adaptive_magnitude_network,
  percentile_magnitude = percentile_magnitude_network,
  dtw = dtw_network
)
state_palette <- c("1" = "#7FC97F", "2" = "#FDC086", "3" = "#F0027F",
                   "4" = "#386CB0", "5" = "#BEAED4", "6" = "#FDBF6F")

series_by_method <- do.call(rbind, Map(function(method, network) {
  series <- as.data.frame(network, what = "series")
  series$method <- method
  series
}, names(networks), networks))

state_counts <- aggregate(
  list(observations = series_by_method$value),
  by = list(method = series_by_method$method, state = series_by_method$state),
  FUN = length
)
print(state_counts)

invisible(lapply(networks, function(network) {
  plot(network, "series", overlay = "vertical", palette = state_palette)
  plot(network, "series", overlay = "horizontal", palette = state_palette, points = TRUE)
}))

mood_network <- tsn(motivation, series = "mood", unit = "state", visibility = "horizontal", discretization = "gaussian", seed = 1707)
motivation_network <- tsn(motivation, series = c("autonomy", "competence", "relatedness", "mood"), method = "distance", distance = "correlation")

plot(mood_network, "series", overlay = "horizontal", palette = state_palette)
plot(motivation_network, "series", columns = 2, scales = "free_y", trend = TRUE)
