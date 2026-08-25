data("srl", package = "tsn")
data("esm_srl", package = "tsn")

threshold_network <- tsn(srl, value = "effort", id = "name", time = "day", series = "Erik", unit = "state", visibility = "horizontal", discretization = "threshold", breaks = c(40, 70))
width_network <- tsn(srl, value = "effort", id = "name", time = "day", series = "Erik", unit = "state", visibility = "horizontal", discretization = "width")
quantile_network <- tsn(srl, value = "effort", id = "name", time = "day", series = "Erik", unit = "state", visibility = "horizontal", discretization = "quantile")
kde_network <- tsn(srl, value = "effort", id = "name", time = "day", series = "Erik", unit = "state", visibility = "horizontal", discretization = "kde")
kmeans_network <- tsn(srl, value = "effort", id = "name", time = "day", series = "Erik", unit = "state", visibility = "horizontal", discretization = "kmeans", seed = 1707)
gaussian_network <- tsn(srl, value = "effort", id = "name", time = "day", series = "Erik", unit = "state", visibility = "horizontal", discretization = "gaussian", seed = 1707)
hclust_network <- tsn(srl, value = "effort", id = "name", time = "day", series = "Erik", unit = "state", visibility = "horizontal", discretization = "hclust")
ordinal_network <- tsn(srl, value = "effort", id = "name", time = "day", series = "Erik", unit = "state", visibility = "horizontal", discretization = "ordinal", m = 3, tau = 1)
symbolic_network <- tsn(srl, value = "effort", id = "name", time = "day", series = "Erik", unit = "state", visibility = "horizontal", discretization = "symbolic")
change_points_network <- tsn(srl, value = "effort", id = "name", time = "day", series = "Erik", unit = "state", visibility = "horizontal", discretization = "change_points")
entropy_network <- tsn(srl, value = "effort", id = "name", time = "day", series = "Erik", unit = "state", visibility = "horizontal", discretization = "entropy")
magnitude_network <- tsn(srl, value = "effort", id = "name", time = "day", series = "Erik", unit = "state", visibility = "horizontal", discretization = "magnitude")
adaptive_magnitude_network <- tsn(srl, value = "effort", id = "name", time = "day", series = "Erik", unit = "state", visibility = "horizontal", discretization = "adaptive_magnitude")
percentile_magnitude_network <- tsn(srl, value = "effort", id = "name", time = "day", series = "Erik", unit = "state", visibility = "horizontal", discretization = "percentile_magnitude")
dtw_network <- tsn(srl, value = "effort", id = "name", time = "day", series = "Erik", unit = "state", visibility = "horizontal", discretization = "dtw")

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

hana <- subset(esm_srl, name == "Hana")
anxiety_network <- tsn(hana, value = "anxiety", id = "name", time = "occasion", unit = "state", visibility = "horizontal", discretization = "gaussian", seed = 1707)
indicator_network <- tsn(hana, series = c("planning", "monitoring", "effort", "anxiety"), method = "distance", distance = "correlation")

plot(anxiety_network, "series", overlay = "horizontal", palette = state_palette)
plot(indicator_network, "series", columns = 2, scales = "free_y", trend = TRUE)
