# tsn 1.2.0

- `series_networks()` now returns a `tsn_series_networks` collection instead of
  a bare list. `print()` and `summary()` give a tidy one-row-per-series table
  (series, type, observations, states, edges), `as.data.frame()` returns the
  same table as a data frame, and `plot()` draws one model directly. `series`
  selects the model by name; it may be omitted only when the collection holds
  exactly one model.
- `plot()` on a network result now applies restrained presentation defaults
  (spring layout, nodes scaled by degree, one node fill, muted edges, no edge
  labels) so a bare `plot(network)` is readable. Every default is a named
  `cograph::splot()` argument and any value supplied through `...` overrides it,
  so the full cograph surface is unchanged.
- The `overlay` default for state and source-series views is now `"horizontal"`
  (value bands) rather than `"vertical"` (time runs), affecting
  `plot.tsn_states()`, `plot(type = "series")`, and `plot.ts_tna()`. Pass
  `overlay = "vertical"` to restore the previous shading.
- New `vignette("plotting-time-series-networks")`: the plotting surface end to
  end — network and source-series views, state overlays, ribbon, heatmap, and
  stack types, and transition-model plots.
- `vignette("pleasure-all-functions")` was rewritten around the packaged
  `motivation` pleasure series, selecting it through each verb's `series`
  argument rather than extracting the column.

# tsn 1.1.0

- New `vignette("pleasure-all-functions")`: a complete tutorial using the
  packaged `motivation$pleasure` measurements to demonstrate all nine exported
  functions, the standard result interface, and cograph-only network plots.
- New Nestimate bridge: `ts_tna()`, `ts_ftna()`, `ts_cna()`, and `ts_atna()`
  discretize one or many time series and build the matching Nestimate
  transition-network model (`netobject`) that keeps the source data
  (`$ts_source`) — plottable and compatible with Nestimate analyses whose
  sampling requirements the data satisfy. Node order follows the state level
  order (low/mid/high), not alphabetical.
- New `series_networks()` verb: splits a pooled bridge model into one full
  model per source series, all sharing the same node set and order for
  direct comparison and retaining the original Nestimate builder settings.
- `plot()` on a bridge result draws each series beside a transition network
  on the same row — by default the series' **own** network
  (`network = "per_series"`); `network = "summary"` draws the pooled model
  spanning all rows instead. Networks render exclusively through
  `cograph::splot()`, with state-matched node fills and nodes sized by
  in-strength (self-loops excluded); `...` passes straight to splot.
- New time-series plot types, all base graphics. `plot()` on a
  `discretize()` result accepts `type = "ribbon"` (clean series with a
  state-classification strip underneath), `"heatmap"` (one row per series,
  one tile per observation — compares state timing across many series), and
  `"stack"` (a companion series such as the original values stacked above
  the discretized series, both panels shaded by the discretized states,
  with optional state-coloured line segments via `color_line`), alongside
  the previous shaded view (now `type = "overlay"`, still the default).
  The overlay view combines colour shading in one direction with dashed
  guide lines in the other via `lines =` (`"horizontal"` at the state
  boundaries, `"vertical"` at persistent state transitions filtered by
  `min_run`). `plot()` on a `trend()` result accepts `"ribbon"`,
  `"heatmap"`, and `"panels"` (each series stacked above the rolling metric
  it was classified on), alongside the previous default
  (`type = "points"`).
- Five new distance measures for `tsn(method = "distance")`, all base-R with no
  new dependencies: `"ccf"` (one minus the maximum absolute cross-correlation
  across lags), `"nmi"` (one minus normalized mutual information on shared
  quantile bins), `"voi"` (variation of information), `"event_sync"` (one minus
  the Quiroga event-synchronization index over event times), and
  `"van_rossum"` (exact closed-form van Rossum spike-train distance). New
  method-specific arguments: `bins` (nmi/voi), `lag` (ccf), and `tolerance`
  (an optional cap on adaptive event-synchronization windows, or the van
  Rossum time constant). NMI and VoI are verified
  equivalent to `aricode`; the van Rossum exact form is verified against the
  discretized-grid reference.
- Visibility geometry, edge distances, temporal limits, and decay now use the
  supplied numeric or date-time axis; nonnumeric labels use observation steps.
  Collision-safe node labels preserve distinct `(id, time)` pairs.
- Temporal discretizers now compute ordinal patterns, adaptive-magnitude
  features, and DTW windows within each series before fitting a shared state
  vocabulary, preventing patterns from crossing ID boundaries.
- Ordinal transition builders exclude trailing display-fill labels, and
  NMI/VoI now use marginal quantile bins so monotone rescaling preserves the
  expected information distance.
- Distance chains preserve caller series order and never link the last window
  of one series to the first window of another.
- Event synchronization now implements the adaptive Quian Quiroga--Kreuz--
  Grassberger coincidence window and validates strictly increasing event times.
- `trend()` now supports missing observations, respects numeric/date elapsed
  time, handles character time without coercion warnings, avoids infinite
  growth factors, and validates its classification thresholds.
- Plot guides now map transformed and magnitude-space breaks back to the raw
  signed axis; cograph transition-network node sizing excludes self-loops, and
  `show_weights = FALSE` is honoured.
- New `similarity` argument selecting the distance-to-weight kernel:
  `"inverse"` (the previous fixed rule), `"normalized_inverse"`,
  `"negative_exp"`, and `"gaussian"`, the latter two scaled by `bandwidth`
  (defaulting to the median positive distance).
- `normalize` now also accepts mode names: `TRUE`/`"max"` (previous
  behaviour), `"minmax"`, and `"quantile"` (5th-95th percentile winsorized
  scaling).
- Full upstream coverage: ported `trend()` (rolling OLS, Theil-Sen, Spearman,
  Kendall, and growth-factor trend classification with an epsilon flat band,
  turbulence override, and center/right/left alignment) and a public
  `discretize()` verb with custom `labels` and `log`/`zscore` transforms.
- Added the `vg()` verb for visibility graphs: `vg(x)` and `vg(x, "horizontal")`
  are discoverable, named-argument wrappers around `tsn(method = "visibility")`.
- Results are now dual-class `c("tsn", "netobject", "cograph_network")` objects.
  With cograph installed, `cograph::splot(x)` renders any tsn network directly;
  `as.data.frame(x)` still returns the tidy dyad table and
  `as.data.frame(x, what = "series")` the source series.
- Network plots now delegate exclusively to `cograph::splot()`; tsn no longer
  maintains a second generic graph renderer. `plot(x, "series")` retains the
  package-specific source-series view.
- Method-string shortcuts: `tsn(x, "hvg")`, `tsn(x, "nvg")`, `tsn(x, "distance")`,
  and every discretizer name (`tsn(x, "ordinal")`, `tsn(x, "quantile")`, ...) as
  a direct method on a bare numeric vector.
- Fifteen discretizers reachable through both `tsn(unit = "state")` and
  `discretize()`, with identical state assignments.
- New network options: `chain` (consecutive-only transition chains), directed
  distance networks, and `normalize`.

# tsn 1.0.0

- Rebuilt the package around the single `tsn()` entry point.
- Added whole-series and sliding-window distance networks.
- Added base-R natural, horizontal, and penetrable visibility graphs.
- Added ten base-R distance measures, including unequal-length DTW.
- Added five consistent distance-to-network connection rules.
- Added seven internal state discretizers, including a base-R Gaussian mixture.
- Standardized every result behind the `tsn` class with tidy dyad-table access.
- Removed all external runtime dependencies and overlapping network verbs.
- Corrected legacy selection, similarity direction, state ordering, DTW,
  visibility duplication, and interleaved-ID defects.
