# Changelog

## tsn 1.3.0

### New features

- Grouped transition models:
  [`ts_tna()`](https://pak.dynasite.org/tsn/reference/ts_tna.md),
  [`ts_ftna()`](https://pak.dynasite.org/tsn/reference/ts_tna.md),
  [`ts_cna()`](https://pak.dynasite.org/tsn/reference/ts_tna.md), and
  [`ts_atna()`](https://pak.dynasite.org/tsn/reference/ts_tna.md) gain a
  `group` argument naming a column of `data`. The result is one network
  per group — a `ts_tna_group` (also a Nestimate `netobject_group`) that
  Nestimate’s compatible grouped verbs
  ([`net_prune()`](https://saqr.me/Nestimate/reference/net_prune.html),
  [`state_distribution()`](https://saqr.me/Nestimate/reference/state_distribution.html),
  [`net_centrality()`](https://saqr.me/Nestimate/reference/net_centrality.html),
  [`compare_model()`](https://saqr.me/Nestimate/reference/compare_model.html),
  [`permutation()`](https://saqr.me/Nestimate/reference/permutation.html))
  accept directly. States are discretized from the pooled series before
  the split, so every group shares one alphabet and node set. A sequence
  never spans a group boundary: contiguous runs of the group column
  become their own sequences, so no transition is ever counted between
  observations that were not adjacent in time.
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) on the
  collection returns tidy `edges`, `series`, or `groups` tables.
- `segment` and `overlap` on the `ts_*na()` family cut each series into
  shorter sequences so sequence-based inference (bootstrap, permutation,
  stability) has units to resample. Partitioned blocks are the
  conservative default; `overlap = TRUE` slides the block one
  observation at a time and at `segment = 2` reproduces the unsegmented
  estimate exactly.
- New
  [`as.matrix.ts_tna()`](https://pak.dynasite.org/tsn/reference/as.matrix.ts_tna.md)
  and
  [`as.data.frame.ts_tna()`](https://pak.dynasite.org/tsn/reference/as.data.frame.ts_tna.md)
  methods: the weight matrix and a tidy one-row-per-state-pair edge
  table (plus `what = "series"` for the per-observation source table).
- New
  [`vignette("nestimate-workflow")`](https://pak.dynasite.org/tsn/articles/nestimate-workflow.md):
  raw series to transition network to the Nestimate inferential stack,
  including segmentation and grouped comparison.
- New
  [`vignette("group-models")`](https://pak.dynasite.org/tsn/articles/group-models.md):
  a full tutorial on grouped transition models — why hand-subsetting
  both fabricates transitions and shifts the state thresholds, how the
  pooled alphabet and boundary rule fix it, and the descriptive and
  inferential comparison of groups end to end.
- New
  [`vignette("nestimate-compatibility")`](https://pak.dynasite.org/tsn/articles/nestimate-compatibility.md):
  the verb-by-verb map of which Nestimate analyses accept a tsn model
  directly — reading, composition, dynamics, entropy, pruning,
  reliability, and model comparison — plus the verbs that do not apply
  and why, ending in a compatibility table.
- New
  [`plot.ts_tna_group()`](https://pak.dynasite.org/tsn/reference/plot.ts_tna_group.md)
  method: draws one selected network of a grouped model
  (`plot(x, group = "Home")`) with the full
  [`plot.ts_tna()`](https://pak.dynasite.org/tsn/reference/plot.ts_tna.md)
  surface, so a group can be rendered without reaching into the
  collection.

### Breaking changes

- Arguments that the selected configuration cannot consume are now
  rejected with an error instead of being silently ignored — for example
  `chain` or `normalize` on a visibility network, `limit`, `decay`,
  `penetrable`, or `aggregation` on a distance network, `step` outside
  `unit = "window"`, `p` without `distance = "minkowski"`, `breaks`
  without `discretization = "threshold"`, `n_states` with the ordinal
  discretizer, `seed` with a deterministic discretizer, and any
  discretization option when `data` is already a `tsn_states`. Shortcut
  methods (`"nvg"`, `"hvg"`, discretizer names) likewise reject granular
  arguments that contradict what the shortcut implies. Previously these
  calls returned a plausible but unintended model.
- [`summary()`](https://rdrr.io/r/base/summary.html) density now uses a
  fixed possible-edge universe per network family: state networks always
  count self-loops among the possible edges, and series/window/time
  networks never do. Previously the denominator depended on whether a
  loop happened to be observed, so densities were not comparable across
  models. State networks without observed loops report a lower (correct)
  density than before.
- Caller-supplied factor `state` values now fix the node order by their
  level order, unused factor levels are retained as zero-degree nodes,
  and the stored source `state` column stays a factor. Discretized
  states order nodes numerically (`"1"`, `"2"`, …) instead of by first
  appearance. Plain character states keep first-appearance order.
- The `"okabe"` plot palette now rejects requests for more than its nine
  colour-blind-safe colours instead of returning `NA` colours.

### Bug fixes

- `series` selection on long data now follows the requested series
  order, exactly as wide, matrix, and list selection do. Previously the
  same selection could reverse chained or directed edges depending on
  the input container.
- Point-level distance networks (`method = "distance", unit = "time"`)
  now use the same collision-safe node labels as the visibility family,
  so valid `(id, time)` pairs whose display strings collide are no
  longer rejected.
- `ts_*na(series = ..., group = ...)` no longer warns that groups
  excluded by the caller’s own `series` selection were “lost to
  discretization”.

### Performance

- Natural visibility construction now runs in quadratic instead of cubic
  time (and no longer materializes all candidate pairs at once): a
  1,000-observation series builds in well under a second. The same scan
  accelerates horizontal visibility with `penetrable` or `limit` set.

## tsn 1.2.0

- [`series_networks()`](https://pak.dynasite.org/tsn/reference/series_networks.md)
  now returns a `tsn_series_networks` collection instead of a bare list.
  [`print()`](https://rdrr.io/r/base/print.html) and
  [`summary()`](https://rdrr.io/r/base/summary.html) give a tidy
  one-row-per-series table (series, type, observations, states, edges),
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns
  the same table as a data frame, and
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) draws one
  model directly. `series` selects the model by name; it may be omitted
  only when the collection holds exactly one model.
- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) on a network
  result now applies restrained presentation defaults (spring layout,
  nodes scaled by degree, one node fill, muted edges, no edge labels) so
  a bare `plot(network)` is readable. Every default is a named
  [`cograph::splot()`](https://sonsoles.me/cograph/reference/splot.html)
  argument and any value supplied through `...` overrides it, so the
  full cograph surface is unchanged.
- The `overlay` default for state and source-series views is now
  `"horizontal"` (value bands) rather than `"vertical"` (time runs),
  affecting
  [`plot.tsn_states()`](https://pak.dynasite.org/tsn/reference/plot.tsn_states.md),
  `plot(type = "series")`, and
  [`plot.ts_tna()`](https://pak.dynasite.org/tsn/reference/plot.ts_tna.md).
  Pass `overlay = "vertical"` to restore the previous shading.
- New
  [`vignette("plotting-time-series-networks")`](https://pak.dynasite.org/tsn/articles/plotting-time-series-networks.md):
  the plotting surface end to end — network and source-series views,
  state overlays, ribbon, heatmap, and stack types, and transition-model
  plots.
- [`vignette("pleasure-all-functions")`](https://pak.dynasite.org/tsn/articles/pleasure-all-functions.md)
  was rewritten around the packaged `motivation` pleasure series,
  selecting it through each verb’s `series` argument rather than
  extracting the column.

## tsn 1.1.0

- New
  [`vignette("pleasure-all-functions")`](https://pak.dynasite.org/tsn/articles/pleasure-all-functions.md):
  a complete tutorial using the packaged `motivation$pleasure`
  measurements to demonstrate all nine exported functions, the standard
  result interface, and cograph-only network plots.
- New Nestimate bridge:
  [`ts_tna()`](https://pak.dynasite.org/tsn/reference/ts_tna.md),
  [`ts_ftna()`](https://pak.dynasite.org/tsn/reference/ts_tna.md),
  [`ts_cna()`](https://pak.dynasite.org/tsn/reference/ts_tna.md), and
  [`ts_atna()`](https://pak.dynasite.org/tsn/reference/ts_tna.md)
  discretize one or many time series and build the matching Nestimate
  transition-network model (`netobject`) that keeps the source data
  (`$ts_source`) — plottable and compatible with Nestimate analyses
  whose sampling requirements the data satisfy. Node order follows the
  state level order (low/mid/high), not alphabetical.
- New
  [`series_networks()`](https://pak.dynasite.org/tsn/reference/series_networks.md)
  verb: splits a pooled bridge model into one full model per source
  series, all sharing the same node set and order for direct comparison
  and retaining the original Nestimate builder settings.
- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) on a bridge
  result draws each series beside a transition network on the same row —
  by default the series’ **own** network (`network = "per_series"`);
  `network = "summary"` draws the pooled model spanning all rows
  instead. Networks render exclusively through
  [`cograph::splot()`](https://sonsoles.me/cograph/reference/splot.html),
  with state-matched node fills and nodes sized by in-strength
  (self-loops excluded); `...` passes straight to splot.
- New time-series plot types, all base graphics.
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) on a
  [`discretize()`](https://pak.dynasite.org/tsn/reference/discretize.md)
  result accepts `type = "ribbon"` (clean series with a
  state-classification strip underneath), `"heatmap"` (one row per
  series, one tile per observation — compares state timing across many
  series), and `"stack"` (a companion series such as the original values
  stacked above the discretized series, both panels shaded by the
  discretized states, with optional state-coloured line segments via
  `color_line`), alongside the previous shaded view (now
  `type = "overlay"`, still the default). The overlay view combines
  colour shading in one direction with dashed guide lines in the other
  via `lines =` (`"horizontal"` at the state boundaries, `"vertical"` at
  persistent state transitions filtered by `min_run`).
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) on a
  [`trend()`](https://pak.dynasite.org/tsn/reference/trend.md) result
  accepts `"ribbon"`, `"heatmap"`, and `"panels"` (each series stacked
  above the rolling metric it was classified on), alongside the previous
  default (`type = "points"`).
- Five new distance measures for `tsn(method = "distance")`, all base-R
  with no new dependencies: `"ccf"` (one minus the maximum absolute
  cross-correlation across lags), `"nmi"` (one minus normalized mutual
  information on shared quantile bins), `"voi"` (variation of
  information), `"event_sync"` (one minus the Quiroga
  event-synchronization index over event times), and `"van_rossum"`
  (exact closed-form van Rossum spike-train distance). New
  method-specific arguments: `bins` (nmi/voi), `lag` (ccf), and
  `tolerance` (an optional cap on adaptive event-synchronization
  windows, or the van Rossum time constant). NMI and VoI are verified
  equivalent to `aricode`; the van Rossum exact form is verified against
  the discretized-grid reference.
- Visibility geometry, edge distances, temporal limits, and decay now
  use the supplied numeric or date-time axis; nonnumeric labels use
  observation steps. Collision-safe node labels preserve distinct
  `(id, time)` pairs.
- Temporal discretizers now compute ordinal patterns, adaptive-magnitude
  features, and DTW windows within each series before fitting a shared
  state vocabulary, preventing patterns from crossing ID boundaries.
- Ordinal transition builders exclude trailing display-fill labels, and
  NMI/VoI now use marginal quantile bins so monotone rescaling preserves
  the expected information distance.
- Distance chains preserve caller series order and never link the last
  window of one series to the first window of another.
- Event synchronization now implements the adaptive Quian Quiroga–Kreuz–
  Grassberger coincidence window and validates strictly increasing event
  times.
- [`trend()`](https://pak.dynasite.org/tsn/reference/trend.md) now
  supports missing observations, respects numeric/date elapsed time,
  handles character time without coercion warnings, avoids infinite
  growth factors, and validates its classification thresholds.
- Plot guides now map transformed and magnitude-space breaks back to the
  raw signed axis; cograph transition-network node sizing excludes
  self-loops, and `show_weights = FALSE` is honoured.
- New `similarity` argument selecting the distance-to-weight kernel:
  `"inverse"` (the previous fixed rule), `"normalized_inverse"`,
  `"negative_exp"`, and `"gaussian"`, the latter two scaled by
  `bandwidth` (defaulting to the median positive distance).
- `normalize` now also accepts mode names: `TRUE`/`"max"` (previous
  behaviour), `"minmax"`, and `"quantile"` (5th-95th percentile
  winsorized scaling).
- Full upstream coverage: ported
  [`trend()`](https://pak.dynasite.org/tsn/reference/trend.md) (rolling
  OLS, Theil-Sen, Spearman, Kendall, and growth-factor trend
  classification with an epsilon flat band, turbulence override, and
  center/right/left alignment) and a public
  [`discretize()`](https://pak.dynasite.org/tsn/reference/discretize.md)
  verb with custom `labels` and `log`/`zscore` transforms.
- Added the [`vg()`](https://pak.dynasite.org/tsn/reference/vg.md) verb
  for visibility graphs: `vg(x)` and `vg(x, "horizontal")` are
  discoverable, named-argument wrappers around
  `tsn(method = "visibility")`.
- Results are now dual-class `c("tsn", "netobject", "cograph_network")`
  objects. With cograph installed, `cograph::splot(x)` renders any tsn
  network directly; `as.data.frame(x)` still returns the tidy dyad table
  and `as.data.frame(x, what = "series")` the source series.
- Network plots now delegate exclusively to
  [`cograph::splot()`](https://sonsoles.me/cograph/reference/splot.html);
  tsn no longer maintains a second generic graph renderer.
  `plot(x, "series")` retains the package-specific source-series view.
- Method-string shortcuts: `tsn(x, "hvg")`, `tsn(x, "nvg")`,
  `tsn(x, "distance")`, and every discretizer name (`tsn(x, "ordinal")`,
  `tsn(x, "quantile")`, …) as a direct method on a bare numeric vector.
- Fifteen discretizers reachable through both `tsn(unit = "state")` and
  [`discretize()`](https://pak.dynasite.org/tsn/reference/discretize.md),
  with identical state assignments.
- New network options: `chain` (consecutive-only transition chains),
  directed distance networks, and `normalize`.

## tsn 1.0.0

- Rebuilt the package around the single
  [`tsn()`](https://pak.dynasite.org/tsn/reference/tsn.md) entry point.
- Added whole-series and sliding-window distance networks.
- Added base-R natural, horizontal, and penetrable visibility graphs.
- Added ten base-R distance measures, including unequal-length DTW.
- Added five consistent distance-to-network connection rules.
- Added seven internal state discretizers, including a base-R Gaussian
  mixture.
- Standardized every result behind the `tsn` class with tidy dyad-table
  access.
- Removed all external runtime dependencies and overlapping network
  verbs.
- Corrected legacy selection, similarity direction, state ordering, DTW,
  visibility duplication, and interleaved-ID defects.
