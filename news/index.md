# Changelog

## tsn 1.4.2

A function-by-function audit of the package. Every function was run
against synthetic data and, where it implements a formula, against an
independent base-R or hand computation. The fixes below are each pinned
by a regression test in the new `tests/testthat/test-audit-*.R` files.

### Behaviour changes (results can differ from 1.4.1)

- **Natural visibility with exact ties.** Collinear points were decided
  by floating-point rounding, so `c(1.1, 1.2, 1.3)` produced a different
  graph from `c(11, 12, 13)`. The slope comparison now carries an
  explicit rounding tolerance and agrees with an exact integer reference
  on every tested configuration. `limit` is now inclusive on decimal
  elapsed time. Integer data and data without exact ties are unchanged.
- **Canberra distance.** The whole-series network used
  [`stats::dist()`](https://rdrr.io/r/stats/dist.html), which drops
  positions where both values are zero and rescales, while the chain
  network used the textbook form. Both paths now use the textbook form,
  so default Canberra networks on data with shared zeros change. Two
  all-zero units now have distance 0 rather than `NA`.
- **Coded states order nodes by value.** Numeric, integer, logical and
  Date `state` vectors now order nodes by their sorted value, matching
  the internal discretizers. Character states keep first-appearance
  order and factors keep their levels.
- **Quantile fallback on tied data.** When type-8 quantile breaks would
  leave a bin empty, the fallback now follows the empirical distribution
  instead of spacing distinct values evenly, so heavily tied series (for
  example zero-inflated ones) get better balanced states.
- **Event synchronisation of two single-event trains** no longer counts
  any two events as simultaneous; only exact coincidences count unless
  `tolerance` is given.
- **`abbreviated` selectors.** `method`, `unit`, `distance`, `connect`,
  `similarity`, `visibility`, `discretization` and `aggregation` are
  completed by partial matching before validation, so `"hv"` or
  `"quant"` can no longer bypass the shortcut-conflict checks or be
  stored verbatim in `parameters`.

### Arguments that were silently ignored are now errors

- `value`, `id` and `time` with non-data-frame input; `id`/`time`
  without `value` on a data frame; `value`/`id`/`time` with a `tsn`
  input to
  [`discretize()`](https://pak.dynasite.org/tsn/reference/discretize.md).
- `overlap` without `segment` in the `ts_*()` builders; `m`/`tau`
  outside the ordinal discretizer, including on grouped data; `breaks`
  missing for the threshold discretizer;
  `neighbors`/`threshold`/`percentile`/`bandwidth` outside the
  connection rule that uses them; a `similarity` kernel together with
  `connect = "gaussian"`.
- Sequence measures (`correlation`, `spearman`, `ccf`, `nmi`, `voi`,
  `cosine`) with `unit = "time"`, where each node holds a single value,
  and a `ccf` `lag` that is not shorter than the units.
- Unknown plotting arguments in every
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method,
  unnamed or duplicated network options, a `palette` that is unnamed,
  misnamed, non-character or does not cover every state, and
  `plot(x, foo = 1)` on states, trends and networks.
- Fractional `window`/`step`, non-finite `max_series`, `node_scale`,
  `network_width`, `penetrable` and `segment`; `labels` that are
  missing, empty or duplicated; `group` that is not a single column
  name; distinct double ids or group values that print identically
  (previously merged into one series or group); duplicate wide column
  names; impossible ISO dates; `Inf` time.

### New warnings

- `tsn_state_shortfall` (classed) whenever a discretizer realizes fewer
  states than requested, on every engine path including grouped `dtw`
  and `adaptive`. [`print()`](https://rdrr.io/r/base/print.html) of a
  `tsn_states` shows “2 states (3 requested)” and the model records
  `n_states_requested`.
- `tsn_trend_nonpositive_growth` (classed) when
  `method = "growth_factor"` meets zero or negative values.
- `tsn_weight_underflow` (classed) when an exponential kernel weight on
  a connected dyad underflows to exactly 0.

### Bug fixes

- Minkowski distances with a large `p` overflowed to `Inf`; they are
  computed in a scaled form.
- `correlation`, `ccf`, `nmi` and `voi` on single-observation units
  either failed cryptically or silently returned 0.
- Gaussian weights with a tiny `bandwidth` produced `0/0`.
- The percentile connection rule could drop the boundary dyad through
  type-7 quantile rounding.
- `difftime` time was treated as unordered labels; `mts` input failed;
  an unnamed matrix was labelled `V1`/`V2` instead of `series_1`; a
  partially named list lost its supplied names; `tsn(numeric(0))` gave
  an unhelpful error.
- A row-aligned `state` vector failed with long data plus `series`
  selection and with wide data.
- [`trend()`](https://pak.dynasite.org/tsn/reference/trend.md): OLS,
  Spearman and Kendall windows containing an `NA` or a constant run were
  labelled `Initial` instead of using the available values;
  [`print()`](https://rdrr.io/r/base/print.html) failed after
  [`subset()`](https://rdrr.io/r/base/subset.html); `par("cex")` was not
  restored.
- Discretization engine: `kmeans` reported a zero-length `converged`
  flag; the Gaussian EM recursed once per iteration and overflowed the
  stack for large `max_iterations`; ordinal patterns with `m >= 10` were
  ordered as strings (locale-dependent) in both the engine and the
  grouped path; magnitude decade boundaries fell in the wrong bin;
  out-of-range counts gave a coercion error.
- `.new_tsn()` now rejects endpoints that are not nodes, `NA` connection
  flags, duplicated dyads and malformed `parameters`.
- [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) for
  `tsn`, `ts_tna` and `ts_tna_group` ignored `row.names`.
- `print.tsn` says when it truncates to 10 dyads and points at
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).
- State networks: `points = TRUE` drew a state with the wrong or a
  missing colour in a group that never visits every state, and colours
  shifted between groups; both now colour the full state alphabet.
- Plotting: the default palette returned `NA` for 11, 21, 23, 27 …
  states (an `hcl.colors("Spectral")` midpoint); POSIXct axes printed
  epoch seconds; character (label) time broke every series view; heatmap
  edge tiles were half width; an absent factor level gave infinite
  guides; empty series selections and too many ribbons failed obscurely;
  [`par()`](https://rdrr.io/r/graphics/par.html) including `cex` and
  `mai` is restored in every view, also when a renderer errors; z-score
  guide lines were wrong after
  [`head()`](https://rdrr.io/r/utils/head.html); the `grid` argument now
  reaches the heatmap views; ribbons built on different times than the
  series are rejected.
- [`vg()`](https://pak.dynasite.org/tsn/reference/vg.md) recorded
  [`tsn()`](https://pak.dynasite.org/tsn/reference/tsn.md) as its
  provenance call and failed confusingly when `visibility` or `method`
  was passed.
- The penetrable visibility scan is about 60x faster on long series.

### Documentation

- Corrected `@return` and `@param` text across
  [`tsn()`](https://pak.dynasite.org/tsn/reference/tsn.md),
  [`discretize()`](https://pak.dynasite.org/tsn/reference/discretize.md),
  [`trend()`](https://pak.dynasite.org/tsn/reference/trend.md),
  [`vg()`](https://pak.dynasite.org/tsn/reference/vg.md), the `ts_*()`
  builders and the accessors; added examples to the accessors,
  `print.tsn`, `summary.tsn`, `srl` and `esm_srl`; the package page
  lists every verb and data set. The
  [`ts_tna()`](https://pak.dynasite.org/tsn/reference/ts_tna.md) example
  uses `as.matrix(network)` instead of reaching into the object.

## tsn 1.4.1

### Documentation

- All five vignettes now render their figures at `dpi = 110`. Three were
  at 150, which pushed the source tarball past CRAN’s 5 MB guidance; the
  package is materially smaller as a result (installed size 7.3 Mb to
  5.8 Mb, of which `doc` falls from 5.9 Mb to 4.5 Mb) with no change to
  any reported number.
- Prose in
  [`vignette("group-models")`](https://pak.dynasite.org/tsn/articles/group-models.md),
  [`vignette("nestimate-compatibility")`](https://pak.dynasite.org/tsn/articles/nestimate-compatibility.md)
  and
  [`vignette("nestimate-workflow")`](https://pak.dynasite.org/tsn/articles/nestimate-workflow.md)
  was copyedited, and the two Nestimate vignettes now name which edges
  survive the sequence bootstrap and which fail it, rather than only
  counting them.

### Internal

- The tutorial test files pin the bootstrap edge identities, the
  `high -> moderate` and `low -> low` edge weights, the `low -> low`
  p-value and the `low` state share, so a change that reshuffled which
  edges survive resampling can no longer pass while contradicting the
  vignette prose.

## tsn 1.4.0

### Breaking changes

- The bundled example data sets have been replaced. `steps` and
  `motivation` (which carried no usable provenance) are gone; in their
  place the package ships two data sets with full provenance owned by
  the package authors:
  - **`srl`** — a balanced daily panel of 36 students reporting nine
    self-regulated-learning indicators for 156 occasions on 0-100
    scales, the companion data of *Advanced Learning Analytics Methods*
    (CC BY-NC-SA 4.0; see the `COPYRIGHTS` file). Identical to the copy
    in the authors’ ‘idiographic’ package.
  - **`esm_srl`** — the authors’ own fully anonymized
    experience-sampling study: 41 students, momentary
    self-regulation/motivation/anxiety on 0-100 scales, fictional
    identifiers and constant-shifted dates. The tsn copy adds a derived
    `day_type` (weekday/weekend) factor so grouped models need no
    caller-side preparation. Code that referenced `steps` or
    `motivation` must switch data sets.
- The internal `R/sysdata.rda` copy of the old data sets (an unused
  leftover that silently re-shipped them inside the namespace) is
  removed.

### Documentation

- Every vignette, example, and the README were rebuilt on the new data,
  with all reported numbers recomputed:
  - `vignette("pleasure-all-functions")` is now
    [`vignette("constructing-time-series-networks")`](https://pak.dynasite.org/tsn/articles/constructing-time-series-networks.md),
    following one student’s momentary anxiety series through every
    exported verb.
  - [`vignette("nestimate-workflow")`](https://pak.dynasite.org/tsn/articles/nestimate-workflow.md)
    runs on the `srl` effort panel (fixed breaks at 40/70), one
    student’s series for segmentation, a first-versus-second-half course
    comparison, and the weekday/weekend grouped model.
  - [`vignette("group-models")`](https://pak.dynasite.org/tsn/articles/group-models.md)
    compares weekday and weekend effort dynamics end to end, including
    the welding and threshold failure modes of hand-subsetting.
  - [`vignette("nestimate-compatibility")`](https://pak.dynasite.org/tsn/articles/nestimate-compatibility.md)
    maps the Nestimate verb surface on the `srl` effort model.
  - [`vignette("plotting-time-series-networks")`](https://pak.dynasite.org/tsn/articles/plotting-time-series-networks.md)
    draws the same plot tour on the momentary anxiety series.

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
  ([`net_prune()`](https://pak.dynasite.org/Nestimate/reference/net_prune.html),
  [`state_distribution()`](https://pak.dynasite.org/Nestimate/reference/state_distribution.html),
  [`net_centrality()`](https://pak.dynasite.org/Nestimate/reference/net_centrality.html),
  [`compare_model()`](https://pak.dynasite.org/Nestimate/reference/compare_model.html),
  [`permutation()`](https://pak.dynasite.org/Nestimate/reference/permutation.html))
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
- `vignette("pleasure-all-functions")` was rewritten around the packaged
  `motivation` pleasure series, selecting it through each verb’s
  `series` argument rather than extracting the column.

## tsn 1.1.0

- New `vignette("pleasure-all-functions")`: a complete tutorial using
  the packaged `motivation$pleasure` measurements to demonstrate all
  nine exported functions, the standard result interface, and
  cograph-only network plots.
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
