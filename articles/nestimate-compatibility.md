# The Nestimate Compatibility Map

## One object, two packages

A [`ts_tna()`](https://pak.dynasite.org/tsn/reference/ts_tna.md) model
is not exported *to* Nestimate; it *is* a Nestimate object. This
vignette maps what that buys, verb by verb: which parts of Nestimate’s
analysis surface accept a tsn transition model directly, what question
each verb answers, what each result draws, and which parts do not apply
and why. The companion vignette
[`vignette("nestimate-workflow", package = "tsn")`](https://pak.dynasite.org/tsn/articles/nestimate-workflow.md)
runs a single analysis end to end; this one is the reference you consult
when deciding which verb to reach for.

The running example is the model from that workflow: 151 participants’
daily step counts, cut at 5,000 and 10,000 steps so the states mean
something outside the sample.

``` r

library(tsn)
library(Nestimate)

data(steps)
walkers <- subset(steps, !is.na(steps))

walk_model <- ts_tna(
  walkers,
  value = "steps",
  id = "id",
  time = "day",
  discretization = "threshold",
  breaks = c(5000, 10000),
  labels = c("sedentary", "moderate", "active")
)

class(walk_model)
#> [1] "ts_tna"          "netobject"       "cograph_network"
```

The class chain is the whole compatibility story. `ts_tna` is the tsn
layer (it remembers the source series and discretization), `netobject`
is what every Nestimate verb dispatches on, and `cograph_network` is
what
[`cograph::splot()`](https://sonsoles.me/cograph/reference/splot.html)
renders. Nestimate’s own validator agrees:

``` r

validate_netobject(walk_model)
```

And because the object is a `cograph_network`, the model itself is one
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) away:

``` r

plot(walk_model, type = "network")
```

![Directed transition network with sedentary, moderate, and active
nodes; edge labels show transition probabilities, with the heaviest
self-loop on the active
state.](nestimate-compatibility_files/figure-html/plot-model-1.png)

The strong diagonal is the first substantive fact: every state prefers
itself, `active` most of all at 0.62.

## Reading the model as data

Both packages provide accessors, and they answer slightly different
questions. tsn’s
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns
every state pair including self-transitions;
[`as.matrix()`](https://rdrr.io/r/base/matrix.html) returns the
row-stochastic weight matrix.

``` r

head(as.data.frame(walk_model))
#>        from        to     weight
#> 1 sedentary sedentary 0.35281837
#> 2  moderate sedentary 0.15365259
#> 3    active sedentary 0.06159838
#> 4 sedentary  moderate 0.44514034
#> 5  moderate  moderate 0.50129748
#> 6    active  moderate 0.31989325
```

Nestimate’s extractors return the same estimates in its own shapes:

``` r

extract_transition_matrix(walk_model)
#>            sedentary  moderate    active
#> sedentary 0.35281837 0.4451403 0.2020413
#> moderate  0.15365259 0.5012975 0.3450499
#> active    0.06159838 0.3198932 0.6185084
#> attr(,"class")
#> [1] "nest_transition_matrix" "matrix"                 "array"

extract_initial_probs(walk_model)
#> sedentary  moderate    active 
#> 0.1125828 0.3112583 0.5761589 
#> attr(,"class")
#> [1] "nest_initial_probs" "numeric"

extract_edges(walk_model)
#>        from        to     weight
#> 1 sedentary  moderate 0.44514034
#> 2  moderate    active 0.34504993
#> 3    active  moderate 0.31989325
#> 4 sedentary    active 0.20204129
#> 5  moderate sedentary 0.15365259
#> 6    active sedentary 0.06159838
```

One difference matters for transition networks:
[`extract_edges()`](https://saqr.me/Nestimate/reference/extract_edges.html)
**excludes self-transitions** — it returns the six between-state edges,
sorted by weight, and omits the diagonal. For this model the diagonal is
where most of the probability lives, so use
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) or
[`extract_transition_matrix()`](https://saqr.me/Nestimate/reference/extract_transition_matrix.html)
whenever persistence is part of the question, and
[`extract_edges()`](https://saqr.me/Nestimate/reference/extract_edges.html)
when only between-state movement is.

The initial-state probabilities say the panel starts active: 57.6% of
participants’ first recorded day is an `active` day, 11.3% a `sedentary`
one.

## Where the process spends its time

[`state_distribution()`](https://saqr.me/Nestimate/reference/state_distribution.html)
and
[`state_frequencies()`](https://saqr.me/Nestimate/reference/state_frequencies.html)
count observations per state — the marginal composition, before any
dynamics.

``` r

state_distribution(walk_model)
#>   group     state count proportion
#> 1   all    active 13921  0.4484425
#> 2   all  moderate 12776  0.4115582
#> 3   all sedentary  4346  0.1399994

state_frequencies(walk_model)
#>       state count proportion
#> 1    active 13921     0.4484
#> 2  moderate 12776     0.4116
#> 3 sedentary  4346     0.1400
```

Active days make up 44.8% of the panel and sedentary days 14.0%. Three
displays draw the composition from different angles. The mosaic is the
proportion bar:

``` r

mosaic_plot(walk_model)
```

![Mosaic plot of the step-count model's state composition, with active
and moderate days forming most of the area and sedentary days a narrow
band.](nestimate-compatibility_files/figure-html/mosaic-1.png)

[`sequence_plot()`](https://saqr.me/Nestimate/reference/sequence_plot.html)
is the sequence carpet — one row per participant, one coloured cell per
day:

``` r

sequence_plot(walk_model)
```

![Sequence heatmap with one row per participant and one coloured cell
per day, showing long same-state spells as horizontal streaks of
colour.](nestimate-compatibility_files/figure-html/carpet-1.png)

The streaks are persistence made visible: participants hold a state for
spells, not single days.
[`distribution_plot()`](https://saqr.me/Nestimate/reference/distribution_plot.html)
stacks the day-by-day state mix of the whole panel:

``` r

distribution_plot(walk_model)
```

![Stacked area chart of the panel's state proportions across the study
days, with bands for sedentary, moderate, and active
days.](nestimate-compatibility_files/figure-html/composition-time-1.png)

This is composition over calendar time — the view in which the seasonal
shift quantified in the workflow vignette (active days falling from
46.9% to 42.8% after November) lives.
[`plot_state_frequencies()`](https://saqr.me/Nestimate/reference/plot_state_frequencies.html)
offers a fourth, marimekko-style variant of the same information.

## Which states and edges carry the structure?

[`net_centrality()`](https://saqr.me/Nestimate/reference/net_centrality.html)
ranks states. Self-transitions are excluded by default — for a
transition network the diagonal would otherwise dominate every strength
measure.

``` r

walk_centrality <- net_centrality(walk_model)
#> centralities computed excluding loops (diagonal). Pass `loops = TRUE` to include self-transitions.

walk_centrality
#>               state InStrength Betweenness Diffusion
#> sedentary sedentary  0.2152510           0 1.0000000
#> moderate   moderate  0.7650336           1 0.4116171
#> active       active  0.5470912           0 0.0000000
```

``` r

plot(walk_centrality)
```

![Centrality chart of the three states showing moderate with the highest
in-strength and
betweenness.](nestimate-compatibility_files/figure-html/centrality-plot-1.png)

`moderate` is the hub: the highest in-strength (0.77) and the only state
with nonzero betweenness. That is the staircase structure — traffic
between `sedentary` and `active` routes through the middle.

[`net_edge_betweenness()`](https://saqr.me/Nestimate/reference/net_edge_betweenness.html)
asks the same question of edges:

``` r

walk_betweenness <- net_edge_betweenness(walk_model)

walk_betweenness
#> Network (method: edge_betweenness) [directed]
#>   Weights: [1.000, 2.000]  |  mean: 1.400
#> 
#>   Weight matrix:
#>             sedentary moderate active
#>   sedentary         0        1      1
#>   moderate          2        0      1
#>   active            0        2      0
```

``` r

plot(walk_betweenness)
```

![Edge-betweenness network view with the moderate-to-sedentary and
active-to-moderate edges carrying the highest
loads.](nestimate-compatibility_files/figure-html/betweenness-plot-1.png)

The heaviest-loaded edges (betweenness 2) are `moderate → sedentary` and
`active → moderate`: the descending staircase. Movement down the
activity scale routes through the middle state.

## How the process moves

### Mean first passage times

[`passage_time()`](https://saqr.me/Nestimate/reference/passage_time.html)
converts the transition matrix into expected travel times between states
(Kemeny & Snell, 1976).

``` r

walk_passage <- passage_time(walk_model)

walk_passage
#> Mean First Passage Times (3 states)
#> 
#>           sedentary moderate active
#> sedentary       7.1      2.5    3.7
#> moderate        9.1      2.4    3.1
#> active         10.2      3.0    2.2
#> 
#> Stationary distribution:
#> sedentary  moderate    active 
#>    0.1405    0.4123    0.4473
```

``` r

plot(walk_passage)
```

![Heatmap of mean first passage times between the three states, with
long times into the sedentary state and short times into moderate and
active.](nestimate-compatibility_files/figure-html/passage-plot-1.png)

A `moderate` or `active` day is reached from anywhere within two to four
days on average; a `sedentary` day takes seven to ten. Low activity is
the state this panel visits reluctantly, not a basin it falls into. The
stationary distribution — 14% sedentary, 41% moderate, 45% active — is
where the chain settles regardless of its start.

### Persistence, return, and sojourn

[`markov_stability()`](https://saqr.me/Nestimate/reference/markov_stability.html)
summarizes each state’s dynamics in one row: the self-transition
probability, the stationary share, the mean recurrence time, and the
mean sojourn (how long a visit lasts once entered).

``` r

walk_spells <- markov_stability(walk_model)

walk_spells
#> Markov Stability Analysis
#> 
#>      state persistence stationary_prob return_time sojourn_time
#>  sedentary      0.3528          0.1405        7.12         1.55
#>   moderate      0.5013          0.4123        2.43         2.01
#>     active      0.6185          0.4473        2.24         2.62
#>  avg_time_to_others avg_time_from_others
#>                3.10                 9.67
#>                6.12                 2.76
#>                6.64                 3.43
```

``` r

plot(walk_spells)
```

![Bar panels of persistence, stationary probability, return time, and
sojourn time for the three
states.](nestimate-compatibility_files/figure-html/markov-stability-plot-1.png)

An active spell lasts 2.6 days on average and recurs every 2.2; a
sedentary spell lasts 1.6 days and recurs only every 7.1. Persistence
and prevalence line up here, but they are logically distinct columns — a
rare state can still be sticky.

### Is the chain well-behaved?

[`chain_structure()`](https://saqr.me/Nestimate/reference/chain_structure.html)
reports the Markov-chain diagnostics that other verbs quietly assume:
irreducibility (every state reachable from every other), aperiodicity,
and reversibility.

``` r

summary(chain_structure(walk_model))
#> Chain structure summary  [3 states, 1 classes]
#>   irreducible: TRUE   aperiodic: TRUE   regular: TRUE   reversible: FALSE
#> 
#>      state classification period persistence return_probability sojourn_steps
#>  sedentary      recurrent      1      0.3528                  1          1.55
#>   moderate      recurrent      1      0.5013                  1          2.01
#>     active      recurrent      1      0.6185                  1          2.62
#>  stationary_probability
#>                  0.1405
#>                  0.4123
#>                  0.4473
```

The chain is irreducible, aperiodic, and regular, so the stationary
distribution above is well-defined and unique. It is *not* reversible:
the forward process is statistically distinguishable from its time
reversal, which is itself a substantive fact about behavior change — the
routes up and the routes down are not mirror images.

## How predictable is the process?

[`transition_entropy()`](https://saqr.me/Nestimate/reference/transition_entropy.html)
gives the point estimate;
[`entropy_bayes()`](https://saqr.me/Nestimate/reference/entropy_bayes.html)
adds a posterior credible interval by placing a Dirichlet prior on each
row.

``` r

transition_entropy(walk_model)
#> Transition Entropy (3 states, bits; ceiling = 1.585)
#> 
#>                           raw            normalised
#>   Entropy rate    h(P)  = 1.346 bits    0.849
#>   Stationary    H(pi)  = 1.444 bits    0.911
#>   Redundancy   H(pi)-h = 0.098 bits    0.068
#> 
#> Normalised: h(P) and H(pi) are raw / log_2(n_states) (0 = deterministic, 1 = uniform);
#>   redundancy is the relative redundancy (H(pi) - h(P)) / H(pi), not raw / log_2(n_states).
```

``` r

set.seed(2026)
walk_entropy <- entropy_bayes(walk_model)

walk_entropy
#> Bayesian Transition Entropy (3 states, bits; Dirichlet prior 0.5, 4000 draws)
#> 
#>   entropy_rate:       1.346 [1.337, 1.355]
#>    stationary_entropy: 1.444 [1.436, 1.452]
#>    redundancy:         0.098 [0.092, 0.104]
#> 
#> Edges: 9 observed; 9 credible (share of h(P) credibly > 1%).
#> Use summary() for the edge table, plot() for the posterior, and
#> $model for the pruned stable entropy network.
```

``` r

plot(walk_entropy)
```

![Posterior distributions of the entropy rate, stationary entropy, and
redundancy, each a narrow density around its point
estimate.](nestimate-compatibility_files/figure-html/entropy-bayes-plot-1.png)

The entropy rate is 1.346 bits against a ceiling of `log2(3) = 1.585` —
0.849 on the normalised scale — with a credible interval two hundredths
wide: with 30,000 transitions, the *uncertainty* about how unpredictable
the process is, is itself tiny. The redundancy of 0.098 bits is the gap
between the marginal entropy and the conditional one: knowing today’s
state removes about 7% of tomorrow’s uncertainty.

[`entropy_network()`](https://saqr.me/Nestimate/reference/entropy_network.html)
maps the same quantity onto the edges — each entry is an edge’s
contribution to the row’s unpredictability — and returns a network of
the same class, so it can be plotted or pruned like any other:

``` r

entropy_network(walk_model)
#> Network (method: entropy) [directed]
#>   Weights: [0.065, 0.235]  |  mean: 0.150
#> 
#>   Weight matrix:
#>             sedentary moderate active
#>   sedentary     0.074    0.073  0.065
#>   moderate      0.171    0.206  0.218
#>   active        0.111    0.235  0.192 
#> 
#>   Initial probabilities:
#>   active        0.576  ████████████████████████████████████████
#>   moderate      0.311  ██████████████████████
#>   sedentary     0.113  ████████
```

## Does history beyond one step matter?

The model assumes the next state depends only on the current one.
[`markov_order_test()`](https://saqr.me/Nestimate/reference/markov_order_test.html)
(Anderson & Goodman, 1957) tests that assumption.

``` r

set.seed(2026)
walk_order <- markov_order_test(walk_model)

walk_order
#> Markov Order Test  [within-w permutation, n_perm = 500, alpha = 0.050]
#>   151 sequences / 31043 observations / 3 states
#> 
#>   Selected order  BIC: 3   AIC: 3   permutation-LRT: 3
#> 
#>  order    loglik      AIC      BIC df      g2 p_permutation  p_asymptotic
#>      0 -31051.63 62107.27 62123.95 NA      NA            NA            NA
#>      1 -28961.91 57939.82 58006.57  4 4179.40   0.001996008  0.000000e+00
#>      2 -28209.20 56470.40 56687.32 12 1505.37   0.001996008 2.624960e-315
#>      3 -27901.81 55963.62 56631.07 36  614.68   0.001996008 1.933327e-106
#>  significant
#>           NA
#>         TRUE
#>         TRUE
#>         TRUE
```

``` r

plot(walk_order)
```

![Model-selection chart of log-likelihood, AIC, and BIC across Markov
orders zero to three, all favouring orders above
one.](nestimate-compatibility_files/figure-html/markov-order-plot-1.png)

BIC, AIC, and the permutation LRT all select order 3: yesterday carries
information about tomorrow beyond today.
[`path_dependence()`](https://saqr.me/Nestimate/reference/path_dependence.html)
says how much:

``` r

walk_history <- path_dependence(walk_model)

walk_history
#> Path Dependence (order 2 vs order 1, bits)
#> 
#> Contexts: 9 (min_count = 5).  Modal-prediction flips: 2.
#> Chain-level KL_weighted      = 0.035 bits
#> Chain-level H_drop_weighted  = 0.035 bits
#> 
#> Top 9 contexts by KL:
#>                 context    n H_order1 H_orderk H_drop    KL   top_o1    top_ok
#>     sedentary -> active  870    1.202    1.486 -0.284 0.164   active    active
#>   sedentary -> moderate 1912    1.444    1.478 -0.033 0.100 moderate  moderate
#>     active -> sedentary  847    1.516    1.552 -0.036 0.080 moderate  moderate
#>      active -> moderate 4410    1.444    1.374  0.071 0.051 moderate    active
#>  sedentary -> sedentary 1509    1.516    1.439  0.078 0.041 moderate sedentary
#>      moderate -> active 4363    1.202    1.323 -0.121 0.038   active    active
#>        active -> active 8544    1.202    1.056  0.146 0.020   active    active
#>    moderate -> moderate 6348    1.444    1.411  0.033 0.008 moderate  moderate
#>   moderate -> sedentary 1938    1.516    1.486  0.030 0.006 moderate  moderate
#>  flips
#>  FALSE
#>  FALSE
#>  FALSE
#>   TRUE
#>   TRUE
#>  FALSE
#>  FALSE
#>  FALSE
#>  FALSE
```

``` r

plot(walk_history)
```

![Bar chart of the KL divergence contributed by each two-step context,
all values below 0.17
bits.](nestimate-compatibility_files/figure-html/path-dependence-plot-1.png)

The weighted divergence is 0.035 bits: history shifts the next-step
distribution by a few hundredths of a bit and flips the modal prediction
in only two of nine contexts. This is the pair of numbers that
reconciles a decisively significant order test with the continued use of
a first-order model — detectable is not the same as large. The
first-order network remains a fair one-step summary; it is not the full
process.

[`path_counts()`](https://saqr.me/Nestimate/reference/path_counts.html)
is the raw material — every observed two-step path with its count:

``` r

path_counts(walk_model)
#>                     path count proportion
#> 1       active -> active  8575     0.2776
#> 2   moderate -> moderate  6375     0.2064
#> 3     active -> moderate  4435     0.1436
#> 4     moderate -> active  4388     0.1420
#> 5  moderate -> sedentary  1954     0.0633
#> 6  sedentary -> moderate  1919     0.0621
#> 7 sedentary -> sedentary  1521     0.0492
#> 8    sedentary -> active   871     0.0282
#> 9    active -> sedentary   854     0.0276
```

`active → active` alone is 27.8% of all transitions. And when the order
test demands more than one step,
[`build_hon()`](https://saqr.me/Nestimate/reference/build_hon.html)
estimates a higher-order network under its own per-node promotion rule:

``` r

hon <- build_hon(walk_model, max_order = 2)

pathways(hon)
#> character(0)
```

HON promotes no node here — no single context shifts the next-step
distribution enough to justify splitting a state — which is why
[`pathways()`](https://saqr.me/Nestimate/reference/pathways.html) comes
back empty. Two criteria disagreeing is a result, not a bug: the
likelihood test sees aggregate improvement, HON asks a stricter per-node
question. Report both.

## Simplifying the model

[`net_prune()`](https://saqr.me/Nestimate/reference/net_prune.html)
removes weak edges and returns the same class;
[`net_pruning_details()`](https://saqr.me/Nestimate/reference/net_pruning_details.html)
is the audit trail;
[`net_deprune()`](https://saqr.me/Nestimate/reference/net_deprune.html)
undoes it.

``` r

pruned <- net_prune(walk_model, method = "threshold", threshold = 0.3)

net_pruning_details(pruned)
#> Pruning details
#>   Method:  user-specified threshold (0.3)
#>   Removed: 3 edges
#>   Retained: 6 edges
#> 
#>       from        to     weight
#>   moderate sedentary 0.15365259
#>     active sedentary 0.06159838
#>  sedentary    active 0.20204129
```

Three edges fall: the two routes *into* sedentary and the direct
`sedentary → active` leap. Because pruning is recorded rather than
destructive, the original model is one call away:

``` r

net_deprune(pruned)
#> Transition Network (relative probabilities) [directed]
#>   Weights: [0.062, 0.619]  |  mean: 0.333
#> 
#>   Weight matrix:
#>             sedentary moderate active
#>   sedentary     0.353    0.445  0.202
#>   moderate      0.154    0.501  0.345
#>   active        0.062    0.320  0.619 
#> 
#>   Initial probabilities:
#>   active        0.576  ████████████████████████████████████████
#>   moderate      0.311  ██████████████████████
#>   sedentary     0.113  ████████
```

## How sure are we about the estimates?

Nestimate offers several answers resting on different machinery:
sequence resampling, node resampling, case dropping, sample splitting,
and a posterior.

### Sequence bootstrap

[`bootstrap_network()`](https://saqr.me/Nestimate/reference/bootstrap_network.html)
resamples the 151 sequences and re-estimates.

``` r

walk_boot <- bootstrap_network(walk_model, iter = 200, seed = 2026)

walk_boot
#>   Edge                   Mean     95% CI          p
#>   -----------------------------------------------
#>   active → active      0.617  [0.588, 0.645]  ** 
#>   moderate → moderate   0.502  [0.477, 0.523]  ** 
#>   sedentary → moderate   0.447  [0.420, 0.477]  ** 
#>   sedentary → sedentary   0.352  [0.317, 0.383]  ** 
#>   moderate → active    0.344  [0.317, 0.373]  ** 
#>   ... and 4 more significant edges
#> 
#> Bootstrap Network  [Transition Network (relative) | directed]
#>   Iterations : 200  |  Nodes : 3
#>   Edges      : 9 significant / 9 total
#>   CI         : 95%  |  Inference: stability  |  CR [0.75, 1.25]
```

[`summary()`](https://rdrr.io/r/base/summary.html) is the tidy
one-row-per-edge table:

``` r

head(summary(walk_boot), 3)
#>        from        to    weight      mean         sd     p_value  sig  ci_lower
#> 1 sedentary sedentary 0.3528184 0.3521126 0.01667936 0.004975124 TRUE 0.3174301
#> 2 sedentary  moderate 0.4451403 0.4465563 0.01462708 0.004975124 TRUE 0.4196235
#> 3 sedentary    active 0.2020413 0.2013311 0.01154200 0.004975124 TRUE 0.1790378
#>    ci_upper  cr_lower  cr_upper
#> 1 0.3829621 0.2646138 0.4410230
#> 2 0.4771233 0.3338553 0.5564254
#> 3 0.2253694 0.1515310 0.2525516
```

All nine edges survive, with confidence intervals a few hundredths wide
— what 151 sequences buy.

### Bayesian edge certainty

[`certainty()`](https://saqr.me/Nestimate/reference/certainty.html)
reaches the same verdict from a Dirichlet posterior on each row, with no
resampling.

``` r

set.seed(2026)
certainty(walk_model)
#>   Edge                   Mean     95% CI          p
#>   -----------------------------------------------
#>   active → active      0.618  [0.610, 0.627]  ***
#>   moderate → moderate   0.501  [0.493, 0.510]  ***
#>   sedentary → moderate   0.445  [0.430, 0.460]  ***
#>   sedentary → sedentary   0.353  [0.339, 0.367]  ***
#>   moderate → active    0.345  [0.337, 0.353]  ***
#>   ... and 4 more certain edges
#> 
#> Certainty (Dirichlet)  [Transition Network (relative) | directed]
#>   Prior      : Dirichlet(0.50)  |  Nodes : 3
#>   Edges      : 9 certain / 9 total
#>   CI         : 95%  |  Inference: stability  |  CR [0.75, 1.25]
```

All nine edges are certain.
[`summary()`](https://rdrr.io/r/base/summary.html) returns the per-edge
table with posterior means, intervals, and flags.

### Whole-network statistics: the vertex bootstrap

[`vertex_bootstrap()`](https://saqr.me/Nestimate/reference/vertex_bootstrap.html)
implements the Snijders–Borgatti (1999) vertex bootstrap for
network-level statistics.

``` r

vertex_bootstrap(walk_model, iter = 100, seed = 2026)
#> Vertex Bootstrap (Snijders & Borgatti)
#>   Nodes: 3 | Directed: TRUE | Replicates: 100
#>   95% CIs (percentile method)
#> 
#>       statistic observed boot_mean boot_sd   bias ci_lower ci_upper
#>         density    1.000     1.000   0.000  0.000    1.000    1.000
#>     mean_weight    0.255     0.265   0.050  0.011    0.147    0.353
#>  centralization    0.291     0.185   0.112 -0.107    0.000    0.374
#>     reciprocity    0.701     0.707   0.150  0.006    0.460    0.970
```

The mean edge weight carries a wide interval (\[0.15, 0.35\]) despite
the huge sample, because the vertex bootstrap resamples *nodes* and
there are only three — a deliberately conservative design for small node
sets.

### Case dropping, in two flavours

[`centrality_stability()`](https://saqr.me/Nestimate/reference/centrality_stability.html)
tracks the centrality *ordering* as sequences are dropped;
[`casedrop_reliability()`](https://saqr.me/Nestimate/reference/casedrop_reliability.html)
tracks the edge weights themselves. Both report the CS-coefficient of
Epskamp, Borsboom & Fried (2018).

``` r

set.seed(2026)
centrality_stability(walk_model, iter = 100)
#> Centrality Stability (100 iterations, threshold = 0.7)
#>   Drop proportions: 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9
#> 
#>   CS-coefficients:
#>     InStrength       0.90
#>     OutStrength      0.90
#>     Betweenness      0.90
```

``` r

walk_reliability <- casedrop_reliability(walk_model, iter = 100,
                                         seed = 2026)

walk_reliability
#> Edge-weight Case-dropping Stability
#>   Cases (rows of $data) : 151
#>   Edges assessed        : 6 (diagonal excluded)
#>   Iterations / prop     : 100
#>   Correlation method    : spearman
#>   CS-coefficient (r)    : 0.90  (threshold=0.70, certainty=0.95)
#> 
#> Model-level reliability across iterations (mean +/- sd per drop):
#>   drop_prop      p=0.1        p=0.2        p=0.3        p=0.4        p=0.5        p=0.6        p=0.7        p=0.8        p=0.9      
#>   mean|diff|      0.003+- 0.001   0.004+- 0.002   0.005+- 0.002   0.008+- 0.003   0.008+- 0.003   0.011+- 0.005   0.012+- 0.006   0.018+- 0.007   0.027+- 0.012
#>   MAD             0.003+- 0.001   0.004+- 0.002   0.005+- 0.002   0.007+- 0.004   0.008+- 0.003   0.009+- 0.005   0.011+- 0.006   0.015+- 0.008   0.025+- 0.012
#>   cor             1.000+- 0.000   0.999+- 0.006   0.997+- 0.013   0.993+- 0.019   0.993+- 0.019   0.990+- 0.022   0.989+- 0.027   0.977+- 0.034   0.957+- 0.042
#>   max|diff|       0.006+- 0.002   0.009+- 0.004   0.012+- 0.005   0.016+- 0.006   0.017+- 0.006   0.023+- 0.009   0.026+- 0.012   0.040+- 0.017   0.055+- 0.021
```

``` r

plot(walk_reliability)
```

![Case-dropping stability curve showing the correlation between
full-sample and subsampled edge weights staying above 0.95 even when 90
percent of sequences are
dropped.](nestimate-compatibility_files/figure-html/casedrop-plot-1.png)

Both CS-coefficients are 0.90 — the orderings and the weights survive
dropping 90% of the participants, far above the 0.7 bar for excellent.
Even at a 90% drop the mean edge-weight correlation with the full model
is 0.957.

### Split-half reliability

``` r

set.seed(2026)
network_reliability(walk_model)
#> Split-Half Reliability (1000 iterations, split = 50%)
#>   Mean Abs. Diff.     mean = 0.0193  sd = 0.0073
#>   Median Abs. Diff.   mean = 0.0172  sd = 0.0083
#>   Pearson             mean = 0.9900  sd = 0.0079
#>   Max Abs. Diff.      mean = 0.0422  sd = 0.0162
```

Randomly halving the panel 1,000 times yields two models that correlate
at 0.99 with a mean absolute edge difference of 0.02. Every verb in this
section agrees: with 151 sequences, this network is estimated about as
precisely as a 3-state model can be.

## Comparing two models

Any two models on the same alphabet can be compared. The pair here
splits the panel at the start of November — the warm and cold seasons —
with the same fixed `breaks`, so both models sit on one alphabet.

``` r

cutoff <- as.Date("2019-11-01")

early <- ts_tna(
  subset(walkers, as.Date(day) < cutoff),
  value = "steps", id = "id", time = "day",
  discretization = "threshold", breaks = c(5000, 10000),
  labels = c("sedentary", "moderate", "active")
)

late <- ts_tna(
  subset(walkers, as.Date(day) >= cutoff),
  value = "steps", id = "id", time = "day",
  discretization = "threshold", breaks = c(5000, 10000),
  labels = c("sedentary", "moderate", "active")
)
```

### The difference itself

[`subtract_networks()`](https://saqr.me/Nestimate/reference/subtract_networks.html)
returns the edge-by-edge difference as an object cograph can draw:

``` r

season_difference <- subtract_networks(early, late)

season_difference
#> Network difference (x - y): 3 nodes, 9 differing edges
#> Plot: cograph::splot(d) or cograph::plot_difference(d)
#> 
#>       from        to          x          y   difference
#>     active    active 0.63855921 0.59645097  0.042108234
#>     active  moderate 0.30333977 0.33761664 -0.034276870
#>   moderate    active 0.35520800 0.33549884  0.019709158
#>   moderate sedentary 0.14430829 0.16225831 -0.017950026
#>     active sedentary 0.05810102 0.06593238 -0.007831364
#>  sedentary sedentary 0.34985280 0.35525154 -0.005398748
#>  sedentary    active 0.20412169 0.20079435  0.003327337
#>  sedentary  moderate 0.44602552 0.44395410  0.002071411
#>   moderate  moderate 0.50048371 0.50224285 -0.001759131
```

``` r

cograph::plot_difference(season_difference)
```

![Difference network of the two seasonal models, edges coloured by the
sign of the seasonal change and all magnitudes
small.](nestimate-compatibility_files/figure-html/subtract-plot-1.png)

The largest seasonal gap on any edge is 0.042 (`active → active`).

### The permutation test

[`permutation()`](https://saqr.me/Nestimate/reference/permutation.html)
shuffles sequences between the two groups to build a null distribution
per edge.

``` r

season_test <- permutation(early, late, iter = 1000, seed = 2026)

summary(season_test)
#>        from        to   weight_x   weight_y         diff effect_size    p_value
#> 1 sedentary sedentary 0.34985280 0.35525154 -0.005398748 -0.17995637 0.86013986
#> 2 sedentary  moderate 0.44602552 0.44395410  0.002071411  0.08197479 0.92607393
#> 3 sedentary    active 0.20412169 0.20079435  0.003327337  0.16242006 0.86213786
#> 4  moderate sedentary 0.14430829 0.16225831 -0.017950026 -1.22971823 0.22977023
#> 5  moderate  moderate 0.50048371 0.50224285 -0.001759131 -0.09268364 0.93106893
#> 6  moderate    active 0.35520800 0.33549884  0.019709158  0.85773223 0.39460539
#> 7    active sedentary 0.05810102 0.06593238 -0.007831364 -0.93677408 0.34165834
#> 8    active  moderate 0.30333977 0.33761664 -0.034276870 -1.76872460 0.07092907
#> 9    active    active 0.63855921 0.59645097  0.042108234  1.75754195 0.08491508
#>     sig
#> 1 FALSE
#> 2 FALSE
#> 3 FALSE
#> 4 FALSE
#> 5 FALSE
#> 6 FALSE
#> 7 FALSE
#> 8 FALSE
#> 9 FALSE
```

No edge reaches significance; the smallest p-value is 0.07
(`active → moderate`). The seasons differ in *composition* (the workflow
vignette shows active days falling from 46.9% to 42.8%) while no
evidence places the difference in the *dynamics*.

### The metric panel

``` r

season_panel <- compare_model(early, late)

season_panel
#> Network comparison
#> ==================
#> Summary metrics:
#>              category               metric    value
#>     Weight Deviations      Mean Abs. Diff.  0.01494
#>     Weight Deviations    Median Abs. Diff. 0.007831
#>     Weight Deviations            RMS Diff.  0.02046
#>     Weight Deviations       Max Abs. Diff.  0.04211
#>     Weight Deviations Rel. Mean Abs. Diff.  0.04481
#>     Weight Deviations             CV Ratio    1.073
#>          Correlations              Pearson   0.9948
#>          Correlations             Spearman     0.95
#>          Correlations              Kendall   0.8889
#>          Correlations             Distance   0.9867
#>       Dissimilarities            Euclidean  0.06138
#>       Dissimilarities            Manhattan   0.1344
#>       Dissimilarities             Canberra   0.2578
#>       Dissimilarities          Bray-Curtis  0.02241
#>       Dissimilarities            Frobenius  0.05012
#>          Similarities               Cosine   0.9986
#>          Similarities              Jaccard   0.9562
#>          Similarities                 Dice   0.9776
#>          Similarities              Overlap   0.9776
#>          Similarities                   RV   0.9999
#>  Pattern Similarities       Rank Agreement        1
#>  Pattern Similarities       Sign Agreement        1
#> 
#> Network metrics (x vs y):
#>                       metric        x      y
#>                   Node Count        3      3
#>                   Edge Count        9      9
#>              Network Density        1      1
#>                Mean Distance   0.2507 0.2577
#>            Mean Out-Strength        1      1
#>              SD Out-Strength   0.3886 0.3686
#>             Mean In-Strength        1      1
#>               SD In-Strength 7.85e-17      0
#>              Mean Out-Degree        3      3
#>                SD Out-Degree        0      0
#>  Centralization (Out-Degree)        0      0
#>   Centralization (In-Degree)        0      0
#>                  Reciprocity        1      1
```

``` r

plot(season_panel)
```

![Comparison panel chart of the two seasonal models across
weight-deviation, correlation, and similarity metrics, all indicating
near-identity.](nestimate-compatibility_files/figure-html/compare-plot-1.png)

A Pearson correlation of 0.995 between the two matrices: the seasons
share their dynamics almost entirely.

### The instructive disagreement

[`bayes_compare()`](https://saqr.me/Nestimate/reference/bayes_compare.html)
asks whether each edge difference is *credibly nonzero*:

``` r

set.seed(2026)
season_bayes <- bayes_compare(early, late)

season_bayes
#> Bayesian Dirichlet-Multinomial Comparison: Transition Network (relative probabilities) 
#>   Prior: Dirichlet(0.50)  |  Draws: 10000  |  CI: 95%
#>   Thresholds: |mean diff| > 0.010, nearest CI bound > 0.001
#>   Nodes: 3  |  Edges compared: 9  |  Credibly different: 4
```

``` r

plot(season_bayes)
```

![Posterior difference intervals for the nine edges, four of which
exclude zero while all magnitudes stay below
0.05.](nestimate-compatibility_files/figure-html/bayes-compare-plot-1.png)

Four of nine edges are credibly different, while the permutation test
above found none significant. Both are right, about different questions.
With roughly 30,000 transitions, differences of two to four hundredths
are estimated precisely enough for the posterior to exclude zero (the
credible-difference threshold here is 0.01) — but the sequence-level
permutation null, which respects the fact that days are nested in
people, absorbs them. Precision detects that the difference exists; it
does not make 0.04 large. Report the magnitude next to whichever test is
used.

[`vertex_compare()`](https://saqr.me/Nestimate/reference/vertex_compare.html)
runs the node-resampling comparison at the level of whole-network
statistics and, consistent with all of the above, finds nothing (all p ≥
0.78):

``` r

vertex_compare(early, late, iter = 200, seed = 2026)
#> Two-Network Vertex Bootstrap Comparison (Snijders & Borgatti)
#>   x (3 nodes) vs y (3 nodes) | 200 replicates each
#> 
#>       statistic observed_x observed_y   diff se_diff      z p_value ci_lower
#>         density      1.000      1.000  0.000   0.000     NA      NA    0.000
#>     mean_weight      0.252      0.258 -0.006   0.076 -0.077   0.939   -0.155
#>  centralization      0.290      0.292 -0.001   0.149 -0.010   0.992   -0.293
#>     reciprocity      0.669      0.729 -0.060   0.211 -0.283   0.777   -0.474
#>  ci_upper sig
#>     0.000    
#>     0.143    
#>     0.290    
#>     0.354    
#> ---
#> Signif. codes: *** p<0.001, ** p<0.01, * p<0.05
```

## Link prediction

[`predict_links()`](https://saqr.me/Nestimate/reference/predict_links.html)
scores unobserved dyads by six structural predictors:

``` r

predict_links(walk_model)
#> Link Prediction  [directed | weighted | 3 nodes | 6 existing edges]
#>   Methods: common_neighbors, resource_allocation, adamic_adar, jaccard, preferential_attachment, katz
```

It runs, but a three-state transition network is fully connected between
states, which leaves link prediction nothing to rank; it earns its place
on larger, pruned alphabets, where a missing edge is a real hypothesis.

## Grouped models

When the comparison variable is a column of the data,
`ts_tna(group = ...)` builds one network per level and returns a
Nestimate `netobject_group`. The grouped verbs verified against it are
[`net_centrality()`](https://saqr.me/Nestimate/reference/net_centrality.html),
[`state_distribution()`](https://saqr.me/Nestimate/reference/state_distribution.html),
[`transition_entropy()`](https://saqr.me/Nestimate/reference/transition_entropy.html),
[`passage_time()`](https://saqr.me/Nestimate/reference/passage_time.html),
[`certainty()`](https://saqr.me/Nestimate/reference/certainty.html),
[`net_prune()`](https://saqr.me/Nestimate/reference/net_prune.html),
[`compare_model()`](https://saqr.me/Nestimate/reference/compare_model.html),
[`permutation()`](https://saqr.me/Nestimate/reference/permutation.html),
[`bootstrap_network()`](https://saqr.me/Nestimate/reference/bootstrap_network.html),
[`markov_order_test()`](https://saqr.me/Nestimate/reference/markov_order_test.html),
[`centrality_stability()`](https://saqr.me/Nestimate/reference/centrality_stability.html),
[`casedrop_reliability()`](https://saqr.me/Nestimate/reference/casedrop_reliability.html),
and
[`mosaic_plot()`](https://saqr.me/Nestimate/reference/mosaic_plot.html);
[`summary()`](https://rdrr.io/r/base/summary.html) and
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) return
tidy per-group tables, and
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) draws one
selected group.
[`net_edge_betweenness()`](https://saqr.me/Nestimate/reference/net_edge_betweenness.html)
also runs, but its result drops back to a plain `netobject_group`. The
dedicated vignette
[`vignette("group-models", package = "tsn")`](https://pak.dynasite.org/tsn/articles/group-models.md)
works through all of it.

## What does not apply, and why

Three families of Nestimate verbs reject a transition model, each for a
principled reason rather than a missing feature:

- **Sequence-data verbs.**
  [`frequencies()`](https://saqr.me/Nestimate/reference/frequencies.html),
  [`mark_first_state()`](https://saqr.me/Nestimate/reference/mark_first_state.html),
  [`mark_terminal_state()`](https://saqr.me/Nestimate/reference/mark_terminal_state.html),
  [`entropy_trajectory()`](https://saqr.me/Nestimate/reference/entropy_trajectory.html),
  [`mosaic_analysis()`](https://saqr.me/Nestimate/reference/mosaic_analysis.html),
  [`plot_mosaic()`](https://saqr.me/Nestimate/reference/plot_mosaic.html),
  and [`as_htna()`](https://saqr.me/Nestimate/reference/as_htna.html)
  operate on a raw wide data frame of sequences, not on an estimated
  model. They belong *before* model construction in a workflow, applied
  to sequence data you hold yourself.
- **Psychometric-network verbs.**
  [`predictability()`](https://saqr.me/Nestimate/reference/predictability.html)
  and the `glasso`/`pcor`/`cor` estimator family need a precision or
  correlation structure estimated from multivariate data. A transition
  matrix is a conditional-probability object, not a
  conditional-independence one; the quantities these verbs compute are
  undefined for it.
- **Verbs that need extra structure.**
  [`cluster_network()`](https://saqr.me/Nestimate/reference/cluster_network.html)
  and
  [`build_clusters()`](https://saqr.me/Nestimate/reference/build_clusters.html)
  require a chosen number of clusters and are aimed at multi-sequence
  typologies; [`wtna()`](https://saqr.me/Nestimate/reference/wtna.html)
  needs one-hot action data;
  [`sequence_compare()`](https://saqr.me/Nestimate/reference/sequence_compare.html)
  wants sequence data plus a grouping vector. And
  [`as_tna()`](https://saqr.me/Nestimate/reference/as_tna.html) is
  unnecessary by design — a tsn model needs no conversion, which is the
  point of this vignette.

## The compatibility map

| Question | Verb | Tidy access | Plot | Notes |
|----|----|----|----|----|
| What are the estimates? | [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html), [`as.matrix()`](https://rdrr.io/r/base/matrix.html), [`extract_transition_matrix()`](https://saqr.me/Nestimate/reference/extract_transition_matrix.html), [`extract_initial_probs()`](https://saqr.me/Nestimate/reference/extract_initial_probs.html) | direct | via [`plot()`](https://rdrr.io/r/graphics/plot.default.html) on the model | tsn accessors include the diagonal |
| Between-state edges only | [`extract_edges()`](https://saqr.me/Nestimate/reference/extract_edges.html) | direct | — | **excludes self-transitions** |
| Marginal composition | [`state_distribution()`](https://saqr.me/Nestimate/reference/state_distribution.html), [`state_frequencies()`](https://saqr.me/Nestimate/reference/state_frequencies.html) | data frame | [`mosaic_plot()`](https://saqr.me/Nestimate/reference/mosaic_plot.html), [`plot_state_frequencies()`](https://saqr.me/Nestimate/reference/plot_state_frequencies.html) |  |
| Composition over time / carpet | [`distribution_plot()`](https://saqr.me/Nestimate/reference/distribution_plot.html), [`sequence_plot()`](https://saqr.me/Nestimate/reference/sequence_plot.html) | — | draws directly | stacked area; sequence heatmap |
| State/edge importance | [`net_centrality()`](https://saqr.me/Nestimate/reference/net_centrality.html), [`net_edge_betweenness()`](https://saqr.me/Nestimate/reference/net_edge_betweenness.html) | data frame / network | [`plot()`](https://rdrr.io/r/graphics/plot.default.html) on both | loops excluded from centrality by default |
| Travel times between states | [`passage_time()`](https://saqr.me/Nestimate/reference/passage_time.html) | print | [`plot()`](https://rdrr.io/r/graphics/plot.default.html) | Kemeny & Snell (1976) |
| Persistence, recurrence, sojourn | [`markov_stability()`](https://saqr.me/Nestimate/reference/markov_stability.html) | `$stability` table | [`plot()`](https://rdrr.io/r/graphics/plot.default.html) |  |
| Chain diagnostics | [`chain_structure()`](https://saqr.me/Nestimate/reference/chain_structure.html) | [`summary()`](https://rdrr.io/r/base/summary.html) | [`plot()`](https://rdrr.io/r/graphics/plot.default.html) heatmap | irreducibility, reversibility |
| Predictability | [`transition_entropy()`](https://saqr.me/Nestimate/reference/transition_entropy.html), [`entropy_bayes()`](https://saqr.me/Nestimate/reference/entropy_bayes.html), [`entropy_network()`](https://saqr.me/Nestimate/reference/entropy_network.html) | print / [`summary()`](https://rdrr.io/r/base/summary.html) | [`plot()`](https://rdrr.io/r/graphics/plot.default.html) on both entropy results | rate in bits; ceiling `log2(n_states)` |
| History beyond one step | [`markov_order_test()`](https://saqr.me/Nestimate/reference/markov_order_test.html), [`path_dependence()`](https://saqr.me/Nestimate/reference/path_dependence.html), [`path_counts()`](https://saqr.me/Nestimate/reference/path_counts.html), [`build_hon()`](https://saqr.me/Nestimate/reference/build_hon.html) + [`pathways()`](https://saqr.me/Nestimate/reference/pathways.html) | print / data frame | [`plot()`](https://rdrr.io/r/graphics/plot.default.html) on test and dependence | test the size, not only the p |
| Simplification | [`net_prune()`](https://saqr.me/Nestimate/reference/net_prune.html), [`net_pruning_details()`](https://saqr.me/Nestimate/reference/net_pruning_details.html), [`net_deprune()`](https://saqr.me/Nestimate/reference/net_deprune.html) | same class back | via model [`plot()`](https://rdrr.io/r/graphics/plot.default.html) | prune after testing |
| Edge uncertainty | [`bootstrap_network()`](https://saqr.me/Nestimate/reference/bootstrap_network.html), [`certainty()`](https://saqr.me/Nestimate/reference/certainty.html) | [`summary()`](https://rdrr.io/r/base/summary.html) | — | resampling vs Bayesian |
| Network-level uncertainty | [`vertex_bootstrap()`](https://saqr.me/Nestimate/reference/vertex_bootstrap.html) | data frame | [`plot()`](https://rdrr.io/r/graphics/plot.default.html) | Snijders & Borgatti (1999) |
| Sample-size reliability | [`centrality_stability()`](https://saqr.me/Nestimate/reference/centrality_stability.html), [`casedrop_reliability()`](https://saqr.me/Nestimate/reference/casedrop_reliability.html), [`network_reliability()`](https://saqr.me/Nestimate/reference/network_reliability.html) | print | [`plot()`](https://rdrr.io/r/graphics/plot.default.html) on all three | CS-coefficient bar: 0.5 / 0.7 |
| Two-model comparison | [`permutation()`](https://saqr.me/Nestimate/reference/permutation.html), [`compare_model()`](https://saqr.me/Nestimate/reference/compare_model.html), [`subtract_networks()`](https://saqr.me/Nestimate/reference/subtract_networks.html), [`bayes_compare()`](https://saqr.me/Nestimate/reference/bayes_compare.html), [`vertex_compare()`](https://saqr.me/Nestimate/reference/vertex_compare.html) | [`summary()`](https://rdrr.io/r/base/summary.html) / data frame | [`plot()`](https://rdrr.io/r/graphics/plot.default.html); [`cograph::plot_difference()`](https://sonsoles.me/cograph/reference/plot_difference.html) | report magnitudes with tests |
| Link prediction | [`predict_links()`](https://saqr.me/Nestimate/reference/predict_links.html) | print | — | needs a sparse alphabet to be informative |
| Many-group comparison | `ts_tna(group=)` + grouped verbs | [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) | `plot(x, group=)`, [`mosaic_plot()`](https://saqr.me/Nestimate/reference/mosaic_plot.html) | see [`vignette("group-models")`](https://pak.dynasite.org/tsn/articles/group-models.md) |
| Rendering | [`plot()`](https://rdrr.io/r/graphics/plot.default.html), [`cograph::splot()`](https://sonsoles.me/cograph/reference/splot.html) | — | — | requires cograph |

## References

Anderson, T. W., & Goodman, L. A. (1957). Statistical inference about
Markov chains. *The Annals of Mathematical Statistics*, 28(1), 89–110.

Epskamp, S., Borsboom, D., & Fried, E. I. (2018). Estimating
psychological networks and their accuracy: A tutorial paper. *Behavior
Research Methods*, 50(1), 195–212.

Kemeny, J. G., & Snell, J. L. (1976). *Finite Markov Chains*. Springer.

Snijders, T. A. B., & Borgatti, S. P. (1999). Non-parametric standard
errors and tests for network statistics. *Connections*, 22(2), 161–170.

Xu, J., Wickramarathne, T. L., & Chawla, N. V. (2016). Representing
higher-order dependencies in networks. *Science Advances*, 2(5),
e1600028.
