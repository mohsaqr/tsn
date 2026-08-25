# Transition Networks from Time Series: Estimation and Inference with Nestimate

## Introduction

A student rates their momentary anxiety on a 0-100 scale, again and
again over a term. That record is a numeric time series: one quantity,
measured repeatedly in time. A transition network asks a different
question of the same record. Once the continuous scale is reduced to a
few qualitative *states* such as `low`, `mid`, and `high`, the network’s
nodes are those states and its directed edges the probability of passing
from one to the next. The series says how high a measurement was; the
network says what a measurement is likely to be followed by.

Recovering the network from the series takes two steps: reducing the
continuous series to a sequence of states, and estimating the
state-to-state transitions. There is a third step that is easy to skip,
and it is the one this vignette keeps in view: deciding what the fitted
model may legitimately claim.

The **tsn** package performs the first two steps in a single call and
hands the result to **Nestimate**, which supplies the third.
[`ts_tna()`](https://pak.dynasite.org/tsn/reference/ts_tna.md)
discretizes a series, estimates the transition network, and returns a
native Nestimate model, so Nestimate’s descriptive and inferential verbs
apply to it with no conversion step in between. This vignette follows
one analysis through the whole path: building the model, reading its
structure, and, where the data support it, testing that structure with
bootstrap confidence intervals, centrality stability, a Markov-order
test, and a permutation comparison of two periods.

The distinction between description and inference organizes everything
below. Descriptive summaries such as centralities, state distributions,
entropy, and pruning ask what a fitted network looks like, and a network
fitted to a single series answers them. Inference asks how far the
estimate would move under a different sample, and a single series is a
single sample: there is nothing to resample. The dividing line is the
unit of analysis, and knowing where it falls is what keeps a transition
model from being read for more than it can support.

Two bundled data sets carry the analysis. `esm_srl` is an
experience-sampling study in which 41 students rated momentary
self-regulation, motivation, and anxiety on 0-100 scales; `srl` is a
balanced daily panel in which 36 students reported nine
self-regulated-learning indicators for 156 occasions each.

``` r

library(tsn)
library(Nestimate)

data(esm_srl)
data(srl)
```

## From measurements to a model

[`ts_tna()`](https://pak.dynasite.org/tsn/reference/ts_tna.md) takes a
data frame and the name of one numeric column, cuts that column into
states, and estimates the transition network in one call. Here it runs
on the 79 momentary `anxiety` reports of one student, Jamal, from the
`esm_srl` study.

``` r

jamal <- subset(esm_srl, name == "Jamal")

model <- ts_tna(
  jamal,
  series = "anxiety",
  labels = c("low", "mid", "high")
)

model
#> Transition Network (relative probabilities) [directed]
#>   Weights: [0.231, 0.423]  |  mean: 0.333
#> 
#>   Weight matrix:
#>          low   mid  high
#>   low  0.370 0.370 0.259
#>   mid  0.240 0.360 0.400
#>   high 0.423 0.231 0.346 
#> 
#>   Initial probabilities:
#>   low           1.000  ████████████████████████████████████████
#>   mid           0.000  
#>   high          0.000
```

The printed object is the complete model: a row-stochastic weight matrix
whose entry *(i, j)* is the probability of moving to state *j* given
that the system is currently in state *i*, together with the
initial-state distribution. Each row sums to one because from any state
the process must go somewhere. One entry already rewards reading: after
a high-anxiety report, the most likely next state is `low` (0.423), so
for this student high anxiety resolves sharply rather than escalating.

### The bridge: this object *is* a Nestimate model

Everything that follows rests on one property of the returned object.

``` r

class(model)
#> [1] "ts_tna"          "netobject"       "cograph_network"
```

[`ts_tna()`](https://pak.dynasite.org/tsn/reference/ts_tna.md) does not
return a private structure that must be converted before Nestimate can
read it. It returns an object that inherits from **`netobject`**,
Nestimate’s core network class, and Nestimate’s verbs dispatch on that
class directly. There is no export step and no adapter call between the
model and the tests applied to it, which is what lets a single analysis
move from construction to inference without leaving the object behind.

### Four constructors, one interface

The same discretized sequence supports four definitions of an edge, and
the four constructors take identical arguments and return the same
class. [`ts_tna()`](https://pak.dynasite.org/tsn/reference/ts_tna.md)
gives relative frequencies, so an edge is the conditional probability of
the next state.
[`ts_ftna()`](https://pak.dynasite.org/tsn/reference/ts_tna.md) gives
the raw transition counts behind those probabilities.
[`ts_atna()`](https://pak.dynasite.org/tsn/reference/ts_tna.md) applies
attention weighting, and
[`ts_cna()`](https://pak.dynasite.org/tsn/reference/ts_tna.md) counts
undirected co-occurrence rather than directed movement. Changing the
edge definition means changing only the constructor.

``` r

ts_ftna(jamal, series = "anxiety", labels = c("low", "mid", "high"))
#> Transition Network (frequency counts) [directed]
#>   Weights: [6.000, 11.000]  |  mean: 8.667
#> 
#>   Weight matrix:
#>        low mid high
#>   low   10  10    7
#>   mid    6   9   10
#>   high  11   6    9 
#> 
#>   Initial probabilities:
#>   low           1.000  ████████████████████████████████████████
#>   mid           0.000  
#>   high          0.000
```

The remainder of the vignette uses
[`ts_tna()`](https://pak.dynasite.org/tsn/reference/ts_tna.md), because
the inferential tests are written for probabilities.

## One series, and what it can support

Asking a single-series model for a bootstrap returns a warning rather
than a result:

> A network with one long sequence is not recommended and can’t be
> validated using bootstrap and other confirmatory testings.

The warning is correct. A bootstrap resamples *sequences* to estimate
how far a quantity would move under a different sample of them; one
series is one sequence, so there is nothing to resample and no sampling
distribution to form. The permutation and stability tests rest on the
same logic.

This is a statement about the unit of analysis, not a dead end. A long
series contains many transitions; what it lacks is a way to group them
into units that can be resampled. The `segment` argument supplies that
unit by cutting the series into shorter blocks, each treated as its own
sequence. This is the block bootstrap, and tsn provides its two standard
forms.

The series below is one student’s 156 daily `effort` reports from the
`srl` panel. Two features of the call matter before the segmenting
itself. States are cut at 40 and 70 on the 0-100 scale with
`discretization = "threshold"` rather than at the default tertiles, so
the states mean the same thing outside this sample; the next section
returns to why that matters. And segmentation is applied *after*
discretization, so every block inherits the same cut points and no block
is ever discretized against itself.

``` r

students <- subset(srl, !is.na(effort))
erik <- subset(students, name == "Erik")

whole <- ts_tna(
  erik,
  value = "effort",
  time = "day",
  discretization = "threshold",
  breaks = c(40, 70),
  labels = c("low", "moderate", "high")
)
```

Partitioning cuts the 156 observations into consecutive, non-overlapping
blocks. At `segment = 10` there are sixteen of them.

``` r

blocks <- ts_tna(
  erik,
  value = "effort",
  time = "day",
  segment = 10,
  discretization = "threshold",
  breaks = c(40, 70),
  labels = c("low", "moderate", "high")
)

bootstrap_network(blocks, iter = 500, seed = 2026)
#>   Edge                   Mean     95% CI          p
#>   -----------------------------------------------
#>   low → low            0.451  [0.358, 0.547]  *  
#> 
#> Bootstrap Network  [Transition Network (relative) | directed]
#>   Iterations : 500  |  Nodes : 3
#>   Edges      : 1 significant / 9 total
#>   CI         : 95%  |  Inference: stability  |  CR [0.75, 1.25]
```

Sixteen sequences, and the bootstrap now runs. Partitioning is not free:
every cut destroys the transition that straddles it, so sixteen blocks
cost fifteen of the 155 transitions, about ten percent, and the estimate
shifts slightly.

Sliding the window instead of partitioning keeps all of them. At
`segment = 2, overlap = TRUE` the blocks are the consecutive lag-1
pairs, so every transition appears exactly once and the network is
*identical* to the unsegmented one.

``` r

lag_one <- ts_tna(
  erik,
  value = "effort",
  time = "day",
  segment = 2,
  overlap = TRUE,
  discretization = "threshold",
  breaks = c(40, 70),
  labels = c("low", "moderate", "high")
)

all.equal(as.matrix(lag_one), as.matrix(whole))
#> [1] TRUE
```

``` r

bootstrap_network(lag_one, iter = 500, seed = 2026)
#> Bootstrap Network  [Transition Network (relative) | directed]
#>   Iterations : 500  |  Nodes : 3
#>   Edges      : 0 significant / 9 total
#>   CI         : 95%  |  Inference: stability  |  CR [0.75, 1.25]
```

The two schemes trade the point estimate against independence. Sliding
blocks preserve the estimate exactly and yield the most units;
partitioned blocks respect whatever dependence exists inside a block, at
the price of the boundary transitions lost to the cuts. On this series
the two run neck and neck: mean interval widths of 0.253 for the
partitioned blocks against 0.254 for the lag-1 pairs, with a single edge
crossing the stability criterion under partitioning and none under
sliding. That is the real lesson of the exercise: 156 volatile
observations of one person yield intervals a quarter of a probability
unit wide however they are resampled. Where the two schemes do differ,
prefer partitioning, because resampling lag-1 pairs assumes exactly the
first-order Markov property the model itself assumes, and the
Markov-order test below rejects that property for these data. Blocks of
10 to 30 retain roughly 90% to 97% of the transitions, which is usually
the right trade; blocks of 2 retain only half and are better avoided.

None of this manufactures information. Segmenting a single series asks
how stable one person’s own dynamics are, and says nothing about anyone
else. A claim about people in general needs people in general, so the
rest of the vignette uses all 36 of them.

### Choosing states that mean something

The default `discretization = "quantile"` cuts at the tertiles, forcing
each of the three states to hold a third of the observations, so the
state sizes carry no information, by construction. That is why every
model built from the `srl` panel uses fixed thresholds of 40 and 70 on
the 0-100 effort scale instead. Fixed breaks make the states
interpretable outside the sample, since below 40 is low effort wherever
it occurs, and they give every model one shared alphabet, which is what
makes the period comparison below legitimate. (The grouped `esm_srl`
model at the end keeps the quantile default; grouping pools the series
before cutting it, so its groups share an alphabet without breaks being
named.)

``` r

effort_model <- ts_tna(
  students,
  value = "effort",
  id = "name",
  time = "day",
  discretization = "threshold",
  breaks = c(40, 70),
  labels = c("low", "moderate", "high")
)

effort_model
#> Transition Network (relative probabilities) [directed]
#>   Weights: [0.165, 0.575]  |  mean: 0.333
#> 
#>   Weight matrix:
#>              low moderate  high
#>   low      0.415    0.314 0.272
#>   moderate 0.218    0.453 0.329
#>   high     0.165    0.260 0.575 
#> 
#>   Initial probabilities:
#>   high          0.417  ████████████████████████████████████████
#>   moderate      0.389  █████████████████████████████████████
#>   low           0.194  ███████████████████
```

The states now mean something outside the data, the same cut points
apply to every comparison made later, and the state distribution is
worth reading.

``` r

state_distribution(effort_model)
#>   group    state count proportion
#> 1   all     high  2341  0.4174394
#> 2   all moderate  1900  0.3388017
#> 3   all      low  1367  0.2437589
```

High effort is the panel’s most common state at 41.7% of the days, and
it is also the most persistent: the `high -> high` probability of 0.575
is the largest entry in the matrix. Effort, once high, tends to
continue, and the asymmetry runs one way: a low-effort day returns to
`low` at only 0.415.

### One network per person

The pooled model estimates a single network across everyone.
[`series_networks()`](https://pak.dynasite.org/tsn/reference/series_networks.md)
splits it into one complete model per student, each rebuilt on the same
alphabet so node sets stay aligned and comparable.

``` r

per_person <- series_networks(effort_model)

head(summary(per_person))
#>   series type observations states edges
#> 1  Aisha  tna          156      3     9
#> 2  Alice  tna          156      3     9
#> 3  Anika  tna          156      3     9
#> 4 Astrid  tna          156      3     7
#> 5  Bjorn  tna          156      3     9
#> 6    Bob  tna          156      3     9
```

Each row indexes a full `ts_tna` model that prints and plots on its own:
the models are the list elements, and this is the tidy index over them.
Because the cut points are fixed scale positions rather than sample
quantiles, a student’s personal network describes their own effort, not
their rank within the panel. The index already shows individual
structure: one of the first six students never makes two of the nine
transitions at all, so their network carries seven edges rather than
nine.

## Describing the model

### Centrality

``` r

net_centrality(effort_model)
#> centralities computed excluding loops (diagonal). Pass `loops = TRUE` to include self-transitions.
#>             state InStrength Betweenness Diffusion
#> low           low  0.3832538           0 1.0000000
#> moderate moderate  0.5738366           0 0.7292556
#> high         high  0.6003974           0 0.0000000
```

The message notes that centralities exclude self-transitions by default,
which matters for a transition network: the diagonal is usually the
largest entry, so a state can be highly persistent while receiving
little traffic from elsewhere. Excluding the diagonal measures how
central a state is to *movement* between states rather than to staying
put. Here `high` and `moderate` receive similar between-state traffic
(in-strengths of 0.60 and 0.57) while `low` receives markedly less
(0.38); the betweenness column is uniformly zero because in a fully
connected three-state network no state is an obligatory bridge. Passing
`loops = TRUE` includes the diagonal.

### How much of the sequence is structure, and how much is noise

``` r

transition_entropy(effort_model)
#> Transition Entropy (3 states, bits; ceiling = 1.585)
#> 
#>                           raw            normalised
#>   Entropy rate    h(P)  = 1.479 bits    0.933
#>   Stationary    H(pi)  = 1.552 bits    0.979
#>   Redundancy   H(pi)-h = 0.073 bits    0.047
#> 
#> Normalised: h(P) and H(pi) are raw / log_2(n_states) (0 = deterministic, 1 = uniform);
#>   redundancy is the relative redundancy (H(pi) - h(P)) / H(pi), not raw / log_2(n_states).
```

The entropy rate is normalized against `log2(n_states)`, so 0 is a
perfectly deterministic process and 1 means every next state is equally
likely from wherever the process currently sits. A value of 1 requires
more than an uninformative current state: a lopsided but
state-independent process still scores well below it. Cut at the
tertiles, these same effort reports score 0.96; cut at 40 and 70 they
score 0.93. The fixed thresholds recover slightly more structure, and
either way the message is sobering: day-to-day effort is mostly noise
around a weak first-order signal, which is worth knowing before any edge
is over-interpreted.

## Pruning

Not every edge deserves a place in the picture.
[`net_prune()`](https://saqr.me/Nestimate/reference/net_prune.html)
returns a model of the same class with weak edges removed, so its result
flows straight back into any verb above.

``` r

net_prune(effort_model, method = "threshold", threshold = 0.3)
#> Transition Network (relative probabilities) [directed]
#>   Weights: [0.314, 0.575]  |  mean: 0.417
#> 
#>   Weight matrix:
#>              low moderate  high
#>   low      0.415    0.314 0.000
#>   moderate 0.000    0.453 0.329
#>   high     0.000    0.000 0.575 
#> 
#>   Initial probabilities:
#>   high          0.417  ████████████████████████████████████████
#>   moderate      0.389  █████████████████████████████████████
#>   low           0.194  ███████████████████
```

Four edges fall below the threshold, and they are not a random four:
every *descending* route (`moderate -> low`, `high -> low`,
`high -> moderate`) disappears, together with the direct `low -> high`
leap. What survives is the ascending ladder (`low -> moderate` at 0.314,
`moderate -> high` at 0.329) and the three persistence loops. Effort
climbs stepwise and decays only weakly.

Prune after testing rather than before, however. A threshold ranks edges
by size, and size is not the same as interest: a small but reliably
estimated edge can matter more than a large but uncertain one. The
bootstrap in the next section shows exactly this happening.

## Bootstrap: which edges survive resampling?

[`bootstrap_network()`](https://saqr.me/Nestimate/reference/bootstrap_network.html)
resamples the sequences with replacement, re-estimates the network on
each resample, and reports a confidence interval for every edge.

``` r

boot <- bootstrap_network(effort_model, iter = 200, seed = 2026)

boot
#>   Edge                   Mean     95% CI          p
#>   -----------------------------------------------
#>   high → high          0.571  [0.458, 0.653]  *  
#>   moderate → moderate   0.452  [0.392, 0.508]  ** 
#>   moderate → high      0.333  [0.284, 0.385]  ** 
#>   low → moderate       0.317  [0.258, 0.375]  *  
#>   high → moderate      0.264  [0.211, 0.328]  *  
#> 
#> Bootstrap Network  [Transition Network (relative) | directed]
#>   Iterations : 200  |  Nodes : 3
#>   Edges      : 5 significant / 9 total
#>   CI         : 95%  |  Inference: stability  |  CR [0.75, 1.25]
```

[`summary()`](https://rdrr.io/r/base/summary.html) returns the tidy,
one-row-per-edge table.

``` r

summary(boot)
#>       from       to    weight      mean         sd     p_value   sig  ci_lower
#> 1      low      low 0.4148311 0.4015282 0.05284228 0.059701493 FALSE 0.3043293
#> 2      low moderate 0.3135095 0.3166900 0.03084593 0.024875622  TRUE 0.2583890
#> 3      low     high 0.2716593 0.2817819 0.03701263 0.084577114 FALSE 0.2172464
#> 4 moderate      low 0.2184517 0.2151464 0.02620083 0.059701493 FALSE 0.1630474
#> 5 moderate moderate 0.4528102 0.4519208 0.02976832 0.004975124  TRUE 0.3920565
#> 6 moderate     high 0.3287381 0.3329328 0.02742877 0.004975124  TRUE 0.2840620
#> 7     high      low 0.1648021 0.1654507 0.02913549 0.154228856 FALSE 0.1105726
#> 8     high moderate 0.2603270 0.2636688 0.03085939 0.044776119  TRUE 0.2111482
#> 9     high     high 0.5748709 0.5708806 0.05172929 0.014925373  TRUE 0.4581270
#>    ci_upper  cr_lower  cr_upper
#> 1 0.5035205 0.3111233 0.5185389
#> 2 0.3754918 0.2351322 0.3918869
#> 3 0.3582807 0.2037445 0.3395742
#> 4 0.2667262 0.1638388 0.2730647
#> 5 0.5079602 0.3396076 0.5660127
#> 6 0.3851077 0.2465536 0.4109226
#> 7 0.2260268 0.1236015 0.2060026
#> 8 0.3284161 0.1952453 0.3254088
#> 9 0.6534180 0.4311532 0.7185886
```

Five of the nine edges clear the stability criterion, and four of them
are edges pruning also kept: the `moderate` and `high` persistence loops
and the two ascending steps, `low -> moderate` and `moderate -> high`.
The fifth is where the two verdicts part. The bootstrap keeps
`high -> moderate` (0.260), a single step down from the top of the scale
that pruning had discarded as too small, and it drops `low -> low`
(0.415), the large loop pruning kept. `low -> low` is the outgoing edge
of the rarest state (`low` fills 24.4% of the days), so it rests on the
fewest transitions and just misses the criterion (p = 0.060). This is
the pruning caution made concrete: ranking edges by size and ranking
them by stability need not agree, and here they trade one edge in each
direction. The four edges that fail resampling are the long-range moves,
the `low -> high` leap and the two descents into `low`
(`moderate -> low`, `high -> low`), together with the `low -> low` near
miss. This is what a moderate panel buys: the ascending backbone and the
strong loops are firm, while the rarest-state and long-range edges are
not.

## Stability: would the same states rank first in a smaller study?

[`centrality_stability()`](https://saqr.me/Nestimate/reference/centrality_stability.html)
repeatedly drops a proportion of the sequences, re-computes the
centralities, and reports the largest proportion that can be dropped
while the ordering still tracks the original, a correlation-stability
coefficient.

``` r

set.seed(2026)
centrality_stability(effort_model, iter = 100)
#> Centrality Stability (100 iterations, threshold = 0.7)
#>   Drop proportions: 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9
#> 
#>   CS-coefficients:
#>     InStrength       0.60
#>     OutStrength      0.40
```

The in-strength ordering holds to a 0.60 drop, acceptable against the
conventional bars of 0.5 for acceptable and 0.7 for excellent, but the
out-strength ordering fails the bar at 0.40. With 36 sequences, claims
about which state *sends* the most between-state traffic should not be
made from this panel; claims about which state receives it are on firmer
ground. This is the stability check doing its job, and the honest report
includes the number that failed.

## Markov order: is a first-order model enough?

Every transition network here assumes the Markov property, that the next
state depends on the current one and nothing earlier. That assumption is
testable, by comparing the fit of models of increasing order.

``` r

set.seed(2026)
markov_order_test(effort_model)
#> Markov Order Test  [within-w permutation, n_perm = 500, alpha = 0.050]
#>   36 sequences / 5608 observations / 3 states
#> 
#>   Selected order  BIC: 2   AIC: 3   permutation-LRT: 3
#> 
#>  order   loglik      AIC      BIC df     g2 p_permutation  p_asymptotic
#>      0 -6031.21 12066.41 12079.67 NA     NA            NA            NA
#>      1 -5750.23 11516.45 11569.51  4 561.95   0.001996008 2.653668e-120
#>      2 -5606.12 11264.24 11436.67 12 288.17   0.001996008  1.422835e-54
#>      3 -5494.72 11149.43 11679.99 36 222.75   0.001996008  8.841816e-29
#>  significant
#>           NA
#>         TRUE
#>         TRUE
#>         TRUE
```

The test disagrees with the first-order model. AIC and the permutation
likelihood-ratio test prefer order 3 and BIC order 2, meaning yesterday
carries information about tomorrow beyond what today already conveys.
This does not invalidate the first-order network, but it bounds the
claim: the network is a summary of one-step behaviour, not a full
description of the process.

The natural follow-up is a higher-order network.

``` r

build_hon(effort_model, max_order = 2)
#> Higher-Order Network (HON)
#>   Nodes:        3 (3 first-order states)
#>   Edges:        9
#>   Max order:    1 (requested 2)
#>   Min freq:     1
#>   Trajectories: 36
```

The higher-order construction keeps every node at order 1: it finds no
individual context that shifts the next-step distribution enough to
justify splitting the node. The two criteria disagree because they ask
different questions. The likelihood test detects aggregate improvement
pooled across all sequences, while the higher-order rule applies a
stricter, per-node test. Both results are real, and both are worth
reporting: there is higher-order structure in aggregate, but not
localized in any single state.

## Comparing two periods

The tests so far describe one network.
[`permutation()`](https://saqr.me/Nestimate/reference/permutation.html)
compares two, shuffling the group labels to build a null distribution
for every edge. The `srl` panel covers 156 occasions per student;
splitting at occasion 78 contrasts the first half of the course with the
second, with the same 36 students on both sides.

Both models are built with the same fixed `breaks`, which matters more
than it appears: had each period been discretized on its own quantiles,
the two networks would rest on different cut points, and any difference
between them could reflect the thresholds rather than behaviour.

``` r

first_half <- ts_tna(
  subset(students, day <= 78),
  value = "effort",
  id = "name",
  time = "day",
  discretization = "threshold",
  breaks = c(40, 70),
  labels = c("low", "moderate", "high")
)

second_half <- ts_tna(
  subset(students, day > 78),
  value = "effort",
  id = "name",
  time = "day",
  discretization = "threshold",
  breaks = c(40, 70),
  labels = c("low", "moderate", "high")
)

comparison <- permutation(first_half, second_half, iter = 2000, seed = 2026)

comparison
#> Permutation Test:Transition Network (relative probabilities) [directed]
#>   Iterations: 2000  |  Alpha: 0.05
#>   Nodes: 3  |  Edges tested: 9  |  Significant: 0
```

``` r

summary(comparison)
#>       from       to  weight_x  weight_y          diff effect_size   p_value
#> 1      low      low 0.4143070 0.4152047 -0.0008976739 -0.01176955 0.9905047
#> 2      low moderate 0.3293592 0.2967836  0.0325755397  0.68417855 0.4937531
#> 3      low     high 0.2563338 0.2880117 -0.0316778658 -0.61092234 0.5532234
#> 4 moderate      low 0.2134472 0.2230523 -0.0096051227 -0.23379724 0.8345827
#> 5 moderate moderate 0.4557097 0.4514408  0.0042689434  0.08454169 0.9385307
#> 6 moderate     high 0.3308431 0.3255069  0.0053361793  0.12210174 0.9010495
#> 7     high      low 0.1665229 0.1637631  0.0027597983  0.07267935 0.9480260
#> 8     high moderate 0.2476273 0.2700348 -0.0224075783 -0.50254694 0.6276862
#> 9     high     high 0.5858499 0.5662021  0.0196477800  0.29315994 0.7611194
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

Nothing separates the halves: no edge comparison comes near significance
(the smallest p-value is 0.49) and the largest estimated difference on
any edge is 0.033. The compositions agree just as closely:

``` r

state_distribution(first_half)
#>   group    state count proportion
#> 1   all     high  1176  0.4195505
#> 2   all moderate   949  0.3385658
#> 3   all      low   678  0.2418837

state_distribution(second_half)
#>   group    state count proportion
#> 1   all     high  1165  0.4153298
#> 2   all moderate   951  0.3390374
#> 3   all      low   689  0.2456328
```

High-effort days make up 42.0% of the first half and 41.5% of the
second; every proportion moves by less than a percentage point. Level
and dynamics are both stationary across the course: whatever effort
habits these students have, they are set before the midpoint and do not
drift. A null this uniform is a finding: it licenses pooling the whole
course into the single model above, which a real first-to-second-half
shift would have forbidden.

Two cautions bound how far that carries. Failing to reject nine tests is
not the same as proving the dynamics identical, and the balanced design
helps the comparison: the same 36 students stand on both sides of the
cut, so the null is not an artefact of different people in different
periods.

## More than two groups: one model per condition

Building two models by hand is manageable when there are two. When the
variable that separates the groups is already a column in the data, the
`group` argument builds one network per level in a single call, and it
scales past two, which the pairwise form does not.

The `esm_srl` study carries a `day_type` column marking weekday and
weekend reports, so momentary effort can be split by day type.

``` r

by_day <- ts_tna(
  subset(esm_srl, !is.na(effort)),
  value = "effort",
  id = "name",
  time = "occasion",
  group = "day_type",
  labels = c("low", "mid", "high")
)

by_day
#> Group Networks (2 groups, group_col: day_type)
#> 
#>   Group    Nodes  Edges  Weights
#>   weekday  3      9      [0.098, 0.631]
#>   weekend  3      9      [0.085, 0.633]
```

Two things happen here that would be tedious to arrange by hand. The
states are cut from the *pooled* series before the split, so both
networks rest on one set of thresholds and share a node set, the same
discipline the fixed `breaks` enforced earlier, applied automatically.
And the result is a Nestimate `netobject_group`, so the compatible
grouped verbs take it directly:
[`net_prune()`](https://saqr.me/Nestimate/reference/net_prune.html),
[`state_distribution()`](https://saqr.me/Nestimate/reference/state_distribution.html),
[`net_centrality()`](https://saqr.me/Nestimate/reference/net_centrality.html),
[`compare_model()`](https://saqr.me/Nestimate/reference/compare_model.html),
[`permutation()`](https://saqr.me/Nestimate/reference/permutation.html),
[`mosaic_plot()`](https://saqr.me/Nestimate/reference/mosaic_plot.html),
and their neighbours, while
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) with a `group`
argument draws one selected network. Not every verb applies: those
needing a precision matrix or a clustering attribute reject a transition
model.

[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) gives the
tidy view of how much data stands behind each network:

``` r

as.data.frame(by_day, what = "groups")
#>     group type sequences observations states edges
#> 1 weekday  tna       238         2033      3     9
#> 2 weekend  tna       233          783      3     9
```

The sequence counts repay a second look: the weekday network holds 2,033
observations in 238 sequences, the weekend network 783 in 233. That is
not padding. A run is cut at every change of student *and* at every
change of day type, and the two cuts are not the same. A change of
student separates measurements that were never consecutive; a change of
day type falls between two genuinely adjacent measurements, and the
transition across it is excluded from both networks because it belongs
to neither day type. A working week of roughly two reports a day
therefore forms one sequence of eight to ten observations, a weekend one
of three to four, and the Friday-to-Saturday transition is counted in
neither.

``` r

state_distribution(by_day)
#>     group state count proportion
#> 1 weekday   low   694  0.3413674
#> 2 weekday  high   676  0.3325135
#> 3 weekday   mid   663  0.3261190
#> 4 weekend   mid   268  0.3422733
#> 5 weekend  high   262  0.3346105
#> 6 weekend   low   253  0.3231162
```

The compositions are nearly identical: each state holds close to a third
of the reports on both day types, which pooled quantile cutting
guarantees for the whole panel, though not for each group taken alone.
Whatever distinguishes weekends, it is not how much high-effort time
they contain. The difference, if any, must live in the dynamics, and the
shared alphabet is what makes that question answerable:

``` r

day_comparison <- permutation(by_day, iter = 1000, seed = 2026)

summary(day_comparison)
#>                group from   to   weight_x   weight_y          diff  effect_size
#> 1 weekday vs weekend  low  low 0.61990212 0.63276836 -0.0128662409 -0.289921277
#> 2 weekday vs weekend  low  mid 0.28221860 0.28248588 -0.0002672786 -0.006925447
#> 3 weekday vs weekend  low high 0.09787928 0.08474576  0.0131335195  0.584487518
#> 4 weekday vs weekend  mid  low 0.29441624 0.26111111  0.0333051325  0.900900323
#> 5 weekday vs weekend  mid  mid 0.42978003 0.46666667 -0.0368866328 -0.878891544
#> 6 weekday vs weekend  mid high 0.27580372 0.27222222  0.0035815003  0.093352074
#> 7 weekday vs weekend high  low 0.11505922 0.12435233 -0.0092931099 -0.329460065
#> 8 weekday vs weekend high  mid 0.25380711 0.34196891 -0.0881618053 -2.327154276
#> 9 weekday vs weekend high high 0.63113367 0.53367876  0.0974549153  2.092734296
#>      p_value   sig
#> 1 0.77722278 FALSE
#> 2 0.99500500 FALSE
#> 3 0.56143856 FALSE
#> 4 0.38061938 FALSE
#> 5 0.37662338 FALSE
#> 6 0.92007992 FALSE
#> 7 0.77822178 FALSE
#> 8 0.02297702  TRUE
#> 9 0.04595405  TRUE
```

One difference holds up under the null: `high -> mid` runs at 0.254 on
weekdays and 0.342 on weekends (p = 0.023). Its mirror image, weekend
`high -> high` retention falling to 0.534 from 0.631, sits exactly on
the boundary (p = 0.046) and should be read as suggestive rather than
established. Read together, the pattern is coherent: on weekends a
high-effort spell is more likely to soften into the mid range and less
likely to hold. This is the reverse of the two-period result above:
there, composition was free to move and nothing moved; here, composition
is pinned by construction, so the dynamics carry the difference.

A mosaic plot shows both transition structures at a glance, one panel
per day type, with the weekend `high -> mid` cell visibly the larger of
the pair:

``` r

mosaic_plot(by_day)
```

![Mosaic panels of the weekday and weekend transition networks, in which
tile area encodes transition probability, showing the weekend
high-to-mid cell visibly larger than its weekday
counterpart.](nestimate-workflow_files/figure-html/grouped-mosaic-1.png)

Because both networks share an alphabet, the pair can also be set side
by side and compared as matrices:

``` r

compare_model(by_day, i = "weekday", j = "weekend")
#> Network comparison
#> ==================
#> Summary metrics:
#>              category               metric   value
#>     Weight Deviations      Mean Abs. Diff. 0.03277
#>     Weight Deviations    Median Abs. Diff. 0.01313
#>     Weight Deviations            RMS Diff. 0.04735
#>     Weight Deviations       Max Abs. Diff. 0.09745
#>     Weight Deviations Rel. Mean Abs. Diff. 0.09832
#>     Weight Deviations             CV Ratio   1.059
#>          Correlations              Pearson  0.9657
#>          Correlations             Spearman  0.8333
#>          Correlations              Kendall  0.6667
#>          Correlations             Distance  0.9357
#>       Dissimilarities            Euclidean   0.142
#>       Dissimilarities            Manhattan   0.295
#>       Dissimilarities             Canberra  0.4608
#>       Dissimilarities          Bray-Curtis 0.04916
#>       Dissimilarities            Frobenius   0.116
#>          Similarities               Cosine  0.9922
#>          Similarities              Jaccard  0.9063
#>          Similarities                 Dice  0.9508
#>          Similarities              Overlap  0.9508
#>          Similarities                   RV  0.9744
#>  Pattern Similarities       Rank Agreement       1
#>  Pattern Similarities       Sign Agreement       1
#> 
#> Network metrics (x vs y):
#>                       metric       x      y
#>                   Node Count       3      3
#>                   Edge Count       9      9
#>              Network Density       1      1
#>                Mean Distance  0.2199 0.2278
#>            Mean Out-Strength       1      1
#>              SD Out-Strength 0.03206 0.1015
#>             Mean In-Strength       1      1
#>               SD In-Strength       0      0
#>              Mean Out-Degree       3      3
#>                SD Out-Degree       0      0
#>  Centralization (Out-Degree)       0      0
#>   Centralization (In-Degree)       0      0
#>                  Reciprocity       1      1
```

A Pearson correlation of 0.966 with a maximum absolute difference of
0.097, the `high -> high` retention gap, confirms the same reading: the
two networks are close, and where they part is in how long high effort
lasts.

Pruning applies group-wise and returns a grouped model, so the
collection can be carried through a workflow without being taken apart:

``` r

net_prune(by_day, method = "threshold", threshold = 0.3)
#> Group Networks (2 groups)
#> 
#>   Group    Nodes  Edges  Weights
#>   weekday  3      5      [0.098, 0.631]
#>   weekend  3      5      [0.085, 0.633]
```

One caution generalizes beyond this example. Every level of the grouping
column gets a network, however little data stands behind it, so read the
`sequences` and `observations` columns of the index before reading any
edges. A network estimated from a handful of transitions is a level of a
column, not a result.

## Plotting a model

Any single network has a
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) method, and a
grouped model draws one selected group at a time via its `group`
argument. Here is the pooled effort model: three states as nodes,
transition probabilities as directed edges, and self-loops for
persistence.

``` r

plot(effort_model)
```

![Transition network of daily effort states, with low, moderate, and
high nodes joined by directed edges whose thickness reflects transition
probability and self-loops that mark each state's
persistence.](nestimate-workflow_files/figure-html/plot-1.png)

The heavy self-loops confirm what the state distribution reported
numerically: each state mostly returns to itself, and movement between
states is the smaller part of the process. The plotting vignette covers
the plot types and their arguments in full.

## Where the line falls

Everything descriptive in this vignette, from centralities, state
distributions, entropy, and pruning to per-student splits and the
grouped models, runs on a model built from a single series. Everything
inferential, meaning the bootstrap, stability, the Markov-order test,
and the permutation test, requires many sequences, because resampling
and permutation both treat the sequence as the unit of analysis.

Three practices follow. Build with `id` from the start if you intend to
test anything, so the sequences exist when a test asks for them. Fix
your `breaks` before you compare anything, or pass `group` and let the
pooled discretization fix them for you, so a difference between networks
is a difference in behaviour and not in thresholds. And when a
comparison rests on few sequences, say so: the `sequences` column exists
to be read, and so does a stability coefficient that comes back at 0.40.
