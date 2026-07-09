# concept

`concept` is an R package for estimating adaptive concept change in categorization tasks using a boundary-anchored analysis framework.

The package was developed to quantify changes in categorization behavior under changing statistical environments, such as prevalence-induced concept change (PICC), while focusing specifically on changes near category boundaries.

The implementation is centered around `ecc()`, an alias for `estimate_concept_change()`.

## Installation

Install the development version from GitHub:

```r
install.packages("remotes")

remotes::install_github("rebecca-albrecht/concept")
```

or

```r
pak::pak("rebecca-albrecht/concept")
```

## Main Features

- Boundary-anchored estimation of concept change
- Participant-level effect estimates
- Flexible formula interface
- Optional beta-binomial bootstrap confidence intervals
- ROI-based analysis around decision boundaries
- Group-level visualization utilities

## Input Requirements

The package expects trial-level categorization data with:

- a binary response variable coded `0`/`1`
- a one-dimensional ordered stimulus-intensity variable
- a condition variable containing a baseline and a treatment condition
- one or more grouping variables, typically participant identifiers and experimental manipulations

For PICC-style applications, the response value `1` should code the target category whose concept is expected to extend, usually the category whose prevalence decreases in the treatment condition. With this coding, positive `effect_mean` values indicate an extension of the target category in the ambiguity regions relative to the prototypical regions.

## Example Data

The package includes a simulated example dataset:

```r
head(concept_data)
```

The dataset contains:

- participant identifiers
- experimental manipulation
- baseline/treatment conditions
- stimulus intensity values
- binary categorization responses

## Basic Example

```r
library(concept)

formula <- responsenum ~ x | condition | participant + manipulation

results <- ecc(
  formula,
  data = concept_data,
  roi_coverage_percent = 6,
  baseline_label = "baseline",
  treatment_label = "treatment",
  bootstrapping = FALSE
)

plot_effect_group(
  data = results,
  measure = "effect_mean",
  group1 = "manipulation"
)
```

To save intermediate ROI tables and estimates for inspection, pass an output directory:

```r
results <- ecc(
  formula,
  data = concept_data,
  bootstrapping = FALSE,
  dir = file.path(getwd(), "results")
)
```

## Formula Interface

The package uses a three-part formula syntax:

```r
response ~ intensity | condition | grouping
```

Example:

```r
responsenum ~ x | condition | participant + manipulation
```

where:

- `responsenum` is the binary response variable
- `x` is the ordered stimulus intensity dimension
- `condition` defines baseline vs. treatment conditions
- `participant` and `manipulation` define grouping variables

## Bootstrap Confidence Intervals

By default, `ecc()` returns point estimates only. Set `bootstrapping = TRUE` to compute participant-level beta-binomial bootstrap intervals:

```r
results_boot <- ecc(
  formula,
  data = concept_data,
  baseline_label = "baseline",
  treatment_label = "treatment",
  bootstrapping = TRUE,
  n_boot = 1000
)
```

When bootstrapping is enabled, the output includes `boot_ci_lower`, `boot_median`, `boot_ci_upper`, and `boot_mean`.

Bootstrap draws use R's current random-number-generator state. Call
`set.seed()` before `ecc()` when reproducible intervals are required.

The plotting helper `plot_effect_group()` summarizes participant-level estimates by group. Its error bars are group-level confidence intervals based on the standard error and Student's t distribution; they are not the participant-level bootstrap intervals.

## Estimand and ROI Width

`roi_coverage_percent` determines the number of unique observed stimulus
intensity levels in each ROI. It is a percentage of the number of observed
levels, not a percentage of the numeric distance between the smallest and
largest stimulus values.

The effect first averages the condition difference across stimulus-intensity
levels within each ROI and then combines the four ROI means using the
predefined contrast. Consequently, intensity levels receive equal weight
within an ROI even when they contain different numbers of trials.

## Validation and Reproducibility

Empirical validation, coverage analyses, power analyses, and reproduction
instructions are available in the
[OSF project "Measuring concept change"](https://osf.io/wzche/overview).

## Output

`ecc()` returns one row per grouping combination. Common output columns are:

- `effect_mean`: participant-level concept-change estimate
- `db`: estimated baseline decision boundary
- `beta0`, `beta1`: logistic boundary-model coefficients
- `n_x_baseline`, `n_x_treatment`: observations contributing to the selected ROIs
- `status`: estimation status (`ok`, `flagged`, or `error`)
- `flags`: diagnostic messages
- `boot_ci_lower`, `boot_ci_upper`: participant-level bootstrap interval bounds, returned only when bootstrapping is enabled

## Current Status

The package is under active development, but the exported user-facing interface is intended to remain stable for reproducible analyses. Breaking changes should be documented explicitly.

## License

GPL-3
