# concept

`concept` is an R package for estimating adaptive concept change in categorization tasks using a boundary-anchored analysis framework.

The package was developed to quantify changes in categorization behavior under changing statistical environments, such as prevalence-induced concept change (PICC), while focusing specifically on changes near category boundaries.

## Installation

Install the development version from GitHub:

```r
install.packages("remotes")

remotes::install_github("rebecca-albrecht/concept")
```

## Main Features

- Boundary-anchored estimation of concept change
- Participant-level effect estimates
- Flexible formula interface
- Optional beta-binomial bootstrap confidence intervals
- ROI-based analysis around decision boundaries
- Group-level visualization utilities

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
  bootstrapping = FALSE,
  dir = file.path(getwd(), "results")
)

plot_effect_group(
  data = results,
  measure = "effect_mean",
  group1 = "manipulation"
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

## Current Status

The package is currently under active development. Function names and interfaces may still change.

## License

GPL-3
