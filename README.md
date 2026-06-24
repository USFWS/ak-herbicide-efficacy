---
editor_options: 
  markdown: 
    wrap: 72
---

# Alaska Herbicide Efficacy

Tools for generating random spatial samples for invasive plant
monitoring in Alaska

<https://img.shields.io/badge/lifecycle-experimental-orange.svg>](<https://lifecycle.r-lib.org/articles/stages.html#experimental>)
<https://img.shields.io/github/issues/USFWS/ak-herbicide-efficacy.svg>](<https://github.com/USFWS/ak-herbicide-efficacy/issues>)
<https://img.shields.io/github/commit-activity/m/USFWS/ak-herbicide-efficacy.svg>](<https://github.com/USFWS/ak-herbicide-efficacy>)

## Overview

The U.S. Fish and Wildlife Service conducts annual monitoring of
terrestrial non‑native plant species within southern Alaska. Herbicide
treatments are applied to known infestations, and follow‑up surveys
evaluate treatment effectiveness by sampling locations within treated
polygons. This package provides a streamlined workflow to:

-   read non‑native plant treatment polygons
-   generate simple random samples
-   transform spatial coordinates
-   organize site IDs and metadata
-   perform batch sampling across species and years

The functions included here formalize and package workflows that
previously existed only as standalone scripts, as described in the
earlier repository README (README).

## Installation

The package is hosted on GitHub and can be installed using either
remotes or devtools.

Install with remotes (recommended)

``` r
install.packages("remotes")
remotes::install_github("USFWS/ak-herbicide-efficacy", ref = "Package")
```

Install with devtools

``` r
install.packages("devtools")
devtools::install_github("USFWS/ak-herbicide-efficacy", ref = "Package")
```

Local installation (ZIP download)

1.  Navigate to the Package branch:
    <https://github.com/USFWS/ak-herbicide-efficacy/tree/Package>
2.  Select Code → Download ZIP
3.  Unzip the folder
4.  Install locally:

``` r
devtools::install_local("path/to/ak-herbicide-efficacy")
```

## Package Functions

The package currently implements:

-   generate_srs() Generate a simple random sample of points within a
    polygon layer.

-   multi_srs() Batch sampling for multiple species or polygons using a
    list‑based interface.

These functions are designed to support general sampling workflows
applied by Alaska Region invasive species programs and match the usage
patterns documented in your existing project materials (e.g., the
earlier script‑based workflows referenced in README).

## Dependencies

Scripts and functions rely on: -
[sf](https://cran.r-project.org/web/packages/sf/index.html) -
[dplyr](https://cran.r-project.org/web/packages/dplyr/index.html) -
[purrr](https://cran.r-project.org/web/packages/purrr/index.html) -
[tidyr](https://cran.r-project.org/web/packages/tidyr/index.html) -
[tibble](https://cran.r-project.org/web/packages/tibble/index.html)

These will be installed automatically when using install_github().

## Example Usage

Scripts contain functions that are designed for general use on point
sampling polygons. Note that default coordinate reference systems assume
NAD83 Alaska Albers (EPSG 3338) as input and WGS84 (EPSG 4326) as the
output.

### Generate a single species sample

``` r
library(akHerbicideEfficacy)

ugoh_samples <- generate_srs(
  path = "data",
  layer = "UgashikOrangeHawkweedJul2021",
  n = 120,
  site_code = "UGOH",
  year = 2025
)
```

### Generate sampling with mutli_srs()

``` r
species_list <- list(
  UGOH = list(path="data",
              layer="UgashikOrangeHawkweedJul2021",
              n=120, site_code="UGOH", year=2025),
  CBTH = list(path="data",
              layer="2022ThistleCombinedPolygon",
              n=120, site_code="CBTH", year=2025)
)

all_samples <- multi_srs(species_list)
```

## Getting help

To request assistance or report issues: - Open a GitHub Issue:
<https://github.com/USFWS/ak-herbicide-efficacy/issues> - Contact the
[project maintainer](emailto:jonah_withers@fws.gov) for help with this
repository.

## Contribute

Contact the project maintainer for information about contributing to
this repository. Submit a [GitHub
Issue](https://github.com/USFWS/ak-herbicide-efficacy/issues) to report
a bug or request a feature or enhancement.

------------------------------------------------------------------------

This work is licensed under a [Creative Commons Zero v1.0
Universal](https://choosealicense.com/licenses/cc0-1.0/).
