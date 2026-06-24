<!-- badges: start -->

<!-- For more info: https://usethis.r-lib.org/reference/badges.html -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

# Alaska Herbicide Efficacy

## Overview

The U.S. Fish and Wildlife Service conducts annual surveys to identify the presence of terrestrial non-native plant species within southern Alaska. Herbicides are applied to identified non-native plants in an effort to control the further spread of these species and subsequent surveys are conducted to evaluate the efficacy of herbicide treatment. Scripts included in this repository allow users to run a power analysis to determine the sample size requirement and create randomly selected sample sites within designated polygons where non-native plants have been treated with herbecide.

## Installation

Scripts rely on the following packages:
- [sf](https://cran.r-project.org/web/packages/sf/index.html)
- [dplyr](https://cran.r-project.org/web/packages/dplyr/index.html)
- [purrr](https://cran.r-project.org/web/packages/purrr/index.html)
- [tidyr](https://cran.r-project.org/web/packages/tidyr/index.html)


## Usage

Scripts contain functions that are designed for general use on point sampling polygons. Note that default coordinate reference systems assume NAd83 Alaska Albers (EPSG 3338) as input and WGS84 (EPSG 4326) as the output. 

## Getting help

Contact the [project maintainer](emailto:jonah_withers@fws.gov) for help with this repository. 

## Contribute

Contact the project maintainer for information about contributing to this repository. Submit a [GitHub Issue](https://github.com/USFWS/ak-herbicide-efficacy/issues) to report a bug or request a feature or enhancement.

-----

This work is
licensed under a [Creative Commons Zero v1.0 Universal](https://choosealicense.com/licenses/cc0-1.0/).
