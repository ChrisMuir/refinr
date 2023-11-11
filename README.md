refinr
======

[![Travis-CI Build Status](https://app.travis-ci.com/ChrisMuir/refinr)](https://app.travis-ci.com/ChrisMuir/refinr)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/ChrisMuir/refinr?branch=master&svg=true)](https://ci.appveyor.com/project/ChrisMuir/refinr)
[![Coverage Status](https://img.shields.io/codecov/c/github/ChrisMuir/refinr/master.svg)](https://app.codecov.io/gh/ChrisMuir/refinr)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/refinr)](https://cran.r-project.org/package=refinr)


refinr is designed to cluster and merge similar values within a character vector. It features two functions that are implementations of clustering algorithms from the open source software [OpenRefine](https://openrefine.org/). The cluster methods used are key collision and ngram fingerprint (more info on these [here](https://openrefine.org/docs/technical-reference/clustering-in-depth)).

In addition, there are a few add-on features included, to make the clustering/merging functions more useful. These include approximate string matching to allow for merging despite minor mispellings, the option to pass a dictionary vector to dictate edit values, and the option to pass a vector of strings to ignore during the clustering process.

Please [report](https://github.com/ChrisMuir/refinr/issues) issues, comments, or feature requests.

Installation
------------

Install from CRAN:

``` r
install.packages("refinr")
```

Or install the dev version from this repo:

``` r
# install.packages("devtools")
devtools::install_github("ChrisMuir/refinr")
```

Example Usage
-------------
```r
library(refinr)
```

```r
x <- c("Acme Pizza, Inc.", "ACME PIZZA COMPANY", "acme pizza LLC", "Acme Pizza, Inc.")
key_collision_merge(x)
#> [1] "Acme Pizza, Inc." "Acme Pizza, Inc." "Acme Pizza, Inc." "Acme Pizza, Inc."
```

A dictionary character vector can be passed to `key_collision_merge`, which will dictate merge values when a cluster has a match within the dict vector.
```r
x <- c("Acme Pizza, Inc.", "ACME PIZZA COMPANY", "acme pizza LLC", "Acme Pizza, Inc.")
key_collision_merge(x, dict = c("Nicks Pizza", "acme PIZZA inc"))
#> [1] "acme PIZZA inc" "acme PIZZA inc" "acme PIZZA inc" "acme PIZZA inc"
```

Function `n_gram_merge` can be used to merge similar values that contain slight spelling differences. The [stringdist](https://CRAN.R-project.org/package=stringdist) package is used for calculating edit distance between strings. `refinr` links to the stringdist C API to improve the speed of the functions.
```r
x <- c("Acmme Pizza, Inc.", "ACME PIZA COMPANY", "Acme Pizzazza LLC")
n_gram_merge(x, weight = c(d = 0.2, i = 0.2, s = 1, t = 1))
#> [1] "ACME PIZA COMPANY" "ACME PIZA COMPANY" "ACME PIZA COMPANY"

# The performance of the approximate string matching can be ajusted using parameters 
# "weight" and/or "edit_threshold".
n_gram_merge(x, weight = c(d = 1, i = 1, s = 0.1, t = 0.1))
#> [1] "Acme Pizzazza LLC" "ACME PIZA COMPANY" "Acme Pizzazza LLC"
```

Both `key_collision_merge` and `n_gram_merge` have optional arg `ignore_strings`, which takes a character vector of strings to be ignored during the merging of values.
```r
x <- c("Bakersfield Highschool", "BAKERSFIELD high", "high school, bakersfield")
key_collision_merge(x, ignore_strings = c("high", "school", "highschool"))
#> [1] "BAKERSFIELD high" "BAKERSFIELD high" "BAKERSFIELD high"
```

The clustering is designed to be insensitive to common business name suffixes, i.e. "inc", "llc", "co", etc. This feature can be turned on/off using function parameter `bus_suffix`.

Workflow for checking the results of the refinr processes
---------------------------------------------------------

```r
library(dplyr)
library(knitr)

x <- c(
  "Clemsson University", 
  "university-of-clemson", 
  "CLEMSON", 
  "Clem son, U.", 
  "college, clemson u", 
  "M.I.T.", 
  "Technology, Massachusetts' Institute of", 
  "Massachusetts Inst of Technology", 
  "UNIVERSITY:  mit"
)

ignores <- c("university", "college", "u", "of", "institute", "inst")

x_refin <- x %>% 
  refinr::key_collision_merge(ignore_strings = ignores) %>% 
  refinr::n_gram_merge(ignore_strings = ignores)

# Create df for comparing the original values to the edited values.
# This is especially useful for larger input vectors.
inspect_results <- data_frame(original_values = x, edited_values = x_refin) %>% 
  mutate(equal = original_values == edited_values)

# Display only the values that were edited by refinr.
knitr::kable(
  inspect_results[!inspect_results$equal, c("original_values", "edited_values")]
)
#> |original_values                         |edited_values                    |
#> |:---------------------------------------|:--------------------------------|
#> |Clemsson University                     |CLEMSON                          |
#> |university-of-clemson                   |CLEMSON                          |
#> |Clem son, U.                            |CLEMSON                          |
#> |college, clemson u                      |CLEMSON                          |
#> |Technology, Massachusetts' Institute of |Massachusetts Inst of Technology |
#> |UNIVERSITY:  mit                        |M.I.T.                           |
```

Notes
-----

- This package is NOT meant to replace OpenRefine for every use case. For situations in which merging accuracy is the most important consideration, OpenRefine is preferable. Since the merging steps in refinr are automated, there will usually be more false positive merges, versus manually selecting clusters to merge in OpenRefine.
- The advantages this package has over OpenRefine: 
  * Operations are fully automated.
  * Facilitates a more reproducible workflow.
  * Faster when working with large input data (character vectors of length 500000+).
