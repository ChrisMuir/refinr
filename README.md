refinr
========

R package implementation of two algorithms from the open source software [OpenRefine](http://openrefine.org/). These functions take a character vector as input, identify and cluster similar values, and then merge clusters together so their values become identical. The cluster methods used are key collision and ngram fingerprint (more info on these [here](https://github.com/OpenRefine/OpenRefine/wiki/Clustering-In-Depth)). In addition, the function `n_gram_merge` uses approximate string matching applied to the ngram fingerprints to form clusters.

This package is built using [stringdist](https://cran.r-project.org/web/packages/stringdist/index.html) for approximate string matching, [ngram](https://cran.r-project.org/web/packages/ngram/index.html) for string tokenization and [Rcpp](https://cran.r-project.org/web/packages/Rcpp/index.html) to allow for a handful of functions written in C++ for faster performance.

The clustering is also designed to be insensitive to common business name suffixes (i.e. inc, llc, co, etc.).

Installation
------------

``` r
# install.packages("devtools")
devtools::install_github("ChrisMuir/refinr")
```

Software Requirements
---------------------
Installing this package directly from GitHub requires a C++ compiler. See [here](https://support.rstudio.com/hc/en-us/articles/200486498-Package-Development-Prerequisites) for an explaination of how to meet this requirement for Windows, Mac and Linux.

Example Usage
-------------

```r
x <- c("Acme Pizza, Inc.", "ACME PIZZA COMPANY", "acme pizza LLC")
```
```r
key_collision_merge(x)
[1] "ACME PIZZA COMPANY" "ACME PIZZA COMPANY" "ACME PIZZA COMPANY"
```

`n_gram_merge` can be used to merge similar values that contain slight spelling differences.
```r
x <- c("Acmme Pizza, Inc.", "ACME PIZA COMPANY", "Acme Pizzazza LLC")
```
```r
n_gram_merge(x, 
             numgram = 2, 
             edit_threshold = 1, 
             edit_dist_weights = c(d = 0.2, i = 0.2, s = 1, t = 1), 
             bus_suffix = TRUE)
[1] "ACME PIZA COMPANY" "ACME PIZA COMPANY" "ACME PIZA COMPANY"
```

The performance of the approximate string matching can be ajusted using parameter `edit_dist_weights`.
```r
n_gram_merge(x, 
             numgram = 2, 
             edit_threshold = 1, 
             edit_dist_weights = c(d = 1, i = 0.2, s = 1, t = 1), 
             bus_suffix = TRUE)
[1] "ACME PIZA COMPANY" "ACME PIZA COMPANY" "Acme Pizzazza LLC"
```
```r
n_gram_merge(x, 
             numgram = 2, 
             edit_threshold = 1, 
             edit_dist_weights = c(d = 1, i = 1, s = 0.1, t = 0.1), 
             bus_suffix = TRUE)
[1] "Acme Pizzazza LLC" "ACME PIZA COMPANY" "Acme Pizzazza LLC"
```

For both `key_collision_merge` and `n_gram_merge`, the merge value for a given cluster will always be the value that appears the greatest number of times in the original vector.
```r
x <- c("Acme Pizza, Inc.", "Acme Pizza, Inc.", "ACME PIZZA COMPANY", "acme pizza LLC")
```
```r
key_collision_merge(x)
[1] "Acme Pizza, Inc." "Acme Pizza, Inc." "Acme Pizza, Inc." "Acme Pizza, Inc."
```

Notes
-----

- This package is NOT meant to replace OpenRefine for every use case. For situations in which merging accuracy is the most important consideration, OpenRefine is preferable. Since the merging steps in refinr are automated, there will usually be more false positive merges, versus manually selecting clusters to merge in OpenRefine.
- The advantages this package has over OpenRefine: 
  * Operations are fully automated.
  * Facilitates a more repoducable workflow.
  * Seems to handle larger datasets better (1000000 - 5000000 observations).
