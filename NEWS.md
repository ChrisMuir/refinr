refinr 0.2.0.9000
=================

## PKG API CHANGES

* In function `n_gram_merge()`, renamed arg `edit_dist_weights` to `weight`. The only purpose of this arg is to be passed along to function `stringdistmatrix` from the [stringdist](https://CRAN.R-project.org/package=stringdist) package (which uses the name `weight`, so this change is simply to match that).

## BUG FIXES

* Fixed issue in which the edit value assigned to a cluster was sometimes not the most frequent string in that cluster ([#5](https://github.com/ChrisMuir/refinr/issues/5)). 

refinr 0.2.0 (2018-01-10)
=========================

* released on CRAN
