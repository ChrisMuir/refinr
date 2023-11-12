refinr 0.3.3
============

* Docs patch, at the request of CRAN ([#15](https://github.com/ChrisMuir/refinr/issues/15))
* Remove requirement on C++11. For more info, see [here](https://www.tidyverse.org/blog/2023/03/cran-checks-compiled-code/#note-regarding-systemrequirements-c11).

refinr 0.3.2
============

* Fix `failure: length > 1 in coercion to logical` issue in `n_gram_merge()`, in commit [4336ee6](https://github.com/ChrisMuir/refinr/commit/4336ee617a075a0b8cd0af1092ef299c14b49f25).

refinr 0.3.1
============

## IMPROVEMENTS

* Package is now linking to the `stringdist` C API, and calling C functions in place of using `stringdist::stringdistmatrix()`. This change results in speed improvements in function `n_gram_merge()`, and requires that `stringdist` v0.9.5.1 or greater be installed.

refinr 0.3.0
============

## PKG API CHANGES

* In function `n_gram_merge()`, renamed arg `edit_dist_weights` to `weight`. The only purpose of this arg is to be passed along to function `stringdistmatrix` from the [stringdist](https://CRAN.R-project.org/package=stringdist) package (which uses the name `weight`, so this change is simply to match that).

## BUG FIXES

* Fixed issue in which input strings that contained accent marks were not being properly handled/clustered ([#9](https://github.com/ChrisMuir/refinr/issues/9)). The fix involved adding [stringi](https://CRAN.R-project.org/package=stringi) to `Imports` and using `stringi::stri_trans_general()`.

* Fixed issue in `n_gram_merge()` in which incorrect values were being return when input arg `ignore_strings` was not `NULL`, and arg `bus_suffix = FALSE` ([#7](https://github.com/ChrisMuir/refinr/issues/7)).

* Fixed issue in which input strings that contained punctuation that was NOT surrounded by spaces was returning incorrect values ([#6](https://github.com/ChrisMuir/refinr/issues/6)).

* Fixed issue in which the edit value assigned to a cluster was sometimes not the most frequent string in that cluster ([#5](https://github.com/ChrisMuir/refinr/issues/5)).

## NEW FEATURES

* Rewrote some of the cpp functions to incorporate `std::unordered_map()`, resulting in a substantial speed improvement when passing large character vectors (length 100,000+) to either of the exported functions ([#8](https://github.com/ChrisMuir/refinr/issues/8)).

refinr 0.2.0 (2018-01-10)
=========================

* released on CRAN
