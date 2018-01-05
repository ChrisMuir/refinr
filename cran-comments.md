# Resubmission

## Reviewer comments

2018-01-05 Uwe Ligges

Thanks, we see:

Package has a VignetteBuilder field but no prebuilt vignette index.

Please fix and resubmit.

## Resubmission Fixes

* I have fixed the vignette index issue. The package has been re-checked on CRAN win-builder, and the "VignetteBuilder" message no longer appears.

# Initial Submission

## Test environments
* local Windows 10 install, R 3.4.3
* local OS X install, R 3.4.2
* ubuntu 14.04.5 (on travis-ci), R 3.4.2
* Windows Server 2012 R2 x64 (on appveyor), R 3.4.3 Patched
* CRAN win-builder, R devel

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

* Possibly mis-spelled words in DESCRIPTION:
  ngram (5:229, 5:353)
  
"ngram" is not in the dictionary, but it's not a misspelling

## Reverse dependencies

This is a new release, so there are no reverse dependencies.
