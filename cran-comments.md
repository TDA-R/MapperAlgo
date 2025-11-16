## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Submission
Fixes an important bug in the construction of the cover in MapperAlgo file, which extension and stride were not correctly calculated.
This is important as is will give the wrong cover and thus wrong clustering results.
