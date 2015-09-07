## Patch release: fixes \donttest{} example
I added the kernlab package to Suggests in the DESCRIPTION.
The package was required by a *donttest* example but not declared and therefore caused an ERROR when CRAN was running R CMD check --run-donttest

## Test environments
* ubuntu 15.04 64bit, (R 3.2.2)
* ubuntu 12.04 (on travis-ci), (oldrel, release, devel)
* win-builder (release, devel)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs. 

## Downstream dependencies
none 