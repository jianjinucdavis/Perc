## Resubmission
This is new submission. 

* The previous Perc was archived on CRAN because of an uncorrected failed check. In this version I have:

* Corrected the codes that were responsible for the failed check- Tests for `valueConverter` was updated so that it is consistent with the most recent version of testthat.

* Version number was increased to "0.1.2".

* `as.conflictmat` was updated to fix minor issues in raw data with warnings returned to users about the fix, instead of returning erros as in previous versions.

## Test environments
* local OS X install, R 3.2.3
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There were 2 NOTEs when checking package dependencies. 
* New submission

Package was archived on CRAN

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2016-05-02 as check problems were not
    corrected despite reminders.

* No repository set, so cyclic dependency check skipped


## Reverse dependencies
This is no package depend on Perc.