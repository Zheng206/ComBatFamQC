## Resubmission of version 1.0.5

## Change in version 1.0.5
1. Revised unit tests for both com_shiny() and age_shiny() to use testthat and shiny, replacing shinytest2 for improved compatibility and maintainability.

## Previous Submissions

### Version 1.0.4
1. Set the R dependency to require at least version 4.1.0.
2. Fixed CRAN build issues related to Quarto availability.
3. Resolved portability issues flagged during CRAN checks.
4. Modified the description of the age_list_gen() function.

### Version 1.0.3
1. Withdrawn due to unresolved issues flagged during M1 Mac testing.

### Version 1.0.2
1. Updated the package description to enclose the package name in single quotes: 'ComBatFamQC'.
2. Added references in the description field to describe the methods, formatted per CRAN guidelines (e.g., authors (year) <doi:...>).
3. Replaced all instances of `print()` with `message()` in `R/visual_prep.R` and `R/Help_Func.R` to align with CRAN's recommended practices for console output.
4. Ensured the use of `tempfile()` for creating temporary directories in examples, vignettes, and tests to comply with CRAN's file handling policies.
5. Cleaned up all temporary files using `unlink()`.
6. Optimized examples to ensure they run in less than 5 seconds.

### Version 1.0.1
1. Did not pass the pre-check on CRAN.


## R CMD check results

0 errors | 0 warnings | 0 notes


Thank you so much for reviewing the package again.