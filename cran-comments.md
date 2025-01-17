## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Resubmission
This is a resubmission of [ComBatFamQC] (version 1.0.1) in response to CRAN comments.

### Issues Addressed:
1. Updated the package description to enclose the package name in single quotes: 'ComBatFamQC'.
2. Added references in the description field to describe the methods, following CRAN guidelines for formatting (e.g., authors (year) <doi:...>).
3. Replaced all instances of `print()` with `message()` in `R/visual_prep.R` and `R/Help_Func.R` to align with CRAN's recommended practices for console output.
4. Ensured the use of `tempfile()` for creating temporary directories in examples, vignettes, and tests to comply with CRAN's file handling policies.

Thank you so much for reviewing the package again.