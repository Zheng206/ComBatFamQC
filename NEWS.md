# ComBatFamQC 1.0.6
* Added the systemfonts package as a suggested dependency and implemented a skip mechanism for both com_shiny() and age_shiny() if the package is not installed. This should resolve the testing issue on r-oldrel-macos-arm64.
* Updated the wording in the Shiny app layout and fixed minor typos.

# ComBatFamQC 1.0.5
* Revised unit tests for both com_shiny() and age_shiny() to use testthat and shiny, replacing shinytest2 for improved compatibility and maintainability.


# ComBatFamQC 1.0.4
* Set the R dependency to require at least version 4.1.0.
* Fixed CRAN build issues related to Quarto availability.
* Resolved portability issues flagged during CRAN checks.
* Modified the description of the age_list_gen() function.

# ComBatFamQC 1.0.2
* Resolved CRAN build issues by optimizing test execution time to comply with runtime limits.

# ComBatFamQC 1.0.1
* Addressed CRAN feedback: improved package description, added references, replaced `print()` with `message()`, ensured proper handling of temporary files, and optimized examples for faster execution.

# ComBatFamQC 1.0.0

* Initial CRAN submission.
