# ComBatFamQC (development version)

* Added optional ComBatLS harmonization in R, the CLI, and Shiny, with explicit
  GAMLSS location and scale formulas, robust estimation, and reference-batch
  support. A location formula is required; the scale formula defaults to `~ 1`.

* Added ComBatLS model checks with biological scale-covariate screening,
  BH-adjusted summaries, SD ratios, centile comparisons, and residual diagnostics
  through `combatls_model_check()`, `combatls_check_centiles()`, and
  `combatls_check_plot()`.

* Improved Shiny layouts and model-check displays, added interactive result
  filters, and accelerated smooth-model checks and centile predictions by
  reusing matching fits and batching predictions.

* Added significant-only checkboxes to additive and multiplicative feature-level
  batch-effect tables, using Bonferroni-adjusted p-values below 0.05.

* Added `check_dependencies()` to check required dependencies and optionally
  install missing or outdated packages.

* Fixed single-feature harmonization by disabling EB, improved reference
  harmonization and prediction handling, and corrected model saving, formula
  validation, and deprecated column-selection warnings.

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
