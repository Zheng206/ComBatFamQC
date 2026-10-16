# ComBatFamQC (development version)

* Single-feature harmonization now disables EB in the wrapper and records
  `combat.object$eb = FALSE`, including after feature filtering and during
  reference harmonization or prediction. Prediction preserves the saved EB setting.

* CLI ComBatLS checks variables from both formulas against `--covariates`
  before reference loading or fitting and reports missing names together.

* Made `robust.LS` explicit in the harmonization wrapper and ComBatLS adapter,
  and added early location/scale formula structure checks for direct R calls.
* Added separate-reference ComBatLS regression tests covering formula scope,
  covariate preprocessing, reference inclusion, and upstream numerical agreement.
* Avoided the model-name evaluation for ComBatLS and clarified CLI engine help.

* ComBatLS now requires an explicit location formula in the R API and CLI,
  retains the optional constant-scale formula default, and rejects legacy smooth,
  interaction, and random arguments. Other families retain automatic formulas.

* CLI ComBatLS harmonization now selects GAMLSS by family and ignores `--model`,
  so nonlinear formulas require no fitting-engine selector.

* Added CLI `--formula` and `--sigma.formula` options for ComBatLS, including
  validation, the constant-scale default, and formula-based interactions.
* Fixed CLI forwarding of `--eb` and `--robust.LS` to harmonization fits, and
  declared its smaller set of optional dependencies instead of loading tidyverse.

* ComBatLS now accepts explicit GAMLSS location and scale formulas, defaults to
  `sigma.formula = ~ 1`, and rejects mgcv smoother syntax instead of translating
  `s()` to `ps()`. Automatic mean-to-scale formula inheritance was removed.

* Fixed installed-package ComBatLS fits failing to resolve `NO()` by passing
  explicitly namespaced GAMLSS functions to the upstream core implementation.
* Replaced deprecated `.data` column selection in diagnostics, tables, and age
  exports to remove tidyselect deprecation warnings.

* Added optional ComBatLS through a thin adapter to ComBatFamily, using
  `combat_harm(family = "combatls")` and the existing result structure.
* Added `check_dependencies()` to report missing, outdated, or unloadable
  dependencies and optionally install them from CRAN or upstream GitHub.
* Preserved single-feature inputs during harmonization preparation and handled
  matrix dimensions when selecting harmonized reference output.
* Made internal ComBat prediction dispatch explicit when ComBatFamily is loaded.

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
