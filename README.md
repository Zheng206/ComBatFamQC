# ComBatFamQC

 <!-- badges: start -->
  [![R-CMD-check](https://github.com/Zheng206/ComBatFamQC/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Zheng206/ComBatFamQC/actions/workflows/R-CMD-check.yaml) [![Codecov test coverage](https://codecov.io/gh/Zheng206/ComBatFamQC/graph/badge.svg)](https://app.codecov.io/gh/Zheng206/ComBatFamQC)
  <!-- badges: end --> 



The **ComBatFamQC** package is a powerful tool designed to streamline <span style="color:purple;">interactive batch effect diagnostics</span>, <span style="color:green;">harmonization</span>, and <span style="color:blue;">post-harmonization downstream analysis</span>. This package is specifically tailored to provide both <u>interactive qualitative visualization</u> and <u>statistical testing for batch effects diagnostics</u>, as well as to offer various easily-used <u>built-in harmonization techniques</u> to facilitate a better harmonization process.

Additionally, the package provides <u>life span age trends of brain structures</u> and <u>residual datasets</u>, eliminating specific covariates' effects to better conduct post-harmonization downstream analysis. For the final delivery, the package offers interactive visualizations through R Shiny for batch effect diagnostics and age trend visualization. For users who wish to save a copy of the diagnostic report, the package also includes an option to generate a Quarto report (if Quarto is installed). Furthermore, it integrates the harmonization process and can provide a harmonized dataset, a fitted ComBat model, a residual dataset, a fitted regression model, and more.

To make the harmonization process more accessible to users from diverse backgrounds, two unified command-line interfaces have been developed for different stages of the processing pipeline (located in the **inst** folder):

-   **CombatQC_CLI.R**: Batch Effect Diagnostics & Harmonization Stage
    -  Offers interactive diagnostics for batch effects
    -  Export the batch effect diagnosis report
    -  Performs data harmonization to adjust for batch effects  
-   **post_CLI.R**: Post-Harmonization Stage
    -   Visualizes age trends across brain structures over the lifespan
    -   Generates a residual dataset with unwanted covariate effects removed

Note: Detailed information and tutorials can be found: https://zheng206.github.io/ComBatQC-Web/

## Diagram
![ComBatFamQC Diagram](/inst/figure/ComBatFamQC.png)

## Package Features

The ComBatFamQC package offers the following five key functionalities:

1. <u>**Interactive Batch Effect Diagnostics & Harmonization**</u>

-   **Batch Effect Diagnostics**: ComBatFamQC provides two types of batch effect diagnostics methods for both individual batch effects and global batch effects: 1) *Qualitative Visualization* and 2) *Statistical Testing*. It simplifies the process of performing statistical analyses to detect potential batch effects and provides all relevant statistical test results for batch effect visualization and evaluation.

-   **Harmonization**: ComBatFamQC also provides four types of commonly used harmonization techniques, integrated through the [ComBatFamily](https://github.com/andy1764/ComBatFamily) package developed by **Dr. Andrew Chen**, for users to consider. The four harmonization techniques include: 
    -   Original ComBat (Johnson et al., 2007)
    -   Longitudinal ComBat (Beer et al., 2020)
    -   ComBat-GAM (Pomponio et al., 2020)
    -   CovBat (Chen et al., 2021)

-   **Interactive Visualization through R Shiny**: The ComBatFamQC package comes with an interactive visualization tool built on R Shiny, providing an intuitive user interface to explore and evaluate batch effects, as well as conduct interactive harmonization if needed. The output is organized into multiple tabs, which includes:

    -   **Data Overview**: Complete data overview and exploratory analysis
    -   **Summary**: Sample Size and Covariate Distribution
    -   **Residual Plot**: Additive and Multiplicative Batch Effect
    -   **Diagnosis of Global Batch Effect**: PCA, T-SNE and MDMR
    -   **Diagnosis of Individual Batch Effect**:
        -   *Statistical Tests for Additive Batch Effect*: Kenward-Roger (liner mix model), ANOVA, Kruskal-Wallis
        -   *Statistical Tests for Multiplicative Batch Effect*: Fligner-Killeen, Levene's Test, Bartlett's Test
    -   **Harmonization** Interactive Harmonization if needed

2. <u>**Post-Harmonization Downstream Analysis**</u>

-   **Age Trajectory** \
    Generate age trend of each brain structure (ROI), adjusting sex and ICV. Customized centiles are enabled as well.
    -  **Age Trend Plots**
    -  **Age Trend Table** 

-   **Residual Generation** \
    Generate residual data set, removing specific covariates' effetcs.


## Installation

```{r}
if (!require("devtools", quietly = TRUE)) {
    install.packages("devtools")   
}

library(devtools)

devtools::install_github("Zheng206/ComBatFamQC", build_vignettes = TRUE)

```

## Tutorial

```{r}
vignette("ComBatQC")
vignette("Post-Harmonization")
```


