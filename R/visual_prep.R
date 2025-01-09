#' Batch Effect Diagnostic Visualization Preparation
#'
#' Prepare relevant datasets and statistical test results for batch/site effect diagnostic visualization.
#'
#' @param type The name of a regression model to be used in batch effect diagnostics stage: "lmer", "lm", "gam".
#' @param features The name of the features to be evaluated.
#' @param batch The name of the batch variable.
#' @param covariates Name of covariates supplied to `model`.
#' @param interaction Expression of interaction terms supplied to `model` (eg: "age,diagnosis").
#' @param random Variable name of a random effect in linear mixed effect model.
#' @param smooth Variable name that requires a smooth function.
#' @param smooth_int_type Indicates the type of interaction in `gam` models. By default, smooth_int_type is set to be "linear", representing linear interaction terms.
#' "categorical-continuous", "factor-smooth" both represent categorical-continuous interactions ("factor-smooth" includes categorical variable as part of the smooth),
#' "tensor" represents interactions with different scales, and "smooth-smooth" represents interaction between smoothed variables.
#' @param df Dataset to be evaluated.
#' @param cores number of cores used for parallel computing.
#' @param mdmr A boolean variable indicating whether to run the MDMR test. By default, it is set to TRUE.
#'
#' @return `visual_prep` returns a list containing the following components:
#' \item{residual_add_df}{Residuals that might contain additive and multiplicative joint batch effects}
#' \item{residual_ml_df}{Residuals that might contain multiplicative batch effect}
#' \item{pr.feature}{PCA results}
#' \item{pca_summary}{A dataframe containing the variance explained by Principal Components (PCs)}
#' \item{pca_df}{A dataframe contains features in the form of PCs}
#' \item{tsne_df}{A dataframe prepared for T-SNE plots}
#' \item{kr_test_df}{A dataframe contains Kenward-Roger(KR) test results}
#' \item{fk_test_df}{A dataframe contains Fligner-Killeen(FK) test results}
#' \item{mdmr.summary}{A dataframe contains MDMR results}
#' \item{anova_test_df}{A dataframe contains ANOVA test results}
#' \item{kw_test_df}{A dataframe contains Kruskal-Wallis test results}
#' \item{lv_test_df}{A dataframe contains Levene's test results}
#' \item{bl_test_df}{A dataframe contains Bartlett's test results}
#' \item{red}{A parameter to highlight significant p-values in result table}
#' \item{info}{A list contains input information like batch, covariates, df etc}
#'
#' @import pbkrtest
#' @import parallel
#' @import Rtsne
#' @import MDMR
#' @importFrom broom tidy
#' @importFrom mgcv gam anova.gam
#' @importFrom gamlss gamlss
#' @importFrom lme4 lmer
#' @importFrom stats family lm median model.matrix prcomp predict qnorm update var anova as.formula coef dist fligner.test p.adjust resid na.omit bartlett.test kruskal.test
#' @importFrom car leveneTest
#'
#' @export
#'
#' @examples
#' visual_prep(type = "lm", features = colnames(adni)[43:53], batch = "manufac",
#' covariates = c("AGE", "SEX", "DIAGNOSIS"), df = head(adni, 500), cores = 2)
#'

visual_prep <- function(type = "lm", features, batch, covariates = NULL, interaction = NULL, random = NULL, smooth = NULL, smooth_int_type = NULL, df, cores = detectCores(), mdmr = TRUE){
  # Data Preparation
  info <- data_prep(stage = "harmonization", result = NULL, features = features, batch = batch, covariates = covariates, df = df, type = type, random = random, smooth = smooth, interaction = interaction, smooth_int_type = smooth_int_type, predict = FALSE, object = NULL)
  df <- info$df
  features <- info$features
  covariates <- info$covariates
  interaction <- info$interaction
  smooth <- info$smooth
  cov_shiny <- info$cov_shiny
  char_var <- info$char_var
  smooth_int_type <- info$smooth_int_type
  interaction_orig <- info$interaction_orig
  smooth_orig <- info$smooth_orig
  # Residual Plots
  vis_df <- df[colnames(df)[!colnames(df) %in% features]]
  residual_add_df <- mclapply(features, function(y){
    model <- model_gen(y = y, type = type, batch = batch, covariates = covariates, interaction = interaction, random = random, smooth = smooth, df = df)
    if(type == "lmer"){
      coef_list <- coef(model)
      intercept <- lapply(1:length(coef_list), function(i){
        b <- coef_list[[i]]
        b_fix <- unique(b[,-1])
        b_fix_ex <- b_fix[which(!grepl(paste0(batch,"*"), names(b_fix)))]
        b[[random[i]]] <- as.factor(rownames(b))
        sub_coef <- df[random[i]] %>% left_join(b[c(random[i], "(Intercept)")], by = c(random[i]))
        colnames(sub_coef) <- c(random[i], paste0(random[i], "_int"))
        return(sub_coef)
      }) %>% bind_cols() %>% dplyr::select(matches("_int$"))
      b <- coef(model)[[1]]
      b_fix <- unique(b[,-1])
      b_fix_ex <- b_fix[which(!grepl(paste0(batch,"*"), names(b_fix)))]
      y_hat <- model.matrix(model)[, which(!grepl(paste0(batch,"*|(Intercept)"), colnames(model.matrix(model))))] %*% t(b_fix_ex)
      for (i in 1:length(random)){
        y_hat <- y_hat + intercept[[i]]
      }
    }else{
      b <- coef(model)
      m_intercept <- b[[1]]
      b <- b[-1]
      b <- b[which(!grepl(paste0(batch,"*"), names(b)))]
      if(length(b) > 1){
        y_hat <- model.matrix(model)[, which(!grepl(paste0(batch,"*|(Intercept)"), colnames(model.matrix(model))))] %*% b + m_intercept}else if(length(b) == 1){
          y_hat <- model.matrix(model)[, which(!grepl(paste0(batch,"*|(Intercept)"), colnames(model.matrix(model))))] * b + m_intercept
        }else if(length(b) == 0){
          y_hat <- rep(m_intercept, nrow(df))
        }
    }
    residual <- data.frame(df[[y]] - y_hat)
    colnames(residual) <- y
    return(residual)
  }, mc.cores = cores) %>% bind_cols()
  residual_add_df <- cbind(vis_df, residual_add_df)

  residual_ml_df <- mclapply(features, function(y){
    model <- model_gen(type = type, y = y, batch = batch, covariates = covariates, interaction = interaction, random = random, smooth = smooth, df = df)
    residual <- data.frame(resid(model))
    colnames(residual) <- y
    return(residual)
  }) %>% bind_cols()
  residual_ml_df <- cbind(vis_df, residual_ml_df)

  # PCA Plots
  pr.feature <- prcomp(x = residual_add_df[features], scale = TRUE, center = TRUE)
  variance_explained <- pr.feature$sdev^2 / sum(pr.feature$sdev^2)
  pca_summary <- data.frame(Principal_Component = paste0("PC",seq_along(variance_explained)), Variance_Explained = variance_explained, Variance_Explained_Cum = cumsum(variance_explained))
  pca_df <- cbind(vis_df, pr.feature$x)

  # T-SNE Plots
  tsne_out <- Rtsne(as.matrix(residual_add_df[features]))
  tsne_df <- cbind(vis_df, tsne_out$Y)
  colnames(tsne_df) <- c(colnames(vis_df), "cor_1", "cor_2")

  # Statistical Test
  ## Kenward-Roger(KR) Test
  if(type == "lmer"){
    kr_test_df <- mclapply(features, function(y){
      lmm1 <- model_gen(type = type, y = y, batch = batch, covariates = covariates, interaction = interaction, random = random, smooth = smooth, df = df)
      lmm2 <- update(lmm1, as.formula(paste0(".~. -", batch)))
      kr.test <- KRmodcomp(lmm1, lmm2)
      kr_df <- kr.test %>% tidy() %>% filter(.data[["type"]] == "Ftest") %>% mutate(feature = y) %>% dplyr::select(all_of(c("feature", "stat", "ndf", "ddf", "p.value")))
      return(kr_df)
    }, mc.cores = cores) %>% bind_rows()
    kr_test_df$p.value <- p.adjust(kr_test_df$p.value, method = "bonferroni", n = length(kr_test_df$p.value))
    kr_test_df <- kr_test_df %>% arrange(.data[["p.value"]]) %>% mutate(sig = case_when(.data[["p.value"]] < 0.05 & .data[["p.value"]] >= 0.01 ~ "*",
                                                       .data[["p.value"]] < 0.01 & .data[["p.value"]] >= 0.001 ~ "**",
                                                       .data[["p.value"]] < 0.001 ~ "***",
                                                       .default = NA)) %>% mutate(p.value.raw = .data[["p.value"]])

    kr_test_df <- kr_test_df %>% mutate(stat = round(.data[["stat"]], 2), ddf = round(.data[["ddf"]], 2))
    kr_test_df$p.value <- sapply(kr_test_df$p.value, function(x){
      ifelse(x >= 0.001, sprintf("%.3f", round(x, 3)), "<0.001")
    }, USE.NAMES = FALSE)
    unique_kr <- unique(kr_test_df$p.value)[unique(kr_test_df$p.value) != "<0.001"][which(as.numeric(unique(kr_test_df$p.value)[unique(kr_test_df$p.value) != "<0.001"]) < 0.05)]
  }else{
    kr_test_df <- data.frame("feature" = NULL, "stat" = NULL, "ndf" = NULL, "ddf" = NULL, "p.value" = NULL, "sig" = NULL)
    unique_kr <- unique(kr_test_df$p.value)[unique(kr_test_df$p.value) != "<0.001"][which(as.numeric(unique(kr_test_df$p.value)[unique(kr_test_df$p.value) != "<0.001"]) < 0.05)]
  }

  ## Fligner-Killeen(FK) Test
  fk_test_df <- mclapply(features, function(y){
    lmm_multi <- model_gen(type = type, y = y, batch = batch, covariates = covariates, interaction = interaction, random = random, smooth = smooth, df = df)
    fit_residuals <- resid(lmm_multi)
    FKtest <- fligner.test(fit_residuals ~ df[[batch]])
    fk_df <- FKtest %>% tidy() %>% dplyr::select(.data[["p.value"]]) %>% mutate(feature = y)
    fk_df <- fk_df[c(2,1)]
    return(fk_df)
  }, mc.cores = cores) %>% bind_rows()
  fk_test_df$p.value <- p.adjust(fk_test_df$p.value, method = "bonferroni", n = length(fk_test_df$p.value))
  fk_test_df <- fk_test_df %>% arrange(.data[["p.value"]]) %>% mutate(sig = case_when(.data[["p.value"]] < 0.05 & .data[["p.value"]] >= 0.01 ~ "*",
                                                     .data[["p.value"]] < 0.01 & .data[["p.value"]] >= 0.001 ~ "**",
                                                     .data[["p.value"]] < 0.001 ~ "***",
                                                     .default = NA)) %>% mutate(p.value.raw = .data[["p.value"]])
  fk_test_df$p.value <- sapply(fk_test_df$p.value, function(x){
    ifelse(x >= 0.001, sprintf("%.3f", round(x, 3)), "<0.001")
  }, USE.NAMES = FALSE)
  unique_fk <- unique(fk_test_df$p.value)[unique(fk_test_df$p.value) != "<0.001"][which(as.numeric(unique(fk_test_df$p.value)[unique(fk_test_df$p.value) != "<0.001"]) < 0.05)]

  ## MDMR
  if(mdmr){
  D <- dist(scale(as.matrix(residual_add_df[features])))
  mdmr.res <- mdmr(X = as.matrix(residual_add_df[batch]), D = D)
  mdmr.summary <- summary(mdmr.res)
  colnames(mdmr.summary) <- c("Statistic", "Numer.DF", "Pseudo.R2", "p.value")
  mdmr.summary <- mdmr.summary %>% arrange(.data[["p.value"]]) %>% mutate(sig = case_when(.data[["p.value"]] < 0.05 & .data[["p.value"]] >= 0.01 ~ "*",
                                                         .data[["p.value"]] < 0.01 & .data[["p.value"]] >= 0.001 ~ "**",
                                                         .data[["p.value"]] < 0.001 ~ "***",
                                                         .default = NA))
  mdmr.summary$p.value <- sapply(mdmr.summary$p.value, function(x){
    ifelse(x >= 0.001, sprintf("%.3f", round(x, 3)), "<0.001")
  }, USE.NAMES = FALSE)
  mdmr.summary <- mdmr.summary %>% mutate(Statistic = round(.data[["Statistic"]], 2),Pseudo.R2 = round(.data[["Pseudo.R2"]], 2))
  unique_mdmr <- unique(mdmr.summary$p.value)[unique(mdmr.summary$p.value) != "<0.001"][which(as.numeric(unique(mdmr.summary$p.value)[unique(mdmr.summary$p.value) != "<0.001"]) < 0.05)]
  }else{
    mdmr.summary <- NULL
    unique_mdmr <- NULL
  }


  ## ANOVA
  anova_test_df <- mclapply(features, function(y){
    lmm1 <- model_gen(type = type, y = y, batch = batch, covariates = covariates, interaction = interaction, random = random, smooth = smooth, df = df)
    lmm2 <- update(lmm1, as.formula(paste0(".~. - ", batch)))
    if(type == "gam"){
      anova.test <- anova.gam(lmm2, lmm1, test = "F")
    }else{
      anova.test <- anova(lmm2, lmm1)}
    if(type != "lmer"){
      p <- anova.test[["Pr(>F)"]][2]}else{
        p <- anova.test %>% tidy() %>% pull(.data[["p.value"]])
      }
    anova_df <- data.frame(cbind(y, p[length(p)]))
    colnames(anova_df) <- c("feature", "p.value")
    return(anova_df)
  }, mc.cores = cores) %>% bind_rows()
  anova_test_df$p.value <- p.adjust(anova_test_df$p.value, method = "bonferroni", n = length(anova_test_df$p.value))
  anova_test_df <- anova_test_df %>% arrange(.data[["p.value"]]) %>% mutate(sig = case_when(.data[["p.value"]] < 0.05 & .data[["p.value"]] >= 0.01 ~ "*",
                                                           .data[["p.value"]] < 0.01 & .data[["p.value"]] >= 0.001 ~ "**",
                                                           .data[["p.value"]] < 0.001 ~ "***",
                                                           .default = NA)) %>% mutate(p.value.raw = .data[["p.value"]])
  anova_test_df$p.value <- sapply(anova_test_df$p.value, function(x){
    ifelse(x >= 0.001, sprintf("%.3f", round(x, 3)), "<0.001")
  }, USE.NAMES = FALSE)
  unique_anova <- unique(anova_test_df$p.value)[unique(anova_test_df$p.value) != "<0.001"][which(as.numeric(unique(anova_test_df$p.value)[unique(anova_test_df$p.value) != "<0.001"]) < 0.05)]

  ## Kruskal-Wallis
  kw_test_df <- mclapply(features, function(y){
    KWtest <- kruskal.test(residual_add_df[[y]] ~ residual_add_df[[batch]])
    kw_df <- KWtest %>% tidy() %>% dplyr::select(.data[["p.value"]]) %>% mutate(feature = y)
    kw_df <- kw_df[c(2,1)]
    return(kw_df)
  }, mc.cores = cores) %>% bind_rows()
  kw_test_df$p.value <- p.adjust(kw_test_df$p.value, method = "bonferroni", n = length(kw_test_df$p.value))
  kw_test_df <- kw_test_df %>% arrange(.data[["p.value"]]) %>% mutate(sig = case_when(.data[["p.value"]] < 0.05 & .data[["p.value"]] >= 0.01 ~ "*",
                                                                          .data[["p.value"]] < 0.01 & .data[["p.value"]] >= 0.001 ~ "**",
                                                                          .data[["p.value"]] < 0.001 ~ "***",
                                                                          .default = NA)) %>% mutate(p.value.raw = .data[["p.value"]])
  kw_test_df$p.value <- sapply(kw_test_df$p.value, function(x){
    ifelse(x >= 0.001, sprintf("%.3f", round(x, 3)), "<0.001")
  }, USE.NAMES = FALSE)
  unique_kw <- unique(kw_test_df$p.value)[unique(kw_test_df$p.value) != "<0.001"][which(as.numeric(unique(kw_test_df$p.value)[unique(kw_test_df$p.value) != "<0.001"]) < 0.05)]

  ## Levene's Test
  lv_test_df <- mclapply(features, function(y){
    lmm_multi <- model_gen(type = type, y = y, batch = batch, covariates = covariates, interaction = interaction, random = random, smooth = smooth, df = df)
    fit_residuals <- resid(lmm_multi)
    LVtest <- leveneTest(fit_residuals ~ as.factor(df[[batch]]))
    lv_df <- LVtest %>% tidy() %>% dplyr::select(.data[["p.value"]]) %>% mutate(feature = y)
    lv_df <- lv_df[c(2,1)]
    return(lv_df)
  }, mc.cores = cores) %>% bind_rows()
  lv_test_df$p.value <- p.adjust(lv_test_df$p.value, method = "bonferroni", n = length(lv_test_df$p.value))
  lv_test_df <- lv_test_df %>% arrange(.data[["p.value"]]) %>% mutate(sig = case_when(.data[["p.value"]] < 0.05 & .data[["p.value"]] >= 0.01 ~ "*",
                                                     .data[["p.value"]] < 0.01 & .data[["p.value"]] >= 0.001 ~ "**",
                                                     .data[["p.value"]] < 0.001 ~ "***",
                                                     .default = NA)) %>% mutate(p.value.raw = .data[["p.value"]])
  lv_test_df$p.value <- sapply(lv_test_df$p.value, function(x){
    ifelse(x >= 0.001, sprintf("%.3f", round(x, 3)), "<0.001")
  }, USE.NAMES = FALSE)
  unique_lv <- unique(lv_test_df$p.value)[unique(lv_test_df$p.value) != "<0.001"][which(as.numeric(unique(lv_test_df$p.value)[unique(lv_test_df$p.value) != "<0.001"]) < 0.05)]

  ## Bartlett's Test
  bl_test_df <- tryCatch({
    mclapply(features, function(y){
    lmm_multi <- model_gen(type = type, y = y, batch = batch, covariates = covariates, interaction = interaction, random = random, smooth = smooth, df = df)
    fit_residuals <- resid(lmm_multi)
    BLtest <- bartlett.test(fit_residuals ~ as.factor(df[[batch]]))
    bl_df <- BLtest %>% tidy() %>% dplyr::select(.data[["p.value"]]) %>% mutate(feature = y)
    bl_df <- bl_df[c(2,1)]
    return(bl_df)
  }, mc.cores = cores) %>% bind_rows()}, error = function(e) {
    cat("Less than 2 observations in each group")
    bl_test_df <- data.frame("feature" = NULL, "p.value" = NULL, "sig" = NULL)
    return(bl_test_df)})

  if(nrow(bl_test_df)!=0){
    bl_test_df$p.value <- p.adjust(bl_test_df$p.value, method = "bonferroni", n = length(bl_test_df$p.value))
    bl_test_df <- bl_test_df %>% arrange(.data[["p.value"]]) %>% mutate(sig = case_when(.data[["p.value"]] < 0.05 & .data[["p.value"]] >= 0.01 ~ "*",
                                                                          .data[["p.value"]] < 0.01 & .data[["p.value"]] >= 0.001 ~ "**",
                                                                          .data[["p.value"]] < 0.001 ~ "***",
                                                                          .default = NA)) %>% mutate(p.value.raw = .data[["p.value"]])
    bl_test_df$p.value <- sapply(bl_test_df$p.value, function(x){
    ifelse(x >= 0.001, sprintf("%.3f", round(x, 3)), "<0.001")
    }, USE.NAMES = FALSE)
    unique_bl <- unique(bl_test_df$p.value)[unique(bl_test_df$p.value) != "<0.001"][which(as.numeric(unique(bl_test_df$p.value)[unique(bl_test_df$p.value) != "<0.001"]) < 0.05)]
    }else{unique_bl <- unique(bl_test_df$p.value)[unique(bl_test_df$p.value) != "<0.001"][which(as.numeric(unique(bl_test_df$p.value)[unique(bl_test_df$p.value) != "<0.001"]) < 0.05)]}

  red <- c(unique_kr, unique_fk, unique_mdmr, unique_anova, unique_kw, unique_lv, unique_bl, "<0.001")

  result <- list("residual_add_df" = residual_add_df, "residual_ml_df" = residual_ml_df, "pr.feature" = pr.feature, "pca_summary" = pca_summary, "pca_df" = pca_df, "tsne_df" = tsne_df, "kr_test_df" = kr_test_df, "fk_test_df" = fk_test_df, "mdmr.summary" = mdmr.summary,
                "anova_test_df" = anova_test_df, "kw_test_df" = kw_test_df, "lv_test_df" = lv_test_df, "bl_test_df" = bl_test_df, "red" = red, "info" = info)
  return(result)
}




