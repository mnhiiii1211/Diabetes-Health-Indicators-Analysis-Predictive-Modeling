# Shared helpers. Every chapter does: source("R/helpers.R")  (run from the repo root,
# or via knitr with root.dir set by chapters).

suppressPackageStartupMessages({
  library(tidyverse)
  library(caret)
  library(pROC)
})

SEED      <- 123
DATA_PATH <- "dataset/diabetes_012_health_indicators_BRFSS2015.csv"

BINARY_VARS  <- c("HighBP", "HighChol", "CholCheck", "Smoker", "Stroke",
                  "HeartDiseaseorAttack", "PhysActivity", "Fruits", "Veggies",
                  "HvyAlcoholConsump", "AnyHealthcare", "NoDocbcCost",
                  "DiffWalk", "Sex")
COUNT_VARS   <- c("BMI", "MentHlth", "PhysHlth")
ORDINAL_VARS <- c("GenHlth", "Age", "Education", "Income")
PREDICTORS   <- c(BINARY_VARS, COUNT_VARS, ORDINAL_VARS)   # 21 variables

# The 11 variables chosen in chapter 04 (IV > 0.10).
SELECTED_VARS <- c("GenHlth", "HighBP", "BMI", "Age", "HighChol", "DiffWalk",
                   "Income", "PhysHlth", "HeartDiseaseorAttack", "Education",
                   "PhysActivity")

# ---- Data ------------------------------------------------------------------

load_data <- function(path = DATA_PATH) {
  df <- read.csv(path)
  stopifnot(nrow(df) == 253680, all(c("Diabetes_012", PREDICTORS) %in% names(df)))
  df$Diabetes_binary <- as.integer(df$Diabetes_012 >= 1)
  df
}

# Binary -> factor, ordinal -> ordered factor, targets -> factor.
to_model_df <- function(df) {
  df |>
    mutate(
      Diabetes_012    = factor(Diabetes_012, levels = 0:2),
      Diabetes_binary = factor(Diabetes_binary, levels = 0:1),
      across(all_of(BINARY_VARS), ~ factor(.x, levels = 0:1)),
      across(all_of(ORDINAL_VARS), ordered)
    )
}

# Stratified 70/15/15 split. Validation is used ONLY to choose decision
# thresholds; test is used once for reporting. SMOTE is applied to train only.
split_data <- function(df, seed = SEED) {
  set.seed(seed)
  i1 <- createDataPartition(df$Diabetes_012, p = 0.70, list = FALSE)
  rest <- df[-i1, ]
  i2 <- createDataPartition(rest$Diabetes_012, p = 0.50, list = FALSE)
  list(train = df[i1, ], valid = rest[i2, ], test = rest[-i2, ])
}

# SMOTE-NC on a TRAINING split only. Keeps the predictors plus one target, so the
# other target column can never leak into a model fitted on the result.
smote_train <- function(train_model_df, target, k = 5, seed = SEED) {
  set.seed(seed)
  x <- train_model_df[, c(target, PREDICTORS)]
  as.data.frame(themis::smotenc(x, var = target, k = k, over_ratio = 1))
}

save_csv <- function(x, name) {
  dir.create("results", showWarnings = FALSE)
  readr::write_csv(as.data.frame(x), file.path("results", paste0(name, ".csv")))
  invisible(x)
}

# Guard against using a target (or a transform of it) as a predictor.
assert_no_target <- function(vars) {
  bad <- intersect(vars, c("Diabetes_012", "Diabetes_binary"))
  if (length(bad)) stop("Target used as predictor: ", paste(bad, collapse = ", "))
  invisible(TRUE)
}

# ---- Metrics ---------------------------------------------------------------

binary_metrics <- function(truth, prob, threshold = 0.5) {
  truth <- as.integer(as.character(truth))
  pred  <- as.integer(prob >= threshold)
  tp <- sum(pred == 1 & truth == 1); fp <- sum(pred == 1 & truth == 0)
  tn <- sum(pred == 0 & truth == 0); fn <- sum(pred == 0 & truth == 1)
  sens <- tp / (tp + fn); spec <- tn / (tn + fp)
  ppv  <- if (tp + fp > 0) tp / (tp + fp) else NA_real_
  tibble(
    threshold         = threshold,
    accuracy          = (tp + tn) / length(truth),
    majority_baseline = max(mean(truth), 1 - mean(truth)),
    sensitivity       = sens,
    specificity       = spec,
    precision         = ppv,
    balanced_accuracy = (sens + spec) / 2,
    f1                = if (is.na(ppv) || ppv + sens == 0) NA_real_ else 2 * ppv * sens / (ppv + sens),
    auc               = as.numeric(auc(roc(truth, prob, quiet = TRUE, direction = "<")))
  )
}

# Threshold maximising Youden's J on a validation set.
youden_threshold <- function(truth, prob) {
  r <- roc(as.integer(as.character(truth)), prob, quiet = TRUE, direction = "<")
  as.numeric(unlist(coords(r, "best", best.method = "youden", ret = "threshold")))[1]
}

multiclass_metrics <- function(truth, pred) {
  truth <- factor(truth, levels = 0:2); pred <- factor(pred, levels = 0:2)
  cm <- table(truth = truth, pred = pred)
  recall    <- as.numeric(diag(cm) / rowSums(cm))
  precision <- as.numeric(diag(cm) / colSums(cm))
  f1 <- ifelse(is.na(precision) | precision + recall == 0, 0,
               2 * precision * recall / (precision + recall))
  list(confusion = cm,
       summary = tibble(accuracy = sum(diag(cm)) / sum(cm),
                        majority_baseline = max(rowSums(cm)) / sum(cm),
                        balanced_accuracy = mean(recall), macro_f1 = mean(f1),
                        recall_0 = recall[1], recall_1 = recall[2], recall_2 = recall[3]))
}

# ---- Inference -------------------------------------------------------------

boot_mean_ci <- function(x, B = 5000, level = 0.95) {
  d <- replicate(B, mean(sample(x, replace = TRUE)))
  q <- quantile(d, c((1 - level) / 2, 1 - (1 - level) / 2), names = FALSE)
  tibble(estimate = mean(x), lower = q[1], upper = q[2])
}

# Empirical-likelihood CI: all mu with -2 log ELR <= chi-square(1) quantile.
el_mean_ci <- function(x, level = 0.95) {
  crit <- qchisq(level, df = 1)
  m <- mean(x); se <- sd(x) / sqrt(length(x))
  f <- function(mu) emplik::el.test(x, mu)[["-2LLR"]] - crit
  tibble(estimate = m,
         lower = uniroot(f, c(m - 6 * se, m))$root,
         upper = uniroot(f, c(m, m + 6 * se))$root)
}

boot_diff_ci <- function(x1, x0, B = 5000, level = 0.95) {
  d <- replicate(B, mean(sample(x1, replace = TRUE)) - mean(sample(x0, replace = TRUE)))
  q <- quantile(d, c((1 - level) / 2, 1 - (1 - level) / 2), names = FALSE)
  tibble(estimate = mean(x1) - mean(x0), lower = q[1], upper = q[2])
}

# Two-sided permutation test for a difference in means. The p-value is
# (1 + #extreme) / (R + 1), so it is never reported as exactly 0.
perm_test <- function(x, g, R = 5000) {
  g <- as.integer(as.character(g))
  n <- length(x); n1 <- sum(g == 1); n0 <- n - n1; S <- sum(x)
  obs <- mean(x[g == 1]) - mean(x[g == 0])
  sd_pooled <- sqrt(((n1 - 1) * var(x[g == 1]) + (n0 - 1) * var(x[g == 0])) / (n - 2))
  d <- replicate(R, { s1 <- sum(x[sample.int(n, n1)]); s1 / n1 - (S - s1) / n0 })
  tibble(mean_diff = obs, cohens_d = obs / sd_pooled,
         p_value = (1 + sum(abs(d) >= abs(obs) - 1e-12)) / (R + 1))
}

cramers_v <- function(x, y) {
  tab <- table(x, y)
  chi <- suppressWarnings(chisq.test(tab, correct = FALSE)$statistic)
  as.numeric(sqrt(chi / (sum(tab) * (min(dim(tab)) - 1))))
}

# ---- Weight of evidence / information value --------------------------------

iv_table <- function(df, target, vars, bins = 10) {
  y <- as.integer(as.character(df[[target]]))
  bind_rows(lapply(vars, function(v) {
    x <- df[[v]]
    if (is.numeric(x) && length(unique(x)) > 10) {
      br <- unique(quantile(x, seq(0, 1, length.out = bins + 1), names = FALSE))
      x <- if (length(br) >= 3) cut(x, br, include.lowest = TRUE) else factor(x)
    }
    tab <- table(x, y)
    ev  <- pmax(tab[, "1"], 0.5); ne <- pmax(tab[, "0"], 0.5)
    pe  <- ev / sum(ev); pn <- ne / sum(ne)
    tibble(variable = v, iv = sum((pe - pn) * log(pe / pn)))
  })) |> arrange(desc(iv))
}
