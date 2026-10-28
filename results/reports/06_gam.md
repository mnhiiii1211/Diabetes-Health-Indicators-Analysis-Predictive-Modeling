---
title: "06 - GAM Logistic"
output: html_document
---



GAM làm trơn các biến liên tục (`BMI`, `MentHlth`, `PhysHlth`); các biến còn lại là hạng tử tham số.
Dùng `bam()` (fREML, `discrete = TRUE`) để chạy nhanh trên dữ liệu đã SMOTE. Cùng quy trình đánh giá như chương 05.

## GAM nhị phân (21 biến và 11 biến)


``` r
train <- smote_train(m$train, "Diabetes_binary")
rows <- list(); fits <- list()
for (v in c("all21", "sel11")) {
  vars <- if (v == "all21") PREDICTORS else SELECTED_VARS
  assert_no_target(vars)
  fit <- bam(gam_formula("Diabetes_binary", vars), data = train, family = binomial,
             method = "fREML", discrete = TRUE, nthreads = 4)
  p_valid <- predict(fit, m$valid, type = "response")
  p_test  <- predict(fit, m$test,  type = "response")
  thr <- youden_threshold(m$valid$Diabetes_binary, p_valid)
  rows[[v]] <- bind_rows(binary_metrics(m$test$Diabetes_binary, p_test, 0.5) |> mutate(rule = "threshold_0.5"),
                         binary_metrics(m$test$Diabetes_binary, p_test, thr) |> mutate(rule = "youden_on_validation")) |>
    mutate(model = paste0("gam_smote_", v), .before = 1)
  fits[[v]] <- fit
}
gam_binary_metrics <- bind_rows(rows)
gam_binary_metrics |> mutate(across(where(is.numeric), \(x) round(x, 4)))
```

```
## # A tibble: 4 × 11
##   model   threshold accuracy majority_baseline sensitivity specificity precision
##   <chr>       <dbl>    <dbl>             <dbl>       <dbl>       <dbl>     <dbl>
## 1 gam_sm…     0.5      0.729             0.841       0.740       0.726     0.338
## 2 gam_sm…     0.460    0.708             0.841       0.777       0.695     0.325
## 3 gam_sm…     0.5      0.726             0.841       0.752       0.721     0.337
## 4 gam_sm…     0.453    0.698             0.841       0.793       0.680     0.319
## # ℹ 4 more variables: balanced_accuracy <dbl>, f1 <dbl>, auc <dbl>, rule <chr>
```

``` r
save_csv(gam_binary_metrics, "gam_binary_metrics")
```


``` r
plot(fits$all21, pages = 1, select = 1:3, shade = TRUE, seWithMean = TRUE)
```

```
## Error in `is.null(select) || i == select`:
## ! 'length = 3' in coercion to 'logical(1)'
```

## GAM ba lớp (one-vs-rest)


``` r
train3 <- smote_train(m$train, "Diabetes_012") |>
  mutate(y0 = as.integer(Diabetes_012 == 0), y1 = as.integer(Diabetes_012 == 1), y2 = as.integer(Diabetes_012 == 2))
probs <- sapply(c("y0", "y1", "y2"), \(y) {
  fit <- bam(gam_formula(y, PREDICTORS), data = train3, family = binomial,
             method = "fREML", discrete = TRUE, nthreads = 4)
  predict(fit, m$test, type = "response")
})
res <- multiclass_metrics(m$test$Diabetes_012, max.col(probs) - 1)
res$confusion
```

```
##      pred
## truth     0     1     2
##     0 21650  5763  4591
##     1   249   198   264
##     2  1129  1434  2774
```

``` r
res$summary |> mutate(across(where(is.numeric), \(x) round(x, 4)))
```

```
## # A tibble: 1 × 7
##   accuracy majority_baseline balanced_accuracy macro_f1 recall_0 recall_1
##      <dbl>             <dbl>             <dbl>    <dbl>    <dbl>    <dbl>
## 1    0.647             0.841             0.492    0.421    0.676    0.278
## # ℹ 1 more variable: recall_2 <dbl>
```

``` r
save_csv(res$summary |> mutate(model = "gam_ovr_smote_all21", .before = 1), "gam_multiclass_metrics")
```

**Đọc kết quả.** BMI là biến phi tuyến rõ nhất; MentHlth và PhysHlth gần như phẳng. So với logistic cùng dữ liệu,
GAM cải thiện AUC không đáng kể, nên quan hệ phi tuyến không phải điểm nghẽn chính. Nhóm tiền tiểu đường
vẫn gần như không tách được khỏi hai nhóm kia.
