---
title: "05 - Hồi quy Logistic và Multinomial Logistic"
output: html_document
---



## Quy trình đánh giá (khác bản gốc)

- Chia **70 / 15 / 15** (train / validation / test), phân tầng theo `Diabetes_012`.
- **SMOTE-NC chỉ áp dụng trên train.**
- **Ngưỡng phân loại chọn trên validation** (Youden), test chỉ dùng để báo cáo. Bản gốc chọn 0.3 và 0.6 bằng cách nhìn kết quả test.
- Không dùng trọng số lớp riêng cho một mô hình (bản gốc dùng weight = 5 cho `logistic_0` nhưng không cho `logistic_2`).
- Báo cáo AUC, độ nhạy, độ đặc hiệu, precision, balanced accuracy, **kèm baseline đoán toàn lớp 0**; accuracy đứng một mình không có nghĩa.

## Logistic nhị phân: 2 cách dữ liệu x 2 tập biến


``` r
train_sets <- list(original = m$train[, c("Diabetes_binary", PREDICTORS)],
                   smote    = smote_train(m$train, "Diabetes_binary"))
var_sets <- list(all21 = PREDICTORS, sel11 = SELECTED_VARS)

metrics <- list(); coefs <- list()
for (s in names(train_sets)) for (v in names(var_sets)) {
  assert_no_target(var_sets[[v]])
  fit <- glm(reformulate(var_sets[[v]], "Diabetes_binary"), data = train_sets[[s]], family = binomial)
  p_valid <- predict(fit, m$valid, type = "response")
  p_test  <- predict(fit, m$test,  type = "response")
  thr <- youden_threshold(m$valid$Diabetes_binary, p_valid)
  id <- paste(s, v, sep = "_")
  metrics[[id]] <- bind_rows(
    binary_metrics(m$test$Diabetes_binary, p_test, 0.5) |> mutate(rule = "threshold_0.5"),
    binary_metrics(m$test$Diabetes_binary, p_test, thr) |> mutate(rule = "youden_on_validation")
  ) |> mutate(model = id, .before = 1)
  co <- coef(summary(fit))
  coefs[[id]] <- tibble(model = id, term = rownames(co), estimate = co[, 1], se = co[, 2],
                        p_value = co[, 4], odds_ratio = exp(co[, 1]))
}
logistic_metrics <- bind_rows(metrics)
logistic_metrics |> mutate(across(where(is.numeric), \(x) round(x, 4)))
```

```
## # A tibble: 8 × 11
##   model   threshold accuracy majority_baseline sensitivity specificity precision
##   <chr>       <dbl>    <dbl>             <dbl>       <dbl>       <dbl>     <dbl>
## 1 origin…     0.5      0.849             0.841       0.189       0.973     0.572
## 2 origin…     0.139    0.701             0.841       0.803       0.682     0.323
## 3 origin…     0.5      0.849             0.841       0.182       0.975     0.578
## 4 origin…     0.133    0.688             0.841       0.810       0.665     0.314
## 5 smote_…     0.5      0.723             0.841       0.751       0.717     0.334
## 6 smote_…     0.461    0.701             0.841       0.787       0.685     0.320
## 7 smote_…     0.5      0.719             0.841       0.764       0.711     0.333
## 8 smote_…     0.486    0.712             0.841       0.777       0.699     0.328
## # ℹ 4 more variables: balanced_accuracy <dbl>, f1 <dbl>, auc <dbl>, rule <chr>
```

``` r
save_csv(logistic_metrics, "logistic_metrics")
save_csv(bind_rows(coefs), "logistic_coefficients")
```

## Multinomial logistic trên `Diabetes_012`

`Diabetes_binary` **không** nằm trong tập biến giải thích (bản gốc để lọt biến này vào mô hình
`multi_2`, tạo ra con số 93.49% sai).


``` r
mt_train <- list(original = m$train[, c("Diabetes_012", PREDICTORS)],
                 smote    = smote_train(m$train, "Diabetes_012"))
mt_rows <- list(); mt_cm <- list(); mt_rrr <- NULL
for (s in names(mt_train)) for (v in names(var_sets)) {
  assert_no_target(var_sets[[v]])
  fit <- multinom(reformulate(var_sets[[v]], "Diabetes_012"), data = mt_train[[s]],
                  maxit = 300, trace = FALSE)
  res <- multiclass_metrics(m$test$Diabetes_012, predict(fit, m$test))
  id <- paste(s, v, sep = "_")
  mt_rows[[id]] <- res$summary |> mutate(model = id, .before = 1)
  mt_cm[[id]] <- res$confusion
  if (id == "original_sel11") mt_rrr <- exp(t(coef(fit)))
}
multinom_metrics <- bind_rows(mt_rows)
multinom_metrics |> mutate(across(where(is.numeric), \(x) round(x, 4)))
```

```
## # A tibble: 4 × 8
##   model  accuracy majority_baseline balanced_accuracy macro_f1 recall_0 recall_1
##   <chr>     <dbl>             <dbl>             <dbl>    <dbl>    <dbl>    <dbl>
## 1 origi…    0.848             0.841             0.386    0.395    0.978    0    
## 2 origi…    0.847             0.841             0.384    0.393    0.979    0    
## 3 smote…    0.624             0.841             0.498    0.416    0.649    0.332
## 4 smote…    0.623             0.841             0.500    0.416    0.645    0.322
## # ℹ 1 more variable: recall_2 <dbl>
```

``` r
save_csv(multinom_metrics, "multinomial_metrics")
mt_cm
```

```
## $original_all21
##      pred
## truth     0     1     2
##     0 31303     0   701
##     1   646     0    65
##     2  4385     0   952
## 
## $original_sel11
##      pred
## truth     0     1     2
##     0 31330     0   674
##     1   646     0    65
##     2  4420     0   917
## 
## $smote_all21
##      pred
## truth     0     1     2
##     0 20767  6810  4427
##     1   221   236   254
##     2   976  1627  2734
## 
## $smote_sel11
##      pred
## truth     0     1     2
##     0 20648  6612  4744
##     1   203   229   279
##     2   918  1580  2839
```

``` r
if (!is.null(mt_rrr)) save_csv(as.data.frame(mt_rrr) |> rownames_to_column("term"), "multinomial_rrr_sel11")
```

**Đọc kết quả.** Trên dữ liệu gốc, mô hình đoán gần như toàn bộ về lớp 0 (nhóm tiền tiểu đường gần như không nhận diện được)
nên accuracy chỉ nhỉnh baseline. SMOTE kéo recall của lớp thiểu số lên nhưng phải đánh đổi bằng nhiều dương tính giả.
Hiệu năng giữa tập 21 và 11 biến gần như tương đương.
