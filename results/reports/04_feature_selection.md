---
title: "04 - Chọn biến"
output: html_document
---



Ba tiêu chí sàng lọc, tính trên **toàn bộ dữ liệu** vì chỉ dùng để gợi ý tập biến, không đo hiệu năng dự báo.


``` r
pearson <- tibble(variable = PREDICTORS,
                  abs_r = sapply(PREDICTORS, \(v) abs(cor(df[[v]], df$Diabetes_binary))))
chisq <- tibble(variable = BINARY_VARS,
                chi_sq = sapply(BINARY_VARS, \(v)
                  unname(chisq.test(table(df[[v]], df$Diabetes_binary), correct = FALSE)$statistic))) |>
  mutate(chisq_rank = rank(-chi_sq))
iv <- iv_table(df, "Diabetes_binary", PREDICTORS)

summary_tbl <- tibble(variable = PREDICTORS) |>
  left_join(pearson, "variable") |> left_join(chisq, "variable") |> left_join(iv, "variable") |>
  mutate(pearson_keep = abs_r > 0.05, chisq_top10 = chisq_rank <= 10, iv_keep = iv > 0.10) |>
  arrange(desc(iv))
summary_tbl
```

```
## # A tibble: 21 × 8
##    variable       abs_r chi_sq chisq_rank    iv pearson_keep chisq_top10 iv_keep
##    <chr>          <dbl>  <dbl>      <dbl> <dbl> <lgl>        <lgl>       <lgl>  
##  1 GenHlth        0.301    NA          NA 0.749 TRUE         NA          TRUE   
##  2 HighBP         0.270 18539.          1 0.575 TRUE         TRUE        TRUE   
##  3 BMI            0.224    NA          NA 0.444 TRUE         NA          TRUE   
##  4 Age            0.186    NA          NA 0.362 TRUE         NA          TRUE   
##  5 HighChol       0.210 11218.          3 0.335 TRUE         TRUE        TRUE   
##  6 DiffWalk       0.222 12520.          2 0.298 TRUE         TRUE        TRUE   
##  7 Income         0.173    NA          NA 0.228 TRUE         NA          TRUE   
##  8 PhysHlth       0.175    NA          NA 0.200 TRUE         NA          TRUE   
##  9 HeartDiseaseo… 0.177  7942.          4 0.178 TRUE         TRUE        TRUE   
## 10 Education      0.132    NA          NA 0.128 TRUE         NA          TRUE   
## # ℹ 11 more rows
```

``` r
save_csv(summary_tbl, "feature_selection_summary")
```


``` r
selected <- summary_tbl |> filter(iv_keep) |> pull(variable)
selected
```

```
##  [1] "GenHlth"              "HighBP"               "BMI"                 
##  [4] "Age"                  "HighChol"             "DiffWalk"            
##  [7] "Income"               "PhysHlth"             "HeartDiseaseorAttack"
## [10] "Education"            "PhysActivity"
```

``` r
stopifnot(setequal(selected, SELECTED_VARS))   # hằng số trong R/helpers.R phải khớp
save_csv(tibble(feature = selected), "selected_features")
```

**Kết luận.** 17 biến qua ngưỡng Pearson, 10 biến nhị phân đứng đầu theo chi-bình phương, 11 biến có IV > 0.10.
Tập 11 biến theo IV được dùng cho các mô hình rút gọn. Đây là sàng lọc theo hướng dự báo, không phải bằng chứng nhân quả.
