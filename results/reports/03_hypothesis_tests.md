---
title: "03 - Kiểm định giả thuyết"
output: html_document
---



Với n = 253,680 hầu như mọi kiểm định đều có p rất nhỏ, nên bên cạnh p-value ta báo cáo **cỡ hiệu ứng**
(phi cho chi-bình phương, Cohen's d cho hoán vị) và hiệu chỉnh BH. P-value hoán vị tính theo
(1 + số lần cực đoan) / (R + 1) nên không bao giờ bằng 0.

## Chi-bình phương cho biến nhị phân


``` r
chi <- bind_rows(lapply(BINARY_VARS, \(v) {
  tab <- table(df[[v]], df$Diabetes_binary)
  t <- chisq.test(tab, correct = FALSE)
  tibble(variable = v, chi_sq = unname(t$statistic), p_value = t$p.value,
         phi = sqrt(unname(t$statistic) / sum(tab)))
})) |>
  mutate(p_adj_bh = p.adjust(p_value, "BH")) |> arrange(desc(chi_sq))
chi
```

```
## # A tibble: 14 × 5
##    variable              chi_sq   p_value    phi  p_adj_bh
##    <chr>                  <dbl>     <dbl>  <dbl>     <dbl>
##  1 HighBP               18539.  0         0.270  0        
##  2 DiffWalk             12520.  0         0.222  0        
##  3 HighChol             11218.  0         0.210  0        
##  4 HeartDiseaseorAttack  7942.  0         0.177  0        
##  5 PhysActivity          3738.  0         0.121  0        
##  6 Stroke                2786.  0         0.105  0        
##  7 CholCheck             1169.  3.62e-256 0.0679 7.25e-256
##  8 Smoker                1000.  2.02e-219 0.0628 3.54e-219
##  9 Veggies                890.  1.76e-195 0.0592 2.74e-195
## 10 HvyAlcoholConsump      815.  2.88e-179 0.0567 4.04e-179
## 11 Fruits                 449.  9.93e-100 0.0421 1.26e- 99
## 12 NoDocbcCost            367.  9.29e- 82 0.0380 1.08e- 81
## 13 Sex                    222.  2.78e- 50 0.0296 2.99e- 50
## 14 AnyHealthcare           50.3 1.33e- 12 0.0141 1.33e- 12
```

``` r
save_csv(chi, "tests_chisq_binary")
```

## Kiểm định hoán vị cho biến định lượng và thứ bậc


``` r
perm <- bind_rows(lapply(c(COUNT_VARS, ORDINAL_VARS), \(v)
  perm_test(df[[v]], df$Diabetes_binary, R = R_PERM) |> mutate(variable = v, .before = 1))) |>
  mutate(p_adj_bh = p.adjust(p_value, "BH"))
perm
```

```
## # A tibble: 7 × 5
##   variable  mean_diff cohens_d  p_value p_adj_bh
##   <chr>         <dbl>    <dbl>    <dbl>    <dbl>
## 1 BMI           4.06     0.630 0.000200 0.000200
## 2 MentHlth      1.53     0.206 0.000200 0.000200
## 3 PhysHlth      4.19     0.488 0.000200 0.000200
## 4 GenHlth       0.882    0.866 0.000200 0.000200
## 5 Age           1.56     0.519 0.000200 0.000200
## 6 Education    -0.357   -0.365 0.000200 0.000200
## 7 Income       -0.982   -0.481 0.000200 0.000200
```

``` r
save_csv(perm, "tests_permutation")
```

**Kết luận.** Ý nghĩa thống kê không đồng nghĩa với ý nghĩa thực tiễn: các biến có |phi| hoặc |d| lớn
(HighBP, DiffWalk, HighChol, GenHlth, BMI) mới đáng quan tâm; AnyHealthcare, Sex, Fruits có cỡ hiệu ứng rất nhỏ
dù p < 0.05.
