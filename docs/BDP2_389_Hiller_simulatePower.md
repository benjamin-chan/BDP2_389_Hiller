---
title: |
  | BDP2-389 Hiller
  | Power and sample size simulation
author: Benjamin Chan (chanb@ohsu.edu)
date: "2018-11-07 17:31:37"
---


```r
source("simulatePower.R")
```

# Summary

The outcome is the post-intervention PSS score, assumed to be normally distributed.
The model is a random intercept model with subjects nested within wave.
The model adjusts for baseline PSS score.

$$
y_\text{post} = \beta_{0j} + \beta_1 x_\text{MBSR1} + \beta_2 x_\text{MBSR2} + \beta_3 y_\text{baseline}
$$

where $j = 1, \ldots, 6$ is the index for wave.
The random intercept accounts for the potential for correlation between subjects within wave.
For example, if the overall economy is good or bad at a particular calendar time, then all subjects recruited before that wave might have correlated stress levels.
The amount of correlation between subjects within wave is the intraclass correlation set by `iccWithinWave`, 5%.

Correlation is also assumed between a subject's baseline and post-intervention stress level.
For example, a person who doesn't get stressed is likely to not be under stress at both time points.
The amount of correlation between time points within subject is the intraclass correlation set by `iccWithinSubject`, 40%.

**Are these correlations reasonable?**

Power simulations assume a study design with 6 waves and 4 subjects in randomized to each intervention arm in each wave.
Simulations results are based on 5000 simulated data sets.


```r
power %>% kable(digits = c(0, rep(3, 7), rep(0, 3), 3))
```



|term      | nominalEffSize| sigma| meanBeta| sdBeta| meanStdBeta| iccWithinSubject| iccWithinWave| groupSize| nReject| nSim| power|
|:---------|--------------:|-----:|--------:|------:|-----------:|----------------:|-------------:|---------:|-------:|----:|-----:|
|MBSR1     |           0.77|     1|    0.773|  0.287|       2.989|              0.4|          0.05|        24|    4034| 5000| 0.807|
|MBSR2     |           0.91|     1|    0.904|  0.285|       3.497|              0.4|          0.05|        24|    4530| 5000| 0.906|
|yBaseline |             NA|    NA|    0.400|  0.114|       3.689|              0.4|          0.05|        24|    4669| 5000| 0.934|

With a total of 24 subjects in each intervention arm 
(72 subjects in total), 
and a within subject correlation of 40% 
and a within wave correlation of 5%, 
there is

* 80.7% 
  power to detect an effect size of 
  0.77
* 90.6% 
  power to detect a standardized effect size of 
  0.91

# Details

`V1` is the covariance matrix for within subject covariance (dimension 2, baseline to post-intervention).
`V2` is the covariance matrix for within wave covariance (dimension 4, number of subjects in each wave in each intervention arm).


```r
V1
```

```
##      [,1] [,2]
## [1,]  1.0  0.4
## [2,]  0.4  1.0
```

```r
V2
```

```
##      [,1] [,2] [,3] [,4]
## [1,] 1.00 0.05 0.05 0.05
## [2,] 0.05 1.00 0.05 0.05
## [3,] 0.05 0.05 1.00 0.05
## [4,] 0.05 0.05 0.05 1.00
```

```r
V
```

```
##      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
## [1,] 1.00 0.05 0.05 0.05 0.40 0.02 0.02 0.02
## [2,] 0.05 1.00 0.05 0.05 0.02 0.40 0.02 0.02
## [3,] 0.05 0.05 1.00 0.05 0.02 0.02 0.40 0.02
## [4,] 0.05 0.05 0.05 1.00 0.02 0.02 0.02 0.40
## [5,] 0.40 0.02 0.02 0.02 1.00 0.05 0.05 0.05
## [6,] 0.02 0.40 0.02 0.02 0.05 1.00 0.05 0.05
## [7,] 0.02 0.02 0.40 0.02 0.05 0.05 1.00 0.05
## [8,] 0.02 0.02 0.02 0.40 0.05 0.05 0.05 1.00
```

Check simulation data.
Make sure it meets expectation.


```r
df %>%
  group_by(intervention) %>%
  summarize(meanBaseline = mean(y0),
            sdBaseline = sd(y0),
            medianBaseline = median(y0),
            minBaseline = min(y0),
            maxBaseline = max(y0)) %>%
  kable(digits = 2)
```



|intervention | meanBaseline| sdBaseline| medianBaseline| minBaseline| maxBaseline|
|:------------|------------:|----------:|--------------:|-----------:|-----------:|
|Control      |         0.00|          1|              0|       -4.55|        4.43|
|MBSR1        |        -0.01|          1|              0|       -4.29|        4.42|
|MBSR2        |         0.00|          1|              0|       -4.50|        4.91|

```r
cor(df %>% select(y0, y1)) %>% kable(digits = 4)
```



|   |     y0|     y1|
|:--|------:|------:|
|y0 | 1.0000| 0.3716|
|y1 | 0.3716| 1.0000|


```r
G <- 
  df %>%
  gather(rep, value, y0, y1) %>%
  ggplot(aes(x = value)) +
  ggtitle("Distributions of simulated data") +
  geom_density(aes(fill = intervention), alpha = 1/2) +
  scale_fill_brewer("Intervention group", palette = "Set1") +
  facet_grid(rep ~ intervention) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))
ggsave("../figures/simDataDensities.png", height = 3)
G <-
  df %>%
  filter(sim <= 10) %>%
  mutate(wave = sprintf("Wave %d", wave)) %>%
  ggplot(aes(x = y0, y = y1)) +
  ggtitle("10 simulated data sets") +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point(alpha = 1/4) +
  geom_rug(alpha = 1/4, sides = "l") +
  facet_grid(wave ~ intervention) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))
ggsave("../figures/simCorrData.png", width = 5)
```

![figures/simDataDensities.png](figures/simDataDensities.png)
![figures/simCorrData.png](figures/simCorrData.png)

Check model simulation output.


```r
G <- 
  simResults %>%
  filter(group == "fixed") %>%
  gather(variable, value, estimate, stdEstimate) %>%
  ggplot(aes(x = value)) +
  ggtitle("Distribution of fixed effects") +
  geom_density(aes(fill = term), alpha = 1/2) +
  facet_grid(term ~ variable, scale = "free") +
  scale_fill_brewer("Model term", palette = "Set1") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))
ggsave("../figures/simModelEstimateDensity.png", height = 6)
G <- 
  simResults %>%
  filter(group == "wave") %>%
  gather(variable, value, estimate) %>%
  ggplot(aes(x = value)) +
  ggtitle("Distribution of random intercept") +
  geom_density(aes(fill = term), alpha = 1/2) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))
ggsave("../figures/simRandomEffectsDensity.png", width = 4, height = 3)
```

![figures/simModelEstimateDensity.png](figures/simModelEstimateDensity.png)
![figures/simRandomEffectsDensity.png](figures/simRandomEffectsDensity.png)


R session information.


```r
sessionInfo()
```

```
## R version 3.5.1 (2018-07-02)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 10 x64 (build 17134)
## 
## Matrix products: default
## 
## locale:
## [1] LC_COLLATE=English_United States.1252 
## [2] LC_CTYPE=English_United States.1252   
## [3] LC_MONETARY=English_United States.1252
## [4] LC_NUMERIC=C                          
## [5] LC_TIME=English_United States.1252    
## 
## attached base packages:
## [1] parallel  stats     graphics  grDevices utils     datasets  methods  
## [8] base     
## 
## other attached packages:
##  [1] bindrcpp_0.2.2    doParallel_1.0.14 iterators_1.0.10 
##  [4] foreach_1.4.4     ggplot2_3.0.0     broom_0.5.0      
##  [7] lme4_1.1-18-1     Matrix_1.2-14     simstudy_0.1.10  
## [10] data.table_1.11.6 dplyr_0.7.6       tidyr_0.8.1      
## [13] magrittr_1.5      knitr_1.20        checkpoint_0.4.5 
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.12.18       RColorBrewer_1.1-2 highr_0.7         
##  [4] plyr_1.8.4         pillar_1.3.0       compiler_3.5.1    
##  [7] nloptr_1.0.4       bindr_0.1.1        tools_3.5.1       
## [10] mvnfast_0.2.5      evaluate_0.12      tibble_1.4.2      
## [13] nlme_3.1-137       gtable_0.2.0       lattice_0.20-35   
## [16] pkgconfig_2.0.2    rlang_0.2.2        withr_2.1.2       
## [19] stringr_1.3.1      grid_3.5.1         tidyselect_0.2.4  
## [22] glue_1.3.0         R6_2.2.2           minqa_1.2.4       
## [25] reshape2_1.4.3     purrr_0.2.5        codetools_0.2-15  
## [28] backports_1.1.2    scales_1.0.0       splines_3.5.1     
## [31] MASS_7.3-50        assertthat_0.2.0   colorspace_1.3-2  
## [34] labeling_0.3       stringi_1.2.4      lazyeval_0.2.1    
## [37] munsell_0.5.0      crayon_1.3.4
```
