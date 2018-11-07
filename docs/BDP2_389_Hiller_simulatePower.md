---
title: |
  | BDP2-389 Hiller
  | Power and sample size simulation
author: Benjamin Chan (chanb@ohsu.edu)
date: "2018-11-06 23:57:18"
---

Load packages.


```r
library(knitr)
library(magrittr)
library(tidyr)
```

```
## 
## Attaching package: 'tidyr'
```

```
## The following object is masked from 'package:magrittr':
## 
##     extract
```

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(simstudy)
```

```
## Loading required package: data.table
```

```
## 
## Attaching package: 'data.table'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     between, first, last
```

```r
library(lme4)
```

```
## Loading required package: Matrix
```

```
## 
## Attaching package: 'Matrix'
```

```
## The following object is masked from 'package:tidyr':
## 
##     expand
```

```r
library(broom)
library(ggplot2)
library(parallel)
library(doParallel)
```

```
## Loading required package: foreach
```

```
## Loading required package: iterators
```

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
##  [1] doParallel_1.0.14 iterators_1.0.10  foreach_1.4.4    
##  [4] ggplot2_3.0.0     broom_0.5.0       lme4_1.1-18-1    
##  [7] Matrix_1.2-14     simstudy_0.1.10   data.table_1.11.6
## [10] dplyr_0.7.6       tidyr_0.8.1       magrittr_1.5     
## [13] knitr_1.20        checkpoint_0.4.5 
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.12.18     plyr_1.8.4       pillar_1.3.0     compiler_3.5.1  
##  [5] nloptr_1.0.4     bindr_0.1.1      tools_3.5.1      evaluate_0.12   
##  [9] tibble_1.4.2     nlme_3.1-137     gtable_0.2.0     lattice_0.20-35 
## [13] pkgconfig_2.0.2  rlang_0.2.2      bindrcpp_0.2.2   withr_2.1.2     
## [17] stringr_1.3.1    grid_3.5.1       tidyselect_0.2.4 glue_1.3.0      
## [21] R6_2.2.2         minqa_1.2.4      purrr_0.2.5      codetools_0.2-15
## [25] backports_1.1.2  scales_1.0.0     splines_3.5.1    MASS_7.3-50     
## [29] assertthat_0.2.0 colorspace_1.3-2 stringi_1.2.4    lazyeval_0.2.1  
## [33] munsell_0.5.0    crayon_1.3.4
```

Define study design and simulation parameters.
Set random number seed.


```r
M <-    6  # Number of waves
N <-    4  # Size in each wave
S <- 5000  # Number of simulations
design <-
  expand.grid(sim = seq(1, S),
              intervention = c("Control", "MBSR1", "MBSR2"),
              wave = seq(1, M)) %>%
  arrange(sim, intervention, wave)
"2018-11-05" %>% as.Date %>% as.numeric %>% set.seed()
```

Define effect sizes, standard deviations, and within-cluster intraclass correlation.
`V1` is the covariance matrix for within subject covariance (dimension 2, baseline to post-intervention).
`V2` is the covariance matrix for within wave covariance (dimension 4, number of subjects in each wave in each intervention arm).


```r
effsize <- c(0, 0.80, 0.94)
sigma   <- rep(1, 3)
iccWithinSubject <- 0.5
iccWithinWave <- 0.1
V1 <- matrix(c(1, iccWithinSubject^2, iccWithinSubject^2, 1), nrow = 2, ncol = 2)
V2 <- diag(N)
V2[lower.tri(V2)] <- iccWithinWave^2
V2[upper.tri(V2)] <- iccWithinWave^2
V <- kronecker(V1, V2)
V
```

```
##        [,1]   [,2]   [,3]   [,4]   [,5]   [,6]   [,7]   [,8]
## [1,] 1.0000 0.0100 0.0100 0.0100 0.2500 0.0025 0.0025 0.0025
## [2,] 0.0100 1.0000 0.0100 0.0100 0.0025 0.2500 0.0025 0.0025
## [3,] 0.0100 0.0100 1.0000 0.0100 0.0025 0.0025 0.2500 0.0025
## [4,] 0.0100 0.0100 0.0100 1.0000 0.0025 0.0025 0.0025 0.2500
## [5,] 0.2500 0.0025 0.0025 0.0025 1.0000 0.0100 0.0100 0.0100
## [6,] 0.0025 0.2500 0.0025 0.0025 0.0100 1.0000 0.0100 0.0100
## [7,] 0.0025 0.0025 0.2500 0.0025 0.0100 0.0100 1.0000 0.0100
## [8,] 0.0025 0.0025 0.0025 0.2500 0.0100 0.0100 0.0100 1.0000
```

Define simulation functions.


```r
simCorrData <- function (group, es, s, data = design) {
  require(magrittr)
  require(tidyr)
  require(simstudy)
  bind_cols(data %>%
              filter(intervention == group) %>%
              select(sim, intervention),
            genCorData(M * S, 
                       mu = c(rep(0, N), rep(es, N)), 
                       sigma = s, 
                       corMatrix = V,
                       idname = "wave")) %>%
    gather(key, y, -sim, -intervention, -wave) %>%
    mutate(key = as.numeric(gsub("V", "", key))) %>%
    mutate(id = (key - 1) %% N + 1) %>%
    mutate(rep = sprintf("y%d", floor((key - 1) / N))) %>%
    arrange(sim, intervention, wave, id, rep) %>%
    select(-key) %>%
    spread(rep, y) %>%
    mutate(wave = (wave - 1) %% M + 1)
}
simModel <- function (i, data = df) {
  require(magrittr)
  require(dplyr)
  require(broom)
  require(lme4)
  fit <- 
    data %>%
    filter(sim == i) %>%
    lmer(y1 ~ intervention + y0 + (1 | wave), data = .)
  beta <-
    tidy(fit, effects = "fixed") %>%
      filter(!grepl("Intercept", .$term)) %>%
      mutate(stdEstimate = estimate / std.error) %>%
      mutate(rejectNull = abs(statistic) > qt(0.975, summary(fit)$devcomp$dims["n"] - 1))
  data.frame(sim = i,
             beta)
}
```

Simulate data and plot to check.


```r
df <- bind_rows(simCorrData("Control", effsize[1], sigma[1]),
                simCorrData("MBSR1"  , effsize[2], sigma[2]),
                simCorrData("MBSR2"  , effsize[3], sigma[3]))
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
|Control      |         0.00|          1|           0.01|       -4.55|        4.43|
|MBSR1        |        -0.01|          1|           0.00|       -4.30|        4.36|
|MBSR2        |         0.00|          1|           0.00|       -4.50|        4.91|

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
```

```
## Saving 7 x 3 in image
```

```r
cor(df %>% select(y0, y1)) %>% kable(digits = 4)
```



|   |     y0|     y1|
|:--|------:|------:|
|y0 | 1.0000| 0.2308|
|y1 | 0.2308| 1.0000|

```r
G <-
  df %>%
  filter(sim <= 10) %>%
  mutate(wave = sprintf("Wave %d", wave)) %>%
  ggplot(aes(x = y0, y = y1)) +
  ggtitle("10 simulated data sets") +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point(alpha = 1/4) +
  facet_grid(wave ~ intervention) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))
ggsave("../figures/simCorrData.png", width = 5)
```

```
## Saving 5 x 7 in image
```

![figures/simDataDensities.png](figures/simDataDensities.png)
![figures/simCorrData.png](figures/simCorrData.png)

Simulate power.


```r
cores <- min(4, detectCores())
cl <- makeCluster(cores)
registerDoParallel(cl)
simResults <- foreach (i = seq(1, S), .combine = rbind) %dopar% {
  require(magrittr)
  require(dplyr)
  require(broom)
  require(lme4)
  simModel(i)
}
stopCluster(cl)
powerTable <- 
  simResults %>%
  group_by(term) %>%
  summarize(nReject = sum(rejectNull),
            nSim = n(),
            meanBeta = mean(estimate),
            sdBeta = sd(estimate),
            meanStdBeta = mean(stdEstimate)) %>%
  mutate(power = nReject / nSim) %>%
  mutate(nominalEffSize = c(effsize[2:3], NA),
         sigma = c(sigma[2:3], NA),
         iccWithinSubject = iccWithinSubject,
         iccWithinWave = iccWithinWave,
         groupSize = M * N) %>%
  select(term, nominalEffSize, sigma, meanBeta, sdBeta, meanStdBeta, iccWithinSubject, iccWithinWave, groupSize, nReject, nSim, power)
G <- 
  simResults %>%
  gather(variable, value, estimate, stdEstimate) %>%
  ggplot(aes(x = value)) +
  ggtitle("Distribution of fixed effects") +
  geom_density(aes(fill = term), alpha = 1/2) +
  facet_grid(term ~ variable, scale = "free_y") +
  scale_fill_brewer("Model term", palette = "Set1") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))
ggsave("../figures/simModelEstimateDensity.png", height = 6)
```

```
## Saving 7 x 6 in image
```

![figures/simModelEstimateDensity.png](figures/simModelEstimateDensity.png)


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
The amount of correlation between subjects within wave is the intraclass correlation set by `iccWithinWave`, 0.10.

Correlation is also assumed between a subject's baseline and post-intervention stress level.
For example, a person who doesn't get stressed is likely to not be under stress at both time points.
The amount of correlation between time points within subject is the intraclass correlation set by `iccWithinSubject`, 0.50.

Power simulations assume a study design with 6 waves and 4 subjects in randomized to each intervention arm in each wave.
Simulations results are based on 5000 simulated data sets.


```r
powerTable %>% kable(digits = c(0, rep(3, 7), rep(0, 3), 3))
```



|term              | nominalEffSize| sigma| meanBeta| sdBeta| meanStdBeta| iccWithinSubject| iccWithinWave| groupSize| nReject| nSim| power|
|:-----------------|--------------:|-----:|--------:|------:|-----------:|----------------:|-------------:|---------:|-------:|----:|-----:|
|interventionMBSR1 |           0.80|     1|    0.803|  0.287|       2.922|              0.5|           0.1|        24|    4033| 5000| 0.807|
|interventionMBSR2 |           0.94|     1|    0.934|  0.285|       3.402|              0.5|           0.1|        24|    4524| 5000| 0.905|
|y0                |             NA|    NA|    0.250|  0.121|       2.179|              0.5|           0.1|        24|    2827| 5000| 0.565|

```r
powerTable %>% write.csv(file = "../data/processed/powerTable.csv", row.names = FALSE)
```
