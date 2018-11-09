# Load packages.
library(knitr)
library(magrittr)
library(tidyr)
library(dplyr)
library(simstudy)
library(lme4)
library(broom)
library(ggplot2)
library(parallel)
library(doParallel)

# Define study design and simulation parameters.
# Set random number seed.
M <-    6  # Number of waves
N <-    4  # Size in each wave
S <- 5000  # Number of simulations
design <-
  expand.grid(sim = seq(1, S),
              intervention = c("Control", "MBSR"),
              wave = seq(1, M)) %>%
  arrange(sim, intervention, wave)
"2018-11-05" %>% as.Date %>% as.numeric %>% set.seed()

# Define effect sizes, standard deviations, and within-cluster intraclass correlation.
effsize <- c(0, 0.77)
sigma   <- rep(1, 2)
iccWithinSubject <- 0.4
iccWithinWave <- 0.05
V1 <- matrix(c(1, iccWithinSubject, iccWithinSubject, 1), nrow = 2, ncol = 2)
V2 <- diag(N)
V2[lower.tri(V2)] <- iccWithinWave
V2[upper.tri(V2)] <- iccWithinWave
V <- kronecker(V1, V2)

# Define simulation functions.
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
    fit %>%
    tidy() %>%
      filter(grepl("MBSR", term) | 
             term == "y0" | 
             grepl("wave", term)) %>%
      mutate(term = case_when(grepl("MBSR", term) ~ gsub("intervention", "", term),
                              term == "y0" ~ "yBaseline",
                              grepl("wave", term) ~ "randomIntercept",
                              TRUE ~ NA_character_)) %>%
      mutate(rejectNull = abs(statistic) > qt(0.975, summary(fit)$devcomp$dims["n"] - 1))
  data.frame(sim = i, 
             beta)
}

# Simulate data.
df <- bind_rows(simCorrData("Control", effsize[1], sigma[1]),
                simCorrData("MBSR"   , effsize[2], sigma[2]))

# Simulate power.
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
power <- 
  simResults %>%
  filter(grepl("MBSR", term) | 
         term == "yBaseline") %>%
  group_by(term) %>%
  summarize(nReject = sum(rejectNull),
            nSim = n(),
            meanBeta = mean(estimate),
            sdBeta = sd(estimate)) %>%
  mutate(power = nReject / nSim) %>%
  mutate(nominalEffSize = c(effsize[2], NA),
         sigma = c(sigma[2], NA),
         iccWithinSubject = iccWithinSubject,
         iccWithinWave = iccWithinWave,
         groupSize = M * N) %>%
  select(term, nominalEffSize, sigma, meanBeta, sdBeta, iccWithinSubject, iccWithinWave, groupSize, nReject, nSim, power)

# Save model object for further evaluation.
fit <- 
    df %>%
    filter(sim == 1) %>%
    lmer(y1 ~ intervention + y0 + (1 | wave), data = .)
save(fit, file = "../data/processed/lmer_object.RData")
# load("../data/processed/lmer_object.RData")

# Write power table to CSV file
power %>% write.csv(file = "../data/processed/power.csv", row.names = FALSE)
