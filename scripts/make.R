library(checkpoint)
checkpoint("2018-10-01", use.knitr = TRUE)

library(knitr)
opts_chunk$set(fig.path = "figures", dpi = 300)
knit("scripts/simulatePower.Rmd", output = "docs/BDP2_389_Hiller_simulatePower.md")
