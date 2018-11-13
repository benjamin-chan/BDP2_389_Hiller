---
title: "BDP2-389 Hiller: Notes"
author: "Benjamin Chan (chanb@ohsu.edu)"
date: "Modified: 2018-11-13"
---


# Notes

* [Power Calculations in Stata](https://dimewiki.worldbank.org/wiki/Power_Calculations_in_Stata)
* [Power analysis for cluster randomized designs](https://www.stata.com/new-in-stata/power-analysis-for-cluster-randomized-designs/)


# Study design

* 3 study groups
  * Control
  * MBSR in-person
  * MBSR video (not used for SA1 or SA2 analysis)
* Subject recruitment and intervention occurs in waves; need to account for clustering by wave
  * 6 waves over the timetable
  * Subjects at each wave will be randomized to any of the 3 study groups; i.e., each cluster will have all study groups represented
* Assessments made at baseline, `t0`, and post-intervention, `t1`
* Calculate power for scenario where 4 subjects are randomzied in each study group in each wave


# SA1

* Efficacy of stress reduction
* Primary outcome: PSS at `t1`
* Secondary outcomes at `t1`
  * Salivary cortisol
  * PDQ-39
  * HamA&D
* Use values at `t0` to adjust
* Basic model is

$$
y_\text{post} = \beta_{0j} + \beta_1 x_\text{MBSR} + \beta_2 y_\text{baseline} + \gamma_1 x_1 + \ldots + \gamma_k x_k
$$

where $j = 1, \ldots, 6$ is the index for wave; 
and $x_1, \ldots, x_k$ are covariates.


# SA2

* Effects of stress reduction on cognitive and motor symptoms
* Outcomes
  * Cognitive
    * Semantic fluency
    * Digit Span sequence
  * Motor
    * UPDRS motor subscore
* Basic model is same as for SA1


# SA3

* Feasibility and efficacy of video-based intervention
* Ideally a non-inferiority or equivalence framework
* Small sample size
* Video-based intervention might not be a randomization option throughout the duration of the study
* May preclude statistical comparison


# Deliverables

* Import data from raw source to analytic data set
  * 0.5 day
* Models for SA1
  * 4 models for each outcome
    * PSS
    * Salivary cortisol
    * PDQ-39
    * HamA&D
  * 1 day for the primary outcome, 1 day for all secondary outcomes
* Models for SA2
  * 3 models for each outcome
    * Semantic fluency
    * Digit Span sequence
    * UPDRS motor subscore
  * 1 day for all
* Write methods and summarize results
  * 0.5 day
* No deliverables for SA3
  * Recommend an addendum if SA3 is statistically feasible for analysis
* Total budget
  * 9% FTE for 2 months or 18% FTE for 1 month
  * $5075
