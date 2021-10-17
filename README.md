
<!-- README.md is generated from README.Rmd. Please edit that file -->

# micp

<!-- badges: start -->

[![R-CMD-check](https://github.com/kaybrodersen/micp/workflows/R-CMD-check/badge.svg)](https://github.com/kaybrodersen/micp/actions)
<!-- badges: end -->

*The `micp` package implements variational Bayesian inference for the
classification accuracy in hierarchical data.*

## Introduction

Classification algorithms are often used in a hierarchical setting,
where a classifier is trained and tested on individual datasets which
are themselves sampled from a group. Examples of this sort of analysis
are ubiquitous and are common in domains as varied as spam detection,
brain-machine interfaces, and neuroimaging.

This R package provides answers to the questions of statistical
inference that arise in all of these settings. It implements models that
account for both within-subjects (fixed-effects) and between-subjects
(random-effects) variance components and thus provide mixed-effects
inference.

The package is extremely easy to use and requires no prerequisites other
than R.

## Installation

You can install the package from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("kaybrodersen/micp")
```
