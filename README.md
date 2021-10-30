
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

## Example 1: Inference on the accuracy

Consider a situation in which a classification algorithm (e.g., a
logistic regression model, a support vector machine, or a deep learning
model) has been trained and tested to predict the binary label (+1 or
-1) of a set of trials. Further, assume the analysis has been carried
out independently for each subject within a group. The results can then
be summarized in terms of two vectors: The first one, *k*, encodes the
number of correctly classified trials in each subject; the second, *n*,
encodes the total number of trials in each subject. The following steps
outline how to apply the R package to this setting.

### Step 1: Note down observed classification outcomes

We begin by specifying two vectors that fully describe the observed
outcomes of our classification analysis:

``` r
ks <- c(82,  75,  92,  85,  88)
ns <- c(100, 100, 100, 100, 100)
```

This says, for example, that 82 out of 100 trials were classified
correctly in the first subject. There are 5 subjects in total in this
example.

### Step 2: inference

We perform inference by typing:

``` r
stats <- micp.stats(ks, ns)
```

The above code performs full Bayesian inference using an efficient
variational Bayes algorithm. The acronym in micp.stats() is short for
*m*ixed-effects *i*nference on *c*lassification *p*erformance.

\[TBC\]

## Literature

  - *Variational Bayesian mixed-effects inference for classification
    studies.* K.H. Brodersen, J. Daunizeau, C. Mathys, J.R. Chumbley,
    J.M. Buhmann, & K.E. Stephan (2013). *NeuroImage*, 76, 345-361
    ([PDF](https://kaybrodersen.github.io/publications/Brodersen_2013_NeuroImage.pdf)).

  - *Bayesian mixed-effects inference on classification performance in
    hierarchical data sets.* K.H. Brodersen, C. Mathys, J.R. Chumbley,
    J. Daunizeau, C.S. Ong, J.M. Buhmann, & K.E. Stephan (2012).
    *Journal of Machine Learning Research*, 13, 3133-3176
    ([PDF](https://kaybrodersen.github.io/publications/Brodersen_2012_JMLR.pdf)).

  - *Mixed-effects inference on classification performance in group
    studies.* K.H. Brodersen, J.R. Chumbley, C. Mathys, J. Daunizeau,
    J.M. Buhmann, K.E. Stephan (2011) *HBM*, Quebec City, Canada
    ([Poster](https://kaybrodersen.github.io/publications/Brodersen_2011d_HBM.pdf)).

  - *Variational Bayesian mixed-effects inference for classification
    studies.* K.H. Brodersen, J. Daunizeau, C. Mathys, J.R. Chumbley,
    J.M. Buhmann, K.E. Stephan (2012) *HBM*, Beijing, China
    ([Poster](https://kaybrodersen.github.io/publications/Brodersen_2012b_HBM.pdf)).

  - *The balanced accuracy and its posterior distribution.* K.H.
    Brodersen, C.S. Ong, J.M. Buhmann, & K.E. Stephan (2010). *ICPR*,
    3121-3124
    ([PDF](https://kaybrodersen.github.io/publications/Brodersen_2010b_ICPR.pdf)).
