
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
#> Skipping install of 'micp' from a github remote, the SHA1 (ed5eb079) has not changed since last install.
#>   Use `force = TRUE` to force installation
```

Then load the package with:

``` r
library(micp)
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

We can obtain a summary of the results using:

``` r
micp.stats(ks, ns)
#> Variational Bayesian mixed-effects inference on classification
#> accuracy
#> 
#> Population inference
#>   posterior mean accuracy:    0.82 (p = 0)
#>   posterior 95% interval:     [0.72, 0.9]
#> 
#> Subject-specific inference
#>   posterior logit means:      1.52, 1.14, 2.27, 1.71, 1.93
#>   posterior logit precisions: 16.62, 20.25, 10.39, 14.86, 13
#> 
#> Bayesian model comparison
#>   free energy F: -20.28
```

This tells us, for example, that the population mean accuracy was 82%,
with a 95% central credible interval of 72% … 90%. This is the interval
in which we place 95% of our posterior belief, and we could use it for
plotting error bars on the classification performance. The output also
contains an approximation to the free energy *F*, which is a lower bound
to the marginal likelihood (or model evidence) and can be used to
compare the current model to future alternatives.

We can inspect the function output in more detail using:

``` r
stats <- micp.stats(ks, ns)
names(stats)
#> [1] "mu"    "p"     "ci"    "q"     "model"
stats$p
#> [1] 2.755991e-07
```

With an infraliminal probability of *p* ≈ 2.8 × 10<sup>−7</sup>, we are
supremely confident that the classifier operated above chance at the
group level. Put differently, the fact that p is approximately 0 means
that we are approximately 100% sure that the population mean accuracy is
above chance.

To display all details about the function micp.stats(), type:

``` r
?micp.stats
```

## Example 2: Inference on the balanced accuracy

In many real-world problems, the data used for classification are not
perfectly balanced. This means that there are more examples from one
class then from the other. Denoting the two classes as the *positive*
and the *negative* class, respectively, there might for instance be more
positive than negative examples in the data. When the data are
imbalanced, the accuracy is a misleading performance measure and should
be replaced by the balanced accuracy.

\[TBC\]

## Literature

-   *Variational Bayesian mixed-effects inference for classification
    studies.* K.H. Brodersen, J. Daunizeau, C. Mathys, J.R. Chumbley,
    J.M. Buhmann, & K.E. Stephan (2013). *NeuroImage*, 76, 345-361
    ([PDF](https://kaybrodersen.github.io/publications/Brodersen_2013_NeuroImage.pdf)).

-   *Bayesian mixed-effects inference on classification performance in
    hierarchical data sets.* K.H. Brodersen, C. Mathys, J.R.
    Chumbley, J. Daunizeau, C.S. Ong, J.M. Buhmann, & K.E. Stephan
    (2012). *Journal of Machine Learning Research*, 13, 3133-3176
    ([PDF](https://kaybrodersen.github.io/publications/Brodersen_2012_JMLR.pdf)).

-   *Mixed-effects inference on classification performance in group
    studies.* K.H. Brodersen, J.R. Chumbley, C. Mathys, J. Daunizeau,
    J.M. Buhmann, K.E. Stephan (2011) *HBM*, Quebec City, Canada
    ([Poster](https://kaybrodersen.github.io/publications/Brodersen_2011d_HBM.pdf)).

-   *Variational Bayesian mixed-effects inference for classification
    studies.* K.H. Brodersen, J. Daunizeau, C. Mathys, J.R. Chumbley,
    J.M. Buhmann, K.E. Stephan (2012) *HBM*, Beijing, China
    ([Poster](https://kaybrodersen.github.io/publications/Brodersen_2012b_HBM.pdf)).

-   *The balanced accuracy and its posterior distribution.* K.H.
    Brodersen, C.S. Ong, J.M. Buhmann, & K.E. Stephan (2010). *ICPR*,
    3121-3124
    ([PDF](https://kaybrodersen.github.io/publications/Brodersen_2010b_ICPR.pdf)).
