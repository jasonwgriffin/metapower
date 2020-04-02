
<!-- README.md is generated from README.Rmd. Please edit that file -->
metapoweR <img src = 'man/figures/meta_logo.png' align = "right" height = "180" />
==================================================================================

<!-- badges: start -->
<!-- badges: end -->
The primary goal of metapower is to compute statistical power for meta-analyses. Currently, metapower has the following functionality:

Computation of statistical power for:

1.  Summary main effects sizes
2.  Test of homogeneity for between-group variance (for Random-effects models).
3.  Test of homogenity for within-study variance
4.  Categorical moderator analyses

metapower can currently handle the following designs and effect sizes:

1.  Standardized mean difference: Cohen's *d*
2.  Correlation betwen two continous variables: Correlatiion Coefficient (via Fisher's r-to-z transformation)
3.  Probability of Success/Faluire: Odds Ratio

Installation
------------

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jasonwgriffin/metapower")
```

Computing Meta-analytic Power
-----------------------------

Suppose that we plan to meta-analyze all published findings to compute a summary effect size estimate for the group difference between typically developing individuals and individuals with autism on a measure of face recognition ability. In order to plan the study accordingly, we must choose plausible values for the following:

1.  Expected effect size
2.  Expected sample size per group
3.  Expected number of studies

...*for our meta-analysis of face recognition deficits in autism*

1.  We expect that face recognition deficits in ASD are small (Cohen's d = 0.25)
2.  Sample sizes in autism research are generally small. We expect the average group size to be 20.
3.  Face recognition is frequently studied in autism; therefore, we expect to find 50 studies.

To do this with `metapower`, we use the core function `mpower()`

``` r
library(metapower)
my_power <- mpower(effect_size = .25, sample_size = 20, k = 30, es_type = "d")
```

Note that we specify this a random-effects model (`model = "random`). For fixed-effects model, use `model = "fixed"`.

``` r
print(my_power)
#> 
#>  Estimated Meta-Analytic Power 
#> 
#>  Expected Effect Size:              0.25 
#>  Expected Sample Size (per group):  20 
#>  Expected Number of Studies;        30 
#>  Expected between-study sd:         
#> 
#>  Estimated Power: Main effect 
#> 
#>  Fixed-Effects Model                            0.990698 
#>  Random-Effects Model (Low Heterogenity):       0.962092 
#>  Random-Effects Model (Moderate Heterogenity):  0.8621495 
#>  Random-Effects Model (Large Heterogenity):     0.57799 
#> 
#>  Estimated Power: Test of Homogenity 
#> 
#>  Fixed-Efects Model                             NA 
#>  Random-Effects Model (Low Heterogenity):       0.2926194 
#>  Random-Effects Model (Moderate Heterogenity):  0.9782353 
#>  Random-Effects Model (Large Heterogenity):     1
```

The first part of the output shows the expected input values, where the main results are shown in the bottom portion, mainly, `Estimated Power`. Under this set of values, our power to detect a mean difference under a Fixed-Effects model is 99.07%. Furthermore, we can look at the power under a Random-Effects model under various heterogenity levels (e.g., Low, Moderate, Large). For the output regarding `Estimated Power: Test of Homogenity`, please see below

To visualize the power curve for these set of input parameters, use `power_plot()` to generate a `ggobject` that is modifiable and by default, shows 10x as many studies as the user inputs.

``` r
power_plot(my_power)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

For users wanting more flexibility in visualization, the `mpower` object contains a dataframe `$df` containing all data populating the `ggobject`,

``` r
str(my_power$df)
#> 'data.frame':    447 obs. of  9 variables:
#>  $ k_v           : int  2 3 4 5 6 7 8 9 10 11 ...
#>  $ es_v          : num  0.125 0.125 0.125 0.125 0.125 0.125 0.125 0.125 0.125 0.125 ...
#>  $ effect_size   : num  0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 ...
#>  $ n_v           : num  20 20 20 20 20 20 20 20 20 20 ...
#>  $ variance      : num  0.1 0.1 0.1 0.1 0.1 ...
#>  $ fixed_power   : num  0.0864 0.1051 0.1239 0.143 0.1621 ...
#>  $ random_power_s: num  0.0772 0.0911 0.1051 0.1192 0.1334 ...
#>  $ random_power_m: num  0.068 0.0772 0.0864 0.0957 0.1051 ...
#>  $ random_power_l: num  0.059 0.0635 0.068 0.0726 0.0772 ...
```

Power for the Test of Homogeneity
---------------------------------

For Fixed-Effects Model, the test of homogeneity examines whether the amount of variation among effect sizes is greater than that of sampling error alone. TO compute this, you must use the arguement `sd` to assign a value representing the average difference between the effect sizes and the mean effect.

``` r
(homogen_power <- mpower(effect_size = .25, sample_size = 20, k = 30, es_type = "d", sd = 3))
#> 
#>  Estimated Meta-Analytic Power 
#> 
#>  Expected Effect Size:              0.25 
#>  Expected Sample Size (per group):  20 
#>  Expected Number of Studies;        30 
#>  Expected between-study sd:         3 
#> 
#>  Estimated Power: Main effect 
#> 
#>  Fixed-Effects Model                            0.990698 
#>  Random-Effects Model (Low Heterogenity):       0.962092 
#>  Random-Effects Model (Moderate Heterogenity):  0.8621495 
#>  Random-Effects Model (Large Heterogenity):     0.57799 
#> 
#>  Estimated Power: Test of Homogenity 
#> 
#>  Fixed-Efects Model                             0.2966788 
#>  Random-Effects Model (Low Heterogenity):       0.2926194 
#>  Random-Effects Model (Moderate Heterogenity):  0.9782353 
#>  Random-Effects Model (Large Heterogenity):     1
```

For Random-Effects models, the test of homogeneity evaluates whether the variance component, *τ*<sup>2</sup>, is different than zero. metapower automatically uses small, moderate, and large values of *τ*<sup>2</sup>.

We can visualize this across a range with `homogen_power_plot`

``` r
homogen_power_plot(homogen_power)
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

Example 2: Power analysis for moderation analysis (categorical variables)
-------------------------------------------------------------------------

Although researchers are primarily interested in conducting meta-analysis to quantify the main effect of a specific phenomenon, It is very common to evaluate the moderation of this overall effect based on a number of study- and/or sample-related characteristics such as task paradigm or age group (e.g., children, adolescents, adults). To compute the statistical power for the detection of categorical moderators, we use the function `mod_power()` with a few additional arguments, mainly:

1.  Expected number of groups (`n_groups`):
2.  Expected effect size of each group(`effect_sizes`):

...*for our meta-analysis of face recognition deficits in autism*

We may expect that face recognition tasks have larger effect sizes then face perception tasks; therefore, we specify 2 groups and their respective expected effect sizes:

1.  `n_groups = 2`
2.  `effect_sizes = c(.2,.5)`
3.  `sd_within = (1,4)`

``` r
my_mod <- mod_power(n_groups = 2, 
                    effect_sizes = c(.2,.5), 
                    es_type = "d",
                    sample_size = 20,
                    k = 10,
                    sd_within = c(1,4))
```

``` r
print(my_mod)
#> 
#>  Power Analysis for Categorical Moderators: 
#> 
#>  Number of groups:                  2 
#>  Expected Effect Sizes:             0.2 0.5 
#>  Expected Sample Size (per group):  20 
#>  Expected Number of Studies:        10 
#> 
#>  Esimated Power 
#> 
#>  Fixed-Effects Model (Between-Group):                          0.4458675 
#>  Fixed-Effects Model (Within-Group):                           0.7753942 
#>  Random-Effects Model (Between-Group, Small Heterogneity):     0.02504419 
#>  Random-Effects Model (Between-Group, Moderate Heterogneity):  0.02506628 
#>  Random-Effects Model (Between-Group, Large Heterogneity):     0.02513259
```

Given, this set of expected values, we have 44.59% to detect between-group differences under a Fixed-Effects model. As expected, moderator effects are much harder to detect and more studies are required, especially when heterogenity is high.

References
----------

All mathematical calculations are derived from L. V. Hedges & Pigott (2004), Bornstein, Hedges, Higgins, & Rothstein (2009), and T. D. Pigott (2012).

Bornstein, M., Hedges, L. V., Higgins, J., & Rothstein, H. (2009). *Introduction to meta-analysis*. Hoboken, NJ: Wiley.

Hedges, L. V., & Pigott, T. D. (2004). The power of statistical tests for moderators in meta-analysis. *Psychological Methods*, *9*(4), 426–445. <https://doi.org/10.1037/1082-989x.9.4.426>

Pigott, T. D. (2012). *Advances in meta-analysis*. NewYork, NY: Springer.

Issues
------

If you encounter a clear bug, please file a minimal reproducible example on [github](https://github.com/jasonwgriffin/metapower/issues).
