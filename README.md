
<!-- README.md is generated from README.Rmd. Please edit that file -->
metapoweR <img src = 'man/figures/metapower_sticker.png' align = "right" height = "180" />
==========================================================================================

<!-- badges: start -->
<!-- badges: end -->
The primary goal of metapower is to compute statistical power for meta-analyses. Currently, metapower has the following functionality:

Computation of statistical power for:

1.  Summary main effects sizes
2.  Test of homogeneity for between-group variance (for Random-effects models).
3.  Test of homogeneity for within-study variance
4.  Categorical moderator analyses

metapower can currently handle the following designs and effect sizes:

1.  Standardized mean difference: Cohen's *d*
2.  Correlation between two continuous variables: Correlation Coefficient (via Fisher's r-to-z transformation)
3.  Probability of Success/Failure: Odds Ratio

Installation
------------

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jasonwgriffin/metapower")
```

Example
-------

``` r
library(metapower)
my_power <- mpower(effect_size = .25, sample_size = 20, k = 30, es_type = "d")
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
power_plot(my_power)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

See Vignette "Using metapower" for more information

References
----------

All mathematical calculations are derived from L. V. Hedges & Pigott (2004), Bornstein, Hedges, Higgins, & Rothstein (2009), and T. D. Pigott (2012).

Bornstein, M., Hedges, L. V., Higgins, J., & Rothstein, H. (2009). *Introduction to meta-analysis*. Hoboken, NJ: Wiley.

Hedges, L. V., & Pigott, T. D. (2004). The power of statistical tests for moderators in meta-analysis. *Psychological Methods*, *9*(4), 426–445. <https://doi.org/10.1037/1082-989x.9.4.426>

Pigott, T. D. (2012). *Advances in meta-analysis*. NewYork, NY: Springer.

Issues
------

If you encounter a clear bug, please file a minimal reproducible example on [github](https://github.com/jasonwgriffin/metapower/issues).
