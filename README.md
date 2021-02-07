
<!-- README.md is generated from README.Rmd. Please edit that file -->

# metapoweR <img src = 'man/figures/metapower_sticker.png' align = "right" height = "180" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/metapower)](https://CRAN.R-project.org/package=metapower)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)

[![](http://cranlogs.r-pkg.org/badges/grand-total/metapower?color=blue)](https://cran.r-project.org/package=metapower)

<!-- badges: end -->

The primary goal of metapower is to compute statistical power for
meta-analyses. Currently, metapower has the following functionality:

Computation of statistical power for:

1.  Summary main effects sizes
2.  Test of homogeneity for between-group variance (for Random-effects
    models).
3.  Test of homogeneity for within-study variance
4.  Subgroup Analyses
5.  Moderator Analysis

metapower can currently handle the following designs and effect sizes:

4.  Standardized mean difference: Cohen’s *d*
5.  Correlation between two continuous variables: Correlation
    Coefficient (via Fisher’s r-to-z transformation)
6.  Probability of Success/Failure: Odds Ratio

## Installation

You can install the released version of metapower from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("metapower")
```

And the development version from
[GitHub](https://github.com/jasonwgriffin/metapower) with:

``` r
# install.packages("devtools")
devtools::install_github("jasonwgriffin/metapower")
```

## Shiny Application

Check out the simple and easy to use [shiny
application](https://jason-griffin.shinyapps.io/shiny_metapower/)

## Example

``` r
library(metapower)
my_power <- mpower(effect_size = .3, study_size = 20, k = 10, i2 = .50, es_type = "d")
print(my_power)
#> 
#>  Power Analysis for Meta-analysis 
#> 
#>  Effect Size Metric:                d 
#>  Expected Effect Size:              0.3 
#>  Expected Study Size:               20 
#>  Expected Number of Studies:        10 
#> 
#>  Estimated Power: Mean Effect Size 
#> 
#>  Fixed-Effects Model                0.5594533 
#>  Random-Effects Model (i2 = 50%):   0.3454424
plot_mpower(my_power)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

See Vignette “Using metapower” for more information..

## References

All mathematical calculations are derived from Hedges & Pigott (2004),
Bornstein, Hedges, Higgins, & Rothstein (2009),Pigott (2012), Jackson &
Turner (2017).

<div id="refs">

<div id="ref-bornstein2009">

Bornstein, M., Hedges, L. V., Higgins, J. P., & Rothstein, H. R. (2009).
*Introduction to meta-analysis*. Hoboken, NJ: Wiley.

</div>

<div id="ref-hedges2004">

Hedges, L. V., & Pigott, T. D. (2004). The power of statistical tests
for moderators in meta-analysis. *Psychological Methods*, *9*(4),
426–445. <https://doi.org/10.1037/1082-989x.9.4.426>

</div>

<div id="ref-jackson2017">

Jackson, D., & Turner, R. (2017). Power analysis for random‐effects
meta-analysis. *Research Synthesis Methods*, *8*(3), 290–302.
<https://doi.org/10.1002/jrsm.1240>

</div>

<div id="ref-pigott2012">

Pigott, T. D. (2012). *Advances in meta-analysis*. NewYork, NY:
Springer.

</div>

</div>

## Issues

If you encounter a clear bug, please file a minimal reproducible example
on [github](https://github.com/jasonwgriffin/metapower/issues).
