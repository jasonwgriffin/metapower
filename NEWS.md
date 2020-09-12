
# metapower 0.1.1

## Update

### Changes
* For random-effects models, mpower() now uses a different formula to account for uncertainty in tau2 (see Jackson & Turner, 2017)
* `mod_power()` was changed to subgroup_power()
* The subgroup_power() has slightly different arguments to allow more flexibility, especially for Odds Ratio
* All plot functions were changes to have a preceding plot_. For example: `plot_mpower()`; `plot_homogen_power()`; `plot_subgroup_power()`

### Additions

* Added subgroup_power(), which computes power to detect differences in subgroups among studies (i.e., Men vs Women)
* Added a plotting function to subgroup_power() called plot_subgroup_power
* A fully functional shiny application is now available (https://jason-griffin.shinyapps.io/shiny_metapower/)


# metapower 0.1.0

## New Release

### Primary functions

* mpower(): Compute statistical power for meta-analysis
* mod_power(): Compute power for categorical moderator meta-analytic models
* power_plot(): Visualize a range of power curves
* homogen_power_plot(): visualize a range of power curves for the test of homogeneity

