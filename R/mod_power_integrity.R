mod_power_integrity <- function(effect_size, sample_size, k, es_type, test_type, p, con_table, sd_within){

# Argument Integrity Checks
test_type_options <- c("one-tailed", "two-tailed")
es_type_options <- c("d","Correlation", "OR")

# n_groups
if(missing(n_groups))
  stop("Must specify number of groups: n_groups")
if(!(is.numeric(n_groups)))
  stop("n_groups must be a single integer")
if(length(n_groups) > 1)
  stop("n_groups must be a single integer")
if(n_groups < 2)
  stop("number of groups must be at least 2")

#effect_sizes
if(missing(effect_sizes))
  stop("Need to specify expected effect sizes per group")
if(length(effect_sizes) != n_groups)
  stop("The number of of effect sizes should match the number of groups")

#sample_size
if(missing(sample_size))
  stop("Need to specify expected sample size")
if(!(is.numeric(sample_size)))
  stop("sample_size must be numeric")
if(length(sample_size) > 1)
  stop("sample_size must be a single number")
if(sample_size < 1)
  stop("sample_size must be greater than 0")

# Number of Studies
if(missing(k))
  stop("Need to specify expected number of studies")
if(!(is.numeric(k)))
  stop("k must be numeric")
if(length(k) > 1)
  stop("k must be a single number")
if(k < 2)
  stop("k must be greater than 1")
if(k <= n_groups)
  stop("Number of studies must be larger than n_groups")
if((k/n_groups)%%1!=0)
  stop("Number of studies must be a multiple of n_groups")

## es_type
if(missing(es_type))
  stop("Need to specify effect size as 'd', 'Correlation', or 'OR'")
if(!(es_type %in% es_type_options))
  stop("Need to specify effect size as 'd', 'Correlation', or 'OR'")

## effect size
# d
if(es_type == 'd' & any(effect_sizes > 10))
  warning("Are you sure at least one effect size is >10?")
# Correlation
if(es_type == 'Correlation' & any(effect_sizes > 1))
  stop("Correlations cannot be above 1")
if(es_type == 'Correlation' & any(effect_sizes < 0))
  stop("Correlations must be above 0")
# Odds Ratio
if(es_type == 'OR' & any(effect_sizes <= 1))
  stop("Odds ratio should be above 1")
if(es_type == "OR" & missing(con_table))
  stop("For Odds Ratio, must enter contigency table (cont_table)")
if(es_type == "OR" & !missing(con_table)){
  if(length(con_table) != 4)
    stop("con_table must reflect a 2x2 contingency table with the form c(a,b,c,d). see documentation")
  if(sample_size != sum(con_table))
    stop("Entered sample size should equal the sum of the contigency table")
}

## test_type errors
if(!(test_type %in% test_type_options))
  stop("Need to specify two-tailed or one-tailed")

##sd_within
if(!missing(sd_within)){
  if(length(sd_within) != n_groups)
    stop("The number of of effect sizes should match the number within-group standard deviations")
  }
}
