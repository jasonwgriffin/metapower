subgroup_power_integrity <- function(n_groups, effect_sizes, sample_size, k, es_type, p, con_table){

# Argument Integrity Checks
es_type_options <- c("d","r", "or")

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


#sample_size
if(missing(sample_size))
  stop("Need to specify expected sample size")
if(!(is.numeric(sample_size)))
  stop("sample_size must be numeric")
if(length(sample_size) > 1)
  stop("sample_size must be a single number")
if(sample_size < 1)
  stop("sample_size must be greater than 0")
if((sample_size/n_groups)%%1!=0)
  stop("sample size must be a multiple of n_groups")

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

## es_type
if(missing(es_type))
  stop("Need to specify effect size as 'd', 'r', or 'or'")
if(!(es_type %in% es_type_options))
  stop("Need to specify effect size as 'd', 'r', or 'or'")

## effect size
# d

if(es_type == 'd' & length(effect_sizes) != n_groups)
  stop("The number of of effect sizes should match the number of groups")
if(es_type == 'd' & !is.numeric(effect_sizes))
  stop("Effect sizes must be  numeric")
if(es_type == 'd' & any(effect_sizes > 10))
  warning("Are you sure at least one effect size is >10?")
if(es_type == 'd' & missing(effect_sizes))
  stop("Need to specify expected effect sizes per group")



# Correlation
if(es_type == 'r' & any(effect_sizes > 1))
  stop("Correlations cannot be above 1")
if(es_type == 'r' & any(effect_sizes < 0))
  stop("Correlations must be above 0")
if(es_type == 'r' & missing(effect_sizes))
  stop("Need to specify expected effect sizes per group")
if(es_type == 'r' & length(effect_sizes) != n_groups)
  stop("The number of of effect sizes should match the number of groups")

# Odds Ratio
if(es_type == 'or' & !is.null(effect_sizes))
  stop("For Odds Ratio, only enter the 2x2 contingency table. Remove effect_size argument")
if(es_type == "or" & missing(con_table))
  stop("For Odds Ratio, must enter contingency tables (con_table) for each group in a list. see documentation on how to specify this argument")
if(es_type == "or" & !missing(con_table)){
  if(!is.list(con_table))
    stop("con_table should be input as a list with each element reflect the group 2x2 tables")
  if(length(con_table) == 1)
    stop("Only 1 group 2x2 table is specified")
  if(length(con_table) != n_groups)
    stop("con_table must reflect a list of 2x2 contingency tables for each group. See documentation on how to specify this")
  for (i in 1:length(con_table)){
    if(!is.numeric(con_table[[i]]))
      stop("Each element of con_table should be numeric")
  }
  for (i in 1:length(con_table)){
    if(sample_size/n_groups != sum(con_table[[i]]))
      stop("Each 2x2 table should sum to the total sample size/number of groups")
    }
  }
}
