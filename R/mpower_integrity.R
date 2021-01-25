mpower_integrity <- function(effect_size, study_size, k, i2, es_type, test_type, p, con_table){

test_type_options <- c("one-tailed", "two-tailed")
es_type_options <- c("d","r", "or")

## study_size integrity checks
if(missing(study_size))
  stop("Need to specify expected sample size")
if(!(is.numeric(study_size)))
  stop("study_size must be numeric")
if(length(study_size) > 1)
  stop("study_size must be a single number")
if(study_size < 1)
  stop("study_size must be greater than 0")

## num studies integrity checks
if(missing(k))
  stop("Need to specify expected number of studies")
if(!(is.numeric(k)))
  stop("k must be numeric")
if(length(k) > 1)
  stop("k must be a single number")
if(k < 2)
  stop("k must be greater than 1")

## i2 - Heterogeneity

if(missing(i2))
  stop("Need to specify heterogeneity(i2); Small = .25, moderatoe = .50, Large = .75")
if(i2 > .9999)
  stop("i2 cannot be greater than 1")
if(i2 < 0)
  stop("i2 cannot be less than 0")

## es_type
if(missing(es_type))
  stop("Need to specify effect size as 'd', 'r', or 'or'")
if(!(es_type %in% es_type_options))
  stop("Need to specify effect size as 'd', 'r', or 'or'")

## effect size d
if(es_type == 'd'){
  if(is.null(effect_size))
    stop("Need to specify expected effect size")
  if(!(is.numeric(effect_size)))
    stop("effect_size must be numeric")
  if(length(effect_size) > 1)
    stop("effect_size must be a single number")
  if(effect_size < 0)
    stop("effect_size must be greater than zero")
  if(effect_size > 10)
    warning("Are you sure effect size is >10?")
}
## Correlation
if(es_type == 'r'){
  if(is.null(effect_size))
    stop("Need to specify expected effect size")
  if(!(is.numeric(effect_size)))
    stop("effect_size must be numeric")
  if(length(effect_size) > 1)
    stop("effect_size must be a single number")
  if(effect_size > 1)
    stop("Correlation cannot be above 1")
  if(effect_size < 0)
    stop("Correlation must be above 0")
}
# Odds Ratio
if(es_type == 'or'){
  if(!is.null(effect_size))
    stop("Do not enter an effect size for Odds Ratio. Instead, Enter a 2x2 contingency table")
  if(missing(con_table))
    stop("For Odds Ratio, must enter contigency table (con_table)")
  if(!missing(con_table)){
    if(length(con_table) != 4)
      stop("con_table must reflect a 2x2 contingency table with the form c(a,b,c,d). see documentation")
    if(study_size != sum(con_table))
      stop("Entered sample size should equal the sum of the contigency table")
  }
}
## test_type errors
if(!(test_type %in% test_type_options))
  stop("Need to specify two-tailed or one-tailed")
}
