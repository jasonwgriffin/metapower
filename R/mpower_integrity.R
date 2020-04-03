mpower_integrity <- function(effect_size, sample_size, k, es_type, test_type, p, sd, con_table){

test_type_options <- c("one-tailed", "two-tailed")
es_type_options <- c("d","Correlation", "OR")


## Effect size integrity checks
if(missing(effect_size))
  stop("Need to specify expected effect size")
if(!(is.numeric(effect_size)))
  stop("effect_size must be numeric")
if(length(effect_size) > 1)
  stop("effect_size must be a single number")
if(effect_size < 0)
  stop("effect_size must be greater than zero")

## sample_size integrity checks
if(missing(sample_size))
  stop("Need to specify expected sample size")
if(!(is.numeric(sample_size)))
  stop("sample_size must be numeric")
if(length(sample_size) > 1)
  stop("sample_size must be a single number")
if(sample_size < 1)
  stop("sample_size must be greater than 0")

## num studies integrity checks
if(missing(k))
  stop("Need to specify expected number of studies")
if(!(is.numeric(k)))
  stop("k must be numeric")
if(length(k) > 1)
  stop("k must be a single number")
if(k < 2)
  stop("k must be greater than 1")

## es_type
if(missing(es_type))
  stop("Need to specify effect size as 'd', 'Correlation', or 'OR'")
if(!(es_type %in% es_type_options))
  stop("Need to specify effect size as 'd', 'Correlation', or 'OR'")
## effect size
if(es_type == 'd' & effect_size > 10)
  warning("Are you sure effect size is >10?")
if(es_type == 'Correlation' & effect_size > 1)
  stop("Correlation cannot be above 1")
if(es_type == 'Correlation' & effect_size < 0)
  stop("Correlation must be above 0")

if(es_type == 'OR' & effect_size <= 1)
  stop("Odds ratio should be above 1")
if(es_type == "OR" & missing(con_table))
  stop("For Odds Ratio, must enter contigency table (cont_table)")
if(es_type == "OR" & !missing(con_table)){
  if(length(con_table) != 4)
    stop("con_table must reflect a 2x2 contingency table with the form c(a,b,c,d). see documentation")
  if(sample_size != sum(con_table))
    stop("Entered sample size should equal the sum of the contigency table")
  if(round(effect_size,1) != round((con_table[1]*con_table[4])/(con_table[2]*con_table[3]),1))
    stop("con_table does not match effect size: OR = ad/bc")
}
## test_type errors
if(!(test_type %in% test_type_options))
  stop("Need to specify two-tailed or one-tailed")
}
