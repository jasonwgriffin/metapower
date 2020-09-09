homogen_power_integrity <- function(effect_size, sample_size, k, es_type, p, con_table){

  es_type_options <- c("d","r", "or")

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
      if(sample_size != sum(con_table))
        stop("Entered sample size should equal the sum of the contigency table")
    }
  }
}
