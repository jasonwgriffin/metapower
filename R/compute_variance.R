
compute_variance <- function(sample_size, effect_size){
  return(
    round(((sample_size+sample_size)/((sample_size)*(sample_size))) + ((effect_size^2)/(2*(sample_size+sample_size))),5))
}
