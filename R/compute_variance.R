compute_variance <- function(sample_size, effect_size, es_type, con_table){

  if(es_type == "d"){

    return(round(((sample_size+sample_size)/((sample_size)*(sample_size))) + ((effect_size^2)/(2*(sample_size+sample_size))),5))

    } else if (es_type == "Correlation"){

      return(1/(sample_size-3))

      }else if (es_type == "OR"){
        con_table <- data.frame(a = con_table[1],
                                b = con_table[2],
                                c = con_table[3],
                                d = con_table[4])
        return((1/con_table$a)+(1/con_table$b)+(1/con_table$c)+(1/con_table$d))

  }
}

