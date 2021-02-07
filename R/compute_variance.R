compute_variance <- function(study_size, effect_size, es_type, con_table){

  if(es_type == "d"){

    return(round(((study_size/2+study_size/2)/((study_size/2)*(study_size/2))) + ((effect_size^2)/(2*(study_size/2+study_size/2))),5))

    } else if (es_type == "r"){

      return(1/(study_size-3))

      }else if (es_type == "or"){
        con_table <- data.frame(a = con_table[1],
                                b = con_table[2],
                                c = con_table[3],
                                d = con_table[4])
        return((1/con_table$a)+(1/con_table$b)+(1/con_table$c)+(1/con_table$d))

  }
}
