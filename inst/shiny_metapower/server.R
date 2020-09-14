library(shiny)

server <- function(input, output) {

  #Power Analysis

  ## Cohen's d
  ## Summary Effect Size
  output$d_plot <- renderPlot({
    es_type <- "d"
    effect_size <- input$d_es
    k <- input$d_k
    sample_size <- input$d_n
    p <- input$d_p
    test_type <- input$d_test_type
    metapower::plot_mpower(mpower(effect_size, sample_size, k, es_type, test_type, p))
  })
  output$d_summary <- renderPrint({
    es_type <- "d"
    effect_size <- input$d_es
    k <- input$d_k
    sample_size <- input$d_n
    p <- input$d_p
    test_type <- input$d_test_type
    print(mpower(effect_size, sample_size, k, es_type, test_type, p))
  })

  ## Homogen Power
  output$homogen_d_plot <- renderPlot({
    es_type <- "d"
    effect_size <- input$homogen_d_es
    k <- input$homogen_d_k
    sample_size <- input$homogen_d_n
    p <- input$homogen_d_p
    metapower::plot_homogen_power(homogen_power(effect_size, sample_size, k, es_type, p))
  })
  output$homogen_d_summary <- renderPrint({
    es_type <- "d"
    effect_size <- input$homogen_d_es
    k <- input$homogen_d_k
    sample_size <- input$homogen_d_n
    p <- input$homogen_d_p
    print(homogen_power(effect_size, sample_size, k, es_type, p))
  })

  ## SUbgroup analysis

  ##output text for user

  output$sg_d_k <- renderText({
    paste("Number of studies per group:", input$subgroup_d_k/input$subgroup_d_n_groups)
  })
  output$sg_d_n <- renderText({
    paste("Number of participants per group:", input$subgroup_d_n/input$subgroup_d_n_groups)
  })


  output$subgroup_d_plot <- renderPlot({
    es_type <- "d"
    n_groups <- input$subgroup_d_n_groups
    ## gather effect sizes
    effect_sizes <- c(input$sg1_d_es,input$sg2_d_es)

    if(!is.na(input$sg3_d_es))
      effect_sizes <- c(effect_sizes,input$sg3_d_es)
    if(!is.na(input$sg4_d_es))
      effect_sizes <- c(effect_sizes,input$sg4_d_es)

    k <- input$subgroup_d_k
    sample_size <- input$subgroup_d_n
    p <- input$subgroup_d_p
    metapower::plot_subgroup_power(subgroup_power(n_groups, effect_sizes, sample_size, k, es_type, p))
  })

  output$subgroup_d_summary <- renderPrint({
    es_type <- "d"
    n_groups <- input$subgroup_d_n_groups
    effect_sizes <- c(input$sg1_d_es,input$sg2_d_es)

    if(!is.na(input$sg3_d_es))
      effect_sizes <- c(effect_sizes,input$sg3_d_es)
    if(!is.na(input$sg4_d_es))
      effect_sizes <- c(effect_sizes,input$sg4_d_es)
    k <- input$subgroup_d_k
    sample_size <- input$subgroup_d_n
    p <- input$subgroup_d_p
    print(subgroup_power(n_groups, effect_sizes, sample_size, k, es_type, p))
  })

  ## Moderator Analysis

  ##output text for user

  output$md_d_k <- renderText({
    paste("Number of studies per group:", input$mod_d_k/input$mod_d_n_groups)
  })
  output$md_d_n <- renderText({
    paste("Number of participants per group:", input$mod_d_n/input$mod_d_n_groups)
  })


  output$mod_d_plot <- renderPlot({
    es_type <- "d"
    n_groups <- input$mod_d_n_groups
    ## gather effect sizes
    effect_sizes <- c(input$mod_d_es1,input$mod_d_es2)

    if(!is.na(input$mod_d_es3))
      effect_sizes <- c(effect_sizes,input$mod_d_es3)
    if(!is.na(input$mod_d_es4))
      effect_sizes <- c(effect_sizes,input$mod_d_es4)

    k <- input$mod_d_k
    sample_size <- input$mod_d_n
    p <- input$mod_d_p
    metapower::plot_mod_power(mod_power(n_groups, effect_sizes, sample_size, k, es_type, p))
  })

  output$mod_d_summary <- renderPrint({
    es_type <- "d"
    n_groups <- input$mod_d_n_groups
    effect_sizes <- c(input$mod_d_es1,input$mod_d_es2)

    if(!is.na(input$mod_d_es3))
      effect_sizes <- c(effect_sizes,input$mod_d_es3)
    if(!is.na(input$mod_d_es4))
      effect_sizes <- c(effect_sizes,input$mod_d_es4)
    k <- input$mod_d_k
    sample_size <- input$mod_d_n
    p <- input$mod_d_p
    print(mod_power(n_groups, effect_sizes, sample_size, k, es_type, p))
  })





  ## Correlation
  # Summary Effect Size
  output$c_plot <- renderPlot({
    es_type <- "r"
    effect_size <- input$c_es
    k <- input$c_k
    sample_size <- input$c_n
    p <- input$c_p
    test_type <- input$c_test_type
    metapower::plot_mpower(mpower(effect_size, sample_size, k, es_type, test_type, p))
  })
  output$c_summary <- renderPrint({
    es_type <- "r"
    effect_size <- input$c_es
    k <- input$c_k
    sample_size <- input$c_n
    p <- input$c_p
    test_type <- input$c_test_type
    print(mpower(effect_size, sample_size, k, es_type, test_type, p))
  })

  ## Homogen Power
  output$homogen_c_plot <- renderPlot({
    es_type <- "r"
    effect_size <- input$homogen_c_es
    k <- input$homogen_c_k
    sample_size <- input$homogen_c_n
    p <- input$homogen_c_p
    metapower::plot_homogen_power(homogen_power(effect_size, sample_size, k, es_type, p))
  })
  output$homogen_c_summary <- renderPrint({
    es_type <- "r"
    effect_size <- input$homogen_c_es
    k <- input$homogen_c_k
    sample_size <- input$homogen_c_n
    p <- input$homogen_c_p
    print(homogen_power(effect_size, sample_size, k, es_type, p))
  })

  ## SUbgroup analysis
  output$sg_c_k <- renderText({
    paste("Number of studies per group:", input$subgroup_c_k/input$subgroup_c_n_groups)
  })
  output$sg_c_n <- renderText({
    paste("Number of participants per group:", input$subgroup_c_n/input$subgroup_c_n_groups)
  })

  output$subgroup_c_plot <- renderPlot({
    es_type <- "r"
    n_groups <- input$subgroup_c_n_groups
    ## gather effect sizes
    effect_sizes <- c(input$subgroup_c_es1,input$subgroup_c_es2)

    if(!is.na(input$subgroup_c_es3))
      effect_sizes <- c(effect_sizes,input$subgroup_c_es3)
    if(!is.na(input$subgroup_c_es4))
      effect_sizes <- c(effect_sizes,input$subgroup_c_es4)

    k <- input$subgroup_c_k
    sample_size <- input$subgroup_c_n
    p <- input$subgroup_c_p
    metapower::plot_subgroup_power(subgroup_power(n_groups, effect_sizes, sample_size, k, es_type, p))
  })

  output$subgroup_c_summary <- renderPrint({
    es_type <- "r"
    n_groups <- input$subgroup_c_n_groups
    effect_sizes <- c(input$subgroup_c_es1,input$subgroup_c_es2)

    if(!is.na(input$subgroup_c_es3))
      effect_sizes <- c(effect_sizes,input$subgroup_c_es3)
    if(!is.na(input$subgroup_c_es4))
      effect_sizes <- c(effect_sizes,input$subgroup_c_es4)
    k <- input$subgroup_c_k
    sample_size <- input$subgroup_c_n
    p <- input$subgroup_c_p
    print(subgroup_power(n_groups, effect_sizes, sample_size, k, es_type, p))
  })

  ## Moderator Analysis
  output$md_c_k <- renderText({
    paste("Number of studies per group:", input$mod_c_k/input$mod_c_n_groups)
  })

  output$mod_c_plot <- renderPlot({
    es_type <- "r"
    n_groups <- input$mod_c_n_groups
    ## gather effect sizes
    effect_sizes <- c(input$mod_c_es1,input$mod_c_es2)

    if(!is.na(input$mod_c_es3))
      effect_sizes <- c(effect_sizes,input$mod_c_es3)
    if(!is.na(input$mod_c_es4))
      effect_sizes <- c(effect_sizes,input$mod_c_es4)

    k <- input$mod_c_k
    sample_size <- input$mod_c_n
    p <- input$mod_c_p
    metapower::plot_mod_power(mod_power(n_groups, effect_sizes, sample_size, k, es_type, p))
  })

  output$mod_c_summary <- renderPrint({
    es_type <- "r"
    n_groups <- input$mod_c_n_groups
    effect_sizes <- c(input$mod_c_es1,input$mod_c_es2)

    if(!is.na(input$mod_c_es3))
      effect_sizes <- c(effect_sizes,input$mod_c_es3)
    if(!is.na(input$mod_c_es4))
      effect_sizes <- c(effect_sizes,input$mod_c_es4)
    k <- input$mod_c_k
    sample_size <- input$mod_c_n
    p <- input$mod_c_p
    print(mod_power(n_groups, effect_sizes, sample_size, k, es_type, p))
  })

  ## Odds Ratio
  ## Summary Effect Size
  output$or_plot <- renderPlot({
    es_type <- "or"
    effect_size <- input$or_es
    k <- input$or_k
    sample_size <- input$or_n
    p <- input$or_p
    test_type <- input$or_test_type
    con_table <- c(input$or_a, input$or_b, input$or_c, input$or_d)
    metapower::plot_mpower(mpower(NULL,sample_size, k, es_type, test_type, p, con_table))
  })
  output$or_summary <- renderPrint({
    es_type <- "or"
    effect_size <- input$or_es
    k <- input$or_k
    sample_size <- input$or_n
    p <- input$or_p
    test_type <- input$or_test_type
    con_table <- c(input$or_a, input$or_b, input$or_c, input$or_d)
    print(mpower(NULL,sample_size, k, es_type, test_type, p, con_table))
  })

  ## Homogen Power
  output$homogen_or_plot <- renderPlot({
    es_type <- "or"
    effect_size <- input$homogen_or_es
    k <- input$homogen_or_k
    sample_size <- input$homogen_or_n
    p <- input$homogen_or_p
    con_table <- c(input$homogen_or_a, input$homogen_or_b, input$homogen_or_c, input$homogen_or_d)
    metapower::plot_homogen_power(homogen_power(NULL,sample_size, k, es_type, p, con_table))
  })
  output$homogen_or_summary <- renderPrint({
    es_type <- "or"
    effect_size <- input$homogen_or_es
    k <- input$homogen_or_k
    sample_size <- input$homogen_or_n
    p <- input$homogen_or_p
    con_table <- c(input$homogen_or_a, input$homogen_or_b, input$homogen_or_c, input$homogen_or_d)
    print(homogen_power(NULL,sample_size, k, es_type, p, con_table))
  })

  ## Subgroup analysis
  output$sg_or_k <- renderText({
    paste("Number of studies per group:", input$subgroup_or_k/input$subgroup_or_n_groups)
  })

  output$subgroup_or_plot <- renderPlot({
    es_type <- "or"
    n_groups <- input$subgroup_or_n_groups
    k <- input$subgroup_or_k
    sample_size <- input$subgroup_or_n
    p <- input$subgroup_or_p
    con_table <- list(group1 = c(input$sg_1_or_a,input$sg_1_or_b,input$sg_1_or_c,input$sg_1_or_d),
                      group2 = c(input$sg_2_or_a,input$sg_2_or_b,input$sg_2_or_c,input$sg_2_or_d))

    ## check if more than 2 subgroups, if so add them to list
    if(input$sg3_or_name != "")
      con_table <- c(con_table, list(group3 = c(input$sg_3_or_a,input$sg_3_or_b,input$sg_3_or_c,input$sg_3_or_d)))
    if(input$sg4_or_name != "")
      con_table <- c(con_table, list(group4 = c(input$sg_4_or_a,input$sg_4_or_b,input$sg_4_or_c,input$sg_4_or_d)))

    metapower::plot_subgroup_power(subgroup_power(n_groups, NULL, sample_size, k, es_type, p, con_table))
  })

  output$subgroup_or_summary <- renderPrint({
    es_type <- "or"
    n_groups <- input$subgroup_or_n_groups
    k <- input$subgroup_or_k
    sample_size <- input$subgroup_or_n
    p <- input$subgroup_or_p
    con_table <- list(group1 = c(input$sg_1_or_a,input$sg_1_or_b,input$sg_1_or_c,input$sg_1_or_d),
                      group2 = c(input$sg_2_or_a,input$sg_2_or_b,input$sg_2_or_c,input$sg_2_or_d))

    ## check if more than 2 subgroups, if so add them to list
    if(input$sg3_or_name != "")
      con_table <- c(con_table, list(group3 = c(input$sg_3_or_a,input$sg_3_or_b,input$sg_3_or_c,input$sg_3_or_d)))
    if(input$sg4_or_name != "")
      con_table <- c(con_table, list(group4 = c(input$sg_4_or_a,input$sg_4_or_b,input$sg_4_or_c,input$sg_4_or_d)))

    print(subgroup_power(n_groups, NULL, sample_size, k, es_type, p, con_table))
  })

  ## Moderator Analysis
  output$md_or_k <- renderText({
    paste("Number of studies per group:", input$mod_or_k/input$mod_or_n_groups)
  })

  output$mod_or_plot <- renderPlot({
    es_type <- "or"
    n_groups <- input$mod_or_n_groups
    k <- input$mod_or_k
    sample_size <- input$mod_or_n
    p <- input$mod_or_p
    con_table <- list(group1 = c(input$md_1_or_a,input$md_1_or_b,input$md_1_or_c,input$md_1_or_d),
                      group2 = c(input$md_2_or_a,input$md_2_or_b,input$md_2_or_c,input$md_2_or_d))

    ## check if more than 2 mods, if so add them to list
    if(input$md3_or_name != "")
      con_table <- c(con_table, list(group3 = c(input$md_3_or_a,input$md_3_or_b,input$md_3_or_c,input$md_3_or_d)))
    if(input$md4_or_name != "")
      con_table <- c(con_table, list(group4 = c(input$md_4_or_a,input$md_4_or_b,input$md_4_or_c,input$md_4_or_d)))

    metapower::plot_mod_power(mod_power(n_groups, NULL, sample_size, k, es_type, p, con_table))
  })

  output$mod_or_summary <- renderPrint({
    es_type <- "or"
    n_groups <- input$mod_or_n_groups
    k <- input$mod_or_k
    sample_size <- input$mod_or_n
    p <- input$mod_or_p
    con_table <- list(group1 = c(input$md_1_or_a,input$md_1_or_b,input$md_1_or_c,input$md_1_or_d),
                      group2 = c(input$md_2_or_a,input$md_2_or_b,input$md_2_or_c,input$md_2_or_d))

    ## check if more than 2 mods, if so add them to list
    if(input$md3_or_name != "")
      con_table <- c(con_table, list(group3 = c(input$md_3_or_a,input$md_3_or_b,input$md_3_or_c,input$md_3_or_d)))
    if(input$md4_or_name != "")
      con_table <- c(con_table, list(group4 = c(input$md_4_or_a,input$md_4_or_b,input$md_4_or_c,input$md_4_or_d)))

    print(mod_power(n_groups, NULL, sample_size, k, es_type, p, con_table))
  })

}
