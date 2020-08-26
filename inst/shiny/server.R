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
    test_type <- input$homogen_d_test_type
    metapower::plot_homogen_power(homogen_power(effect_size, sample_size, k, es_type, test_type, p))
  })
  output$homogen_d_summary <- renderPrint({
    es_type <- "d"
    effect_size <- input$homogen_d_es
    k <- input$homogen_d_k
    sample_size <- input$homogen_d_n
    p <- input$homogen_d_p
    test_type <- input$homogen_d_test_type
    print(homogen_power(effect_size, sample_size, k, es_type, test_type, p))
  })



  ## Correlation
  # Summary Effect Size
  output$c_plot <- renderPlot({
    es_type <- "Correlation"
    effect_size <- input$c_es
    k <- input$c_k
    sample_size <- input$c_n
    p <- input$c_p
    test_type <- input$c_test_type
    metapower::plot_mpower(mpower(effect_size, sample_size, k, es_type, test_type, p))
  })
  output$c_summary <- renderPrint({
    es_type <- "Correlation"
    effect_size <- input$c_es
    k <- input$c_k
    sample_size <- input$c_n
    p <- input$c_p
    test_type <- input$c_test_type
    print(mpower(effect_size, sample_size, k, es_type, test_type, p))
  })

  ## Homogen Power
  output$homogen_c_plot <- renderPlot({
    es_type <- "Correlation"
    effect_size <- input$homogen_c_es
    k <- input$homogen_c_k
    sample_size <- input$homogen_c_n
    p <- input$homogen_c_p
    test_type <- input$homogen_c_test_type
    metapower::plot_homogen_power(homogen_power(effect_size, sample_size, k, es_type, test_type, p))
  })
  output$homogen_c_summary <- renderPrint({
    es_type <- "Correlation"
    effect_size <- input$homogen_c_es
    k <- input$homogen_c_k
    sample_size <- input$homogen_c_n
    p <- input$homogen_c_p
    test_type <- input$homogen_c_test_type
    print(homogen_power(effect_size, sample_size, k, es_type, test_type, p))
  })

  ## Odds Ratio
  ## Summary Effect Size
  output$or_plot <- renderPlot({
    es_type <- "OR"
    effect_size <- input$or_es
    k <- input$or_k
    sample_size <- input$or_n
    p <- input$or_p
    test_type <- input$or_test_type
    i2 <- .50
    con_table <- c(input$or_a, input$or_b, input$or_c, input$or_d)
    metapower::plot_mpower(mpower(NULL,sample_size, k, es_type, test_type, p, i2, con_table))
  })
  output$or_summary <- renderPrint({
    es_type <- "OR"
    effect_size <- input$or_es
    k <- input$or_k
    sample_size <- input$or_n
    p <- input$or_p
    test_type <- input$or_test_type
    i2 <- .50
    con_table <- c(input$or_a, input$or_b, input$or_c, input$or_d)
    print(mpower(NULL,sample_size, k, es_type, test_type, p, i2, con_table))
  })

  ## Homogen Power
  output$homogen_or_plot <- renderPlot({
    es_type <- "OR"
    effect_size <- input$homogen_or_es
    k <- input$homogen_or_k
    sample_size <- input$homogen_or_n
    p <- input$homogen_or_p
    test_type <- input$homogen_or_test_type
    i2 <- .50
    con_table <- c(input$homogen_or_a, input$homogen_or_b, input$homogen_or_c, input$homogen_or_d)
    metapower::plot_homogen_power(homogen_power(NULL,sample_size, k, es_type, test_type, p, i2, con_table))
  })
  output$homogen_or_summary <- renderPrint({
    es_type <- "OR"
    effect_size <- input$homogen_or_es
    k <- input$homogen_or_k
    sample_size <- input$homogen_or_n
    p <- input$homogen_or_p
    test_type <- input$homogen_or_test_type
    i2 <- .50
    con_table <- c(input$homogen_or_a, input$homogen_or_b, input$homogen_or_c, input$homogen_or_d)
    print(homogen_power(NULL,sample_size, k, es_type, test_type, p, i2, con_table))
  })



}
