#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(metapower)
# Define UI ----
ui <- fluidPage(
  navbarPage("metapoweR",theme = shinytheme("lumen"),
             navbarMenu("Power Analysis",

                        ## Cohen's d
                        tabPanel("Cohens; d", fluid = TRUE, # also icon = pretty picture
                                 sidebarLayout(
                                   sidebarPanel(
                                     titlePanel("Cohen's d"),
                                     fluidRow(column(12,
                                                     #select number of studies
                                                     numericInput(inputId = "d_es", h3("Effect Size Magnitude"), min = 0, max = 5, value = 0.3, step = 0.05),
                                                     sliderInput(inputId = "d_k", h3("Number of Studies"), min = 2, max = 100, value = 10),
                                                     sliderInput(inputId = "d_n", h3("Number of Participants"), min = 2, max = 300, value = 20),
                                                     numericInput(inputId = "d_p", h3("p-value"), min = .0001, max = .05, value = .05, step = 0.01),
                                                     radioButtons(inputId = "d_test_type", label = "Test type", choices = list("two-tailed", "one-tailed"), selected = "two-tailed")))),
                                   mainPanel(plotOutput(outputId = "d_plot")))),

                        ## Correlation
                        tabPanel("Correlation", fluid = TRUE,
                                 sidebarLayout(
                                   sidebarPanel(
                                     titlePanel("Correlation"),
                                     fluidRow(column(12,
                                                     sliderInput("c_es", h3("Effect Size Magnitude"), min = 0, max = 1, value = .2, step = 0.05),
                                                     sliderInput("c_k", h3("Number of Studies"), min = 2, max = 100, value = 10),
                                                     sliderInput("c_n", h3("Number of Participants"), min = 2, max = 300, value = 20),
                                                     numericInput("c_p", h3("p-value"), min = .0001, max = .05, value = .05, step = 0.01),
                                                     radioButtons(inputId = "c_test_type", label = "Test type", choices = list("two-tailed", "one-tailed"), selected = "two-tailed")))),
                                   mainPanel(plotOutput(outputId = "c_plot")))),
                        ## Odds Ratio
                        tabPanel("Odds Ratio", fluid = TRUE,
                                 sidebarLayout(
                                   sidebarPanel(
                                     titlePanel("Odds Ratio"),
                                     fluidRow(column(12,
                                                     numericInput("or_es", h3("Effect Size Magnitude"), min = 1, max = 75, value = 1.3, step = 0.1),
                                                     sliderInput("or_k", h3("Number of Studies"), min = 2, max = 100, value = 10),
                                                     sliderInput("or_n", h3("Number of Participants"), min = 2, max = 2000, value = 1000),
                                                     numericInput("or_p", h3("p-value"), min = .0001, max = .05, value = .05, step = 0.01),
                                                     radioButtons("or_test_type", label = "Test type", choices = list("two-tailed", "one-tailed"), selected = "two-tailed")),
                                              box(width = 12, title = "2x2 Contingency Table",
                                                  splitLayout(
                                                    numericInput("or_a", "Group 1 (Condition Present)", value = 13),
                                                    numericInput("or_b", "Group 2 (Condition Present)", value = 487))),
                                              box(width = 12,
                                                  splitLayout(
                                                    numericInput("or_c", "Group 1 (Condition Absent)", value = 10),
                                                    numericInput("or_d", "Group 2 (Condition Absent)", value = 490))))),
                                   mainPanel(plotOutput(outputId = "or_plot"))))




                        )))




# Define server logic ----
server <- function(input, output) {

  #Power Analysis

  ## Cohen's d Plot
  output$d_plot <- renderPlot({

    es_type <- "d"
    effect_size <- input$d_es
    k <- input$d_k
    sample_size <- input$d_n
    p <- input$d_p
    test_type <- input$d_test_type
    metapower::plot_mpower(mpower(effect_size, sample_size, k, es_type, test_type, p))
  })

  # Correlation Plot
  output$c_plot <- renderPlot({
    es_type <- "Correlation"
    effect_size <- input$c_es
    k <- input$c_k
    sample_size <- input$c_n
    p <- input$c_p
    test_type <- input$c_test_type
    metapower::plot_mpower(mpower(effect_size, sample_size, k, es_type, test_type, p))
  })

  # Correlation Plot
  output$or_plot <- renderPlot({
    es_type <- "OR"
    effect_size <- input$or_es
    k <- input$or_k
    sample_size <- input$or_n
    p <- input$or_p
    test_type <- input$or_test_type
    i2 <- .50
    con_table <- c(input$or_a, input$or_b, input$or_c, input$or_d)
    metapower::plot_mpower(mpower(effect_size, sample_size, k, es_type, test_type, p, i2, con_table))
  })

}

# Run the app ----
shinyApp(ui = ui, server = server)
