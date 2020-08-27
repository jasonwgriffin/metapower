library(shiny)
library(shinythemes)
library(shinydashboard)
library(metapower)

# Define UI ----
ui <- fluidPage(
  navbarPage(img(src = "/metapower_sticker.png", height = 60),


             sidebarLayout(
               sidebarPanel(
                 #titlePanel("Welcome to metapoweR"),

                 fluidRow(column(12, align = "center",
                                 h1("Power Analysis for Meta-analysis"),
                                 div(style="display: inline-block;",img(src="/metapower_sticker.png", height=300, width=300))),
                          column(12,
                                 br(),
                                 h2("Install on R:"),
                                 code('install.packages("metapower")')))),
               mainPanel(h1("Description:"),
                         h3("metapoweR is a new package that is a",
                           em("simple and effective"),
                           "tool for computing statistical power for meta-analysis"),
                         br(),
                         h1("Functionality:"),
                         h3("By using the tabs above, this shiny app allows users to compute statistical power for:"),
                         h4("1. Summary Effect Size"),
                         h4("2. Test of Homogeneity"),
                         h4("3. Moderator Analysis"),
                         )),

             ##Summary Effect Size Tab
             navbarMenu("Summary Effect Size", icon = icon("equalizer", lib = "glyphicon"),
                        tags$head(
                          tags$style(HTML(' .navbar {
                          height: 60px;
                          font-size: 25px;
                          min-height:60px !important;
                          }
                        text {padding-top: 30px;}
                      .navbar-nav > li > a, .navbar-brand {
                            padding-top:1px !important;
                            padding-bottom:1px !important;
                            height: 60px;
                            }'))),


                        ## Cohen's d
                        tabPanel("Cohen's d", fluid = TRUE,#also icon = pretty picture
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
                                   mainPanel(
                                     tabsetPanel(type = "tabs",
                                                 tabPanel("Power Curve",
                                                          plotOutput("d_plot", height = "600px"),
                                                          fluidRow(column(7,
                                                                          helpText("Note: Horizontal dashed line is 80% power.")))),
                                                 tabPanel("Summary", verbatimTextOutput("d_summary")))))),


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
                                   mainPanel(
                                     tabsetPanel(type = "tabs",
                                                 tabPanel("Power Curve", plotOutput("c_plot", height = "600px"),
                                                          fluidRow(column(7,
                                                                 helpText("Note: Horizontal dashed line is 80% power.")))),
                                                 tabPanel("Summary", verbatimTextOutput("c_summary")))))),
                        ## Odds Ratio
                        tabPanel("Odds Ratio", fluid = TRUE,
                                 sidebarLayout(
                                   sidebarPanel(
                                     titlePanel("Odds Ratio"),
                                     fluidRow(#column(12,numericInput("or_es", h3("Effect Size Magnitude"), min = 1, max = 75, value = 1.3, step = 0.)),
                                              shinydashboard::box(width = 12, title = "2x2 Contingency Table",
                                                                  splitLayout(
                                                                    numericInput("or_a", "Group 1 (Condition Present)", value = 13),
                                                                    numericInput("or_b", "Group 2 (Condition Present)", value = 487))),
                                              shinydashboard::box(width = 12,
                                                                  splitLayout(
                                                                    numericInput("or_c", "Group 1 (Condition Absent)", value = 10),
                                                                    numericInput("or_d", "Group 2 (Condition Absent)", value = 490))),
                                              column(12,
                                                     sliderInput("or_k", h3("Number of Studies"), min = 2, max = 100, value = 10),
                                                     sliderInput("or_n", h3("Number of Participants"), min = 2, max = 2000, value = 1000),
                                                     numericInput("or_p", h3("p-value"), min = .0001, max = .05, value = .05, step = 0.01),
                                                     radioButtons("or_test_type", label = "Test type", choices = list("two-tailed", "one-tailed"), selected = "two-tailed")))),

                                   mainPanel(
                                     tabsetPanel(type = "tabs",
                                                 tabPanel("Power Curve", plotOutput("or_plot", height = "600px"),
                                                          fluidRow(column(7,
                                                                 helpText("Note: Horizontal dashed line is 80% power.")))),
                                                 tabPanel("Summary", verbatimTextOutput("or_summary"))))))),


            ## Test of Homogeneity Tab
             navbarMenu("Test of Homogeneity", icon = icon("tasks", lib = "glyphicon"),

                        ## Cohen's d
                        tabPanel("Cohen's d", fluid = TRUE,#also icon = pretty picture
                                 sidebarLayout(
                                   sidebarPanel(
                                     titlePanel("Cohen's d"),
                                     fluidRow(column(12,
                                                     #select number of studies
                                                     numericInput(inputId = "homogen_d_es", h3("Effect Size Magnitude"), min = 0, max = 5, value = 0.3, step = 0.05),
                                                     sliderInput(inputId = "homogen_d_k", h3("Number of Studies"), min = 2, max = 100, value = 10),
                                                     sliderInput(inputId = "homogen_d_n", h3("Number of Participants"), min = 2, max = 300, value = 20),
                                                     numericInput(inputId = "homogen_d_p", h3("p-value"), min = .0001, max = .05, value = .05, step = 0.01),
                                                     radioButtons(inputId = "homogen_d_test_type", label = "Test type", choices = list("two-tailed", "one-tailed"), selected = "two-tailed")))),
                                   mainPanel(
                                     tabsetPanel(type = "tabs",
                                                 tabPanel("Power Curve", plotOutput("homogen_d_plot", height = "600px"),
                                                          fluidRow(column(7,
                                                                          helpText("Note: Horizontal dashed line is 80% power.")))),
                                                 tabPanel("Summary", verbatimTextOutput("homogen_d_summary")))))),


                        ## Correlation
                        tabPanel("Correlation", fluid = TRUE,
                                 sidebarLayout(
                                   sidebarPanel(
                                     titlePanel("Correlation"),
                                     fluidRow(column(12,
                                                     sliderInput("homogen_c_es", h3("Effect Size Magnitude"), min = 0, max = 1, value = .2, step = 0.05),
                                                     sliderInput("homogen_c_k", h3("Number of Studies"), min = 2, max = 100, value = 10),
                                                     sliderInput("homogen_c_n", h3("Number of Participants"), min = 2, max = 300, value = 20),
                                                     numericInput("homogen_c_p", h3("p-value"), min = .0001, max = .05, value = .05, step = 0.01),
                                                     radioButtons(inputId = "homogen_c_test_type", label = "Test type", choices = list("two-tailed", "one-tailed"), selected = "two-tailed")))),
                                   mainPanel(
                                     tabsetPanel(type = "tabs",
                                                 tabPanel("Power Curve", plotOutput("homogen_c_plot", height = "600px"),
                                                          fluidRow(column(7,
                                                                          helpText("Note: Horizontal dashed line is 80% power.")))),
                                                 tabPanel("Summary", verbatimTextOutput("homogen_c_summary")))))),
                        ## Odds Ratio
                        tabPanel("Odds Ratio", fluid = TRUE,
                                 sidebarLayout(
                                   sidebarPanel(
                                     titlePanel("Odds Ratio"),
                                     fluidRow(#column(12, numericInput("homogen_or_es", h3("Effect Size Magnitude"), min = 1, max = 75, value = 1.3, step = 0.)),
                                              shinydashboard::box(width = 12, title = "2x2 Contingency Table",
                                                                  splitLayout(
                                                                    numericInput("homogen_or_a", "Group 1 (Condition Present)", value = 13),
                                                                    numericInput("homogen_or_b", "Group 2 (Condition Present)", value = 487))),
                                              shinydashboard::box(width = 12,
                                                                  splitLayout(
                                                                    numericInput("homogen_or_c", "Group 1 (Condition Absent)", value = 10),
                                                                    numericInput("homogen_or_d", "Group 2 (Condition Absent)", value = 490))),
                                              column(12,
                                                     sliderInput("homogen_or_k", h3("Number of Studies"), min = 2, max = 100, value = 10),
                                                     sliderInput("homogen_or_n", h3("Number of Participants"), min = 2, max = 2000, value = 1000),
                                                     numericInput("homogen_or_p", h3("p-value"), min = .0001, max = .05, value = .05, step = 0.01),
                                                     radioButtons("homogen_or_test_type", label = "Test type", choices = list("two-tailed", "one-tailed"), selected = "two-tailed")))),

                                   mainPanel(
                                     tabsetPanel(type = "tabs",
                                                 tabPanel("Power Curve", plotOutput("homogen_or_plot", height = "600px"),
                                                          fluidRow(column(7,
                                                                          helpText("Note: Horizontal dashed line is 80% power.")))),
                                                 tabPanel("Summary", verbatimTextOutput("homogen_or_summary"))))))),
            ## Subgroup Analysis
            ## Subgroup Analysis
            ## Subgroup Analysis
            ## Subgroup Analysis
            ## Subgroup Analysis
            navbarMenu("Subgroup Analysis", icon = icon("tasks", lib = "glyphicon"),

                       ## Cohen's d
                       tabPanel("Cohen's d", fluid = TRUE,#also icon = pretty picture
                                sidebarLayout(
                                  sidebarPanel(
                                    titlePanel("Cohen's d"),
                                    fluidRow(column(12,
                                                    numericInput(inputId = "subgroup_d_n_groups", h3("Number of Groups"), min = 2, max = 10, value = 2)),
                                             shinydashboard::box(width = 6, title = "Subgroup Effect Sizes",
                                                                 numericInput("subgroup_d_es1", "Group 1", value = .2, max = 5, step = .1),
                                                                 numericInput("subgroup_d_es2", "Group 2", value = .4, max = 5, step = .1),
                                                                 numericInput("subgroup_d_es3", "Group 3", value = NULL, max = 5, step = .1),
                                                                 numericInput("subgroup_d_es4", "Group 4", value = NULL, max = 5, step = .1)),

                                             column(12,
                                                    sliderInput(inputId = "subgroup_d_n", h3("Number of Participants (total)"), min = 2, max = 300, value = 20),
                                                    sliderInput(inputId = "subgroup_d_k", h3("Number of Studies (total)"), min = 2, max = 100, value = 20),
                                                    numericInput(inputId = "subgroup_d_p", h3("p-value"), min = .0001, max = .05, value = .05, step = 0.01),
                                                    radioButtons(inputId = "subgroup_d_test_type", label = "Test type", choices = list("two-tailed", "one-tailed"), selected = "two-tailed")))),
                                  mainPanel(
                                    tabsetPanel(type = "tabs",
                                                tabPanel("Power Curve", plotOutput("subgroup_d_plot", height = "600px"),
                                                         fluidRow(column(7,
                                                                         helpText("Note: Horizontal dashed line is 80% power.")))),
                                                tabPanel("Summary", verbatimTextOutput("subgroup_d_summary")))))))
             ))
