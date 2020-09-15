library(shiny)
library(shinythemes)
library(shinydashboard)
library(metapower)
options(shiny.sanitize.errors = FALSE)
# Define UI ----
ui <- fluidPage(
  list(tags$head(HTML('<link rel="icon", href="metapower_sticker.png",
                                   type="image/png" />'))),
  div(style="padding: 1px 0px; width: '100%'",
      titlePanel(
        title="", windowTitle="metapoweR"
      )
  ),
  navbarPage(title = div(img(src='metapower_sticker.png',style="margin-top:-10px;padding-right:0px;padding-bottom:0px; padding-top: 0px;", height = 40)),


             sidebarLayout(
               sidebarPanel(
                 #titlePanel("Welcome to metapoweR"),

                 fluidRow(column(12, align = "center",
                                 h1("Power Analysis for Meta-analysis"),
                                 div(style="display: inline-block;",img(src="metapower_sticker.png", height= '75%', width='75%'))),
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
                         h4("3. Subgroup Analysis"),
                         )),

             ##Summary Effect Size Tab
             navbarMenu(title ="Summary Effect Size", icon = icon("database"),
                        tags$head(
                          tags$style(HTML(' .navbar {
                          padding-top:0px !important;
                          height: 40px;
                          font-size: 20px;
                          min-height:40px !important;
                          }
                        text {padding-top: 30px;}
                      .navbar-nav > li > a, .navbar-brand {
                            padding-top: 10px !important;
                            padding-bottom:1px !important;
                            height: 40px;
                            }'))),


                        ## Cohen's d
                        tabPanel("Cohen's d", fluid = TRUE,#also icon = pretty picture
                                 sidebarLayout(
                                   sidebarPanel(
                                     titlePanel("Cohen's d"),
                                     fluidRow(column(12,
                                                     #select number of studies
                                                     sliderInput(inputId = "d_k", h3("Number of Studies"), min = 2, max = 100, value = 10),
                                                     sliderInput(inputId = "d_n", h3("Number of Participants (per study)"), min = 2, max = 300, value = 20),
                                                     numericInput(inputId = "d_es", h3("Effect Size Magnitude"), min = 0, max = 5, value = 0.3, step = 0.05)),
                                              shinydashboard::box(width = 12,
                                                                         splitLayout(
                                                                           numericInput(inputId = "d_p", h4("p-value"), min = .0001, max = .05, value = .05, step = 0.01),
                                                                           radioButtons(inputId = "d_test_type", label = h4("Test type"), choices = list("two-tailed", "one-tailed"), selected = "two-tailed"))))),
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
                                                     sliderInput("c_k", h3("Number of Studies"), min = 2, max = 100, value = 10),
                                                     sliderInput("c_n", h3("Number of Participants"), min = 2, max = 300, value = 20),
                                                     sliderInput("c_es", h3("Effect Size Magnitude"), min = 0, max = 1, value = .2, step = 0.05)),
                                              shinydashboard::box(width = 12,
                                                                         splitLayout(
                                                                           numericInput(inputId = "c_p", h4("p-value"), min = .0001, max = .05, value = .05, step = 0.01),
                                                                           radioButtons(inputId = "c_test_type", label = h4("Test type"), choices = list("two-tailed", "one-tailed"), selected = "two-tailed"))))),
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
                                       column(12,
                                              sliderInput("or_k", h3("Number of Studies"), min = 2, max = 100, value = 10),
                                              sliderInput("or_n", h3("Number of Participants(per study)"), min = 2, max = 300, value = 20),
                                              h3("2x2 Contingency Table")),
                                       column(width = 12,offset = 0,
                                              tags$form(
                                                class="form-horizontal",
                                                tags$div(class="form-group",
                                                         tags$label(class = "col-sm-4 control-label", `for` = "or_a", br(),br(), "Present"),
                                                         column(width = 4, numericInput(inputId = "or_a", label = HTML("Group 1 <br/> (e.g., Treatment)"), value = 6)),
                                                         column(width = 4, numericInput(inputId = "or_b", label = HTML("Group 2 <br/> (e.g., Control)"), value = 5))),
                                                tags$div(class="form-group",
                                                         tags$label(class = "col-sm-4 control-label", `for` = "or_d", "Absent"),
                                                         column(width = 4, numericInput(inputId = "or_c", label = NULL, value = 4)),
                                                         column(width = 4, numericInput(inputId = "or_d", label = NULL, value = 5))))),
                                       shinydashboard::box(width = 12,
                                                           splitLayout(
                                                             numericInput("or_p", h4("p-value"), min = .0001, max = .05, value = .05, step = 0.01),
                                                             radioButtons("or_test_type", label = h4("Test type"), choices = list("two-tailed", "one-tailed"), selected = "two-tailed"))))),
                                   mainPanel(
                                     tabsetPanel(type = "tabs",
                                                 tabPanel("Power Curve", plotOutput("or_plot", height = "600px"),
                                                          fluidRow(column(7,
                                                                 helpText("Note: Horizontal dashed line is 80% power.")))),
                                                 tabPanel("Summary", verbatimTextOutput("or_summary"))))))),


            ## Test of Homogeneity Tab
             navbarMenu("Test of Homogeneity", icon = icon("chart-bar"),

                        ## Cohen's d
                        tabPanel("Cohen's d", fluid = TRUE,#also icon = pretty picture
                                 sidebarLayout(
                                   sidebarPanel(
                                     titlePanel("Cohen's d"),
                                     fluidRow(column(12,
                                                     #select number of studies
                                                     sliderInput(inputId = "homogen_d_k", h3("Number of Studies"), min = 2, max = 100, value = 10),
                                                     sliderInput(inputId = "homogen_d_n", h3("Number of Participants (per study)"), min = 2, max = 300, value = 20),
                                                     numericInput(inputId = "homogen_d_es", h3("Effect Size Magnitude"), min = 0, max = 5, value = 0.3, step = 0.05)),
                                              shinydashboard::box(width = 12,
                                                                  splitLayout(
                                                                    numericInput(inputId = "homogen_d_p", h4("p-value"), min = .0001, max = .05, value = .05, step = 0.01))))),


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
                                                     sliderInput("homogen_c_k", h3("Number of Studies"), min = 2, max = 100, value = 10),
                                                     sliderInput("homogen_c_n", h3("Number of Participants (per study)"), min = 2, max = 300, value = 20),
                                                     sliderInput("homogen_c_es", h3("Effect Size Magnitude"), min = 0, max = 1, value = .2, step = 0.05)),
                                              shinydashboard::box(width = 12,
                                                                  splitLayout(
                                                                    numericInput(inputId = "homogen_c_p", h4("p-value"), min = .0001, max = .05, value = .05, step = 0.01))))),

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
                                     fluidRow(column(12,
                                                     sliderInput("homogen_or_k", h3("Number of Studies"), min = 2, max = 100, value = 10),
                                                     sliderInput("homogen_or_n", h3("Number of Participants(per study)"), min = 2, max = 300, value = 20),
                                                     h3("2x2 Contingency Table")),
                                              column(width = 12,offset = 0,
                                                     tags$form(
                                                       class="form-horizontal",
                                                       tags$div(class="form-group",
                                                                tags$label(class = "col-sm-4 control-label", `for` = "homogen_or_a", br(),br(), "Present"),
                                                                column(width = 4, numericInput(inputId = "homogen_or_a", label = HTML("Group 1 <br/> (e.g., Treatment)"), value = 6)),
                                                                column(width = 4, numericInput(inputId = "homogen_or_b", label = HTML("Group 2 <br/> (e.g., Control)"), value = 5))),
                                                       tags$div(class="form-group",
                                                                tags$label(class = "col-sm-4 control-label", `for` = "homogen_or_d", "Absent"),
                                                                column(width = 4, numericInput(inputId = "homogen_or_c", label = NULL, value = 4)),
                                                                column(width = 4, numericInput(inputId = "homogen_or_d", label = NULL, value = 5))))),
                                              shinydashboard::box(width = 12,
                                                                  splitLayout(
                                                                    numericInput("homogen_or_p", h4("p-value"), min = .0001, max = .05, value = .05, step = 0.01))))),

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
            navbarMenu("Subgroup Analysis", icon = icon("user-friends"),

                       ## Cohen's d
                       tabPanel("Cohen's d", fluid = TRUE,#also icon = pretty picture
                                sidebarLayout(
                                  sidebarPanel(
                                    titlePanel("Cohen's d"),
                                    fluidRow(column(12,
                                                    sliderInput(inputId = "subgroup_d_k", h3("Number of Studies (Total)"), min = 2, max = 50, value = 20),
                                                    sliderInput(inputId = "subgroup_d_n", h3("Number of Participants (per group)"), min = 2, max = 300, value = 40),
                                                    numericInput(inputId = "subgroup_d_n_groups", h3("Number of Subgroups"), min = 2, max = 10, value = 2),
                                                    textOutput("sg_d_k"),
                                                    textOutput("sg_d_n")),
                                             dashboardBody( tags$head(tags$style(HTML('.form-group, .selectize-control { margin-bottom: 5px;}.box-body {padding-bottom: 5px;}'))),
                                                            shinydashboard::box(width = 12, title = "Subgroup Effect Sizes",
                                                                 splitLayout(
                                                                   numericInput("sg1_d_es", "Subgroup1", value = .1, step = .1),
                                                                   numericInput("sg2_d_es", "Subgroup2", value = .3, step = .1),
                                                                   numericInput("sg3_d_es", "Subgroup3", value = NULL, step = .1),
                                                                   numericInput("sg4_d_es", "Subgroup4", value = NULL, step = .1)))),
                                             column(12,

                                                    numericInput(inputId = "subgroup_d_p", h3("p-value"), min = .0001, max = .05, value = .05, step = 0.01)))),
                                  mainPanel(
                                    tabsetPanel(type = "tabs",
                                                tabPanel("Power Curve", plotOutput("subgroup_d_plot", height = "600px"),
                                                         fluidRow(column(7,
                                                                         helpText("Note: Horizontal dashed line is 80% power.")))),
                                                tabPanel("Summary", verbatimTextOutput("subgroup_d_summary")))))),
                       tabPanel("Correlation", fluid = TRUE,#also icon = pretty picture
                                sidebarLayout(
                                  sidebarPanel(
                                    titlePanel("Correlation"),
                                    fluidRow(column(12,
                                                    sliderInput(inputId = "subgroup_c_k", h3("Number of Studies (total)"), min = 2, max = 50, value = 20),
                                                    sliderInput(inputId = "subgroup_c_n", h3("Number of Participants (per study)"), min = 2, max = 300, value = 40),
                                                    numericInput(inputId = "subgroup_c_n_groups", h3("Number of Groups"), min = 2, max = 10, value = 2),
                                                    textOutput("sg_c_k"),
                                                    textOutput("sg_c_n")),

                                             shinydashboard::box(width = 12, title = "Subgroup Effect Sizes",
                                                                 splitLayout(
                                                                   numericInput("subgroup_c_es1", "Subgroup1", value = .2, min = .1, max = .9, step = .05),
                                                                   numericInput("subgroup_c_es2", "Subgroup2", value = .4, min = .1, max = .99, step = .05),
                                                                   numericInput("subgroup_c_es3", "Subgroup3", value = NULL, min = .1, max = .99, step = .05),
                                                                   numericInput("subgroup_c_es4", "Subgroup4", value = NULL, min = .1, max = .99, step = .05))),
                                             column(12,
                                                    numericInput(inputId = "subgroup_c_p", h4("p-value"), min = .0001, max = .05, value = .05, step = 0.01)))),
                                  mainPanel(
                                    tabsetPanel(type = "tabs",
                                                tabPanel("Power Curve", plotOutput("subgroup_c_plot", height = "600px"),
                                                         fluidRow(column(7,
                                                                         helpText("Note: Horizontal dashed line is 80% power.")))),
                                                tabPanel("Summary", verbatimTextOutput("subgroup_c_summary")))))),
                       tabPanel("Odds Ratio", fluid = TRUE,#also icon = pretty picture
                                sidebarLayout(
                                  sidebarPanel(
                                    titlePanel("Odds Ratio"),
                                    fluidRow(column(12,
                                                    sliderInput(inputId = "subgroup_or_k", h3("Number of Studies (total)"), min = 2, max = 50, value = 20),
                                                    sliderInput(inputId = "subgroup_or_n", h3("Number of Participants (per study)"), min = 2, max = 300, value = 40),
                                                    numericInput(inputId = "subgroup_or_n_groups", h3("Number of Subgroups"), min = 2, max = 10, value = 2),
                                                    textOutput("sg_or_k"),
                                                    textOutput("sg_or_n"),
                                             br()),
                                             column(width = 12,offset = 0,
                                                    textInput("sg1_or_name", "Subgroup 1 (e.g., Men)", value = "Men"),
                                                    tags$form(
                                                      class="form-horizontal",
                                                      tags$div(class="form-group",
                                                               tags$label(class = "col-sm-4 control-label", `for` = "sg_1_or_a", br(),br(), "Present"),
                                                               column(width = 4, numericInput(inputId = "sg_1_or_a", label = HTML("Group 1 <br/> (e.g., Treatment)"), value = 6)),
                                                               column(width = 4, numericInput(inputId = "sg_1_or_b", label = HTML("Group 2 <br/> (e.g., Control)"), value = 5))),
                                                      tags$div(class="form-group",
                                                               tags$label(class = "col-sm-4 control-label", `for` = "sg_1_or_d", "Absent"),
                                                               column(width = 4, numericInput(inputId = "sg_1_or_c", label = NULL, value = 4)),
                                                               column(width = 4, numericInput(inputId = "sg_1_or_d", label = NULL, value = 5))))),
                                             column(width = 12,offset = 0,
                                                    textInput("sg2_or_name", "Subgroup 2 (e.g., Women)", value = "Women"),
                                                    tags$form(
                                                      class="form-horizontal",
                                                      tags$div(class="form-group",
                                                               tags$label(class = "col-sm-4 control-label", `for` = "sg_2_or_a", br(),br(), "Present"),
                                                               column(width = 4, numericInput(inputId = "sg_2_or_a", label = HTML("Group 1 <br/> (e.g., Treatment)"), value = 7)),
                                                               column(width = 4, numericInput(inputId = "sg_2_or_b", label = HTML("Group 2 <br/> (e.g., Control)"), value = 5))),
                                                      tags$div(class="form-group",
                                                               tags$label(class = "col-sm-4 control-label", `for` = "sg_2_or_d", "Absent"),
                                                               column(width = 4, numericInput(inputId = "sg_2_or_c", label = NULL, value = 3)),
                                                               column(width = 4, numericInput(inputId = "sg_2_or_d", label = NULL, value = 5))))),
                                             column(width = 12,offset = 0,
                                                    textInput("sg3_or_name", "Subgroup 3:"),
                                                    tags$form(
                                                      class="form-horizontal",
                                                      tags$div(class="form-group",
                                                               tags$label(class = "col-sm-4 control-label", `for` = "sg_3_or_a", br(), "Present"),
                                                               column(width = 4, numericInput(inputId = "sg_3_or_a", label = HTML("Group 1 <br/> (e.g., Treatment)"), value = NA)),
                                                               column(width = 4, numericInput(inputId = "sg_3_or_b", label = HTML("Group 2 <br/> (e.g., Control)"), value = NA))),
                                                      tags$div(class="form-group",
                                                               tags$label(class = "col-sm-4 control-label", `for` = "sg_3_or_d", "Absent"),
                                                               column(width = 4, numericInput(inputId = "sg_3_or_c", label = NULL, value = NA)),
                                                               column(width = 4, numericInput(inputId = "sg_3_or_d", label = NULL, value = NA))))),
                                             column(width = 12,offset = 0,
                                                    textInput("sg4_or_name", "Subgroup 4:"),
                                                    tags$form(
                                                      class="form-horizontal",
                                                      tags$div(class="form-group",
                                                               tags$label(class = "col-sm-4 control-label", `for` = "sg_4_or_a", br(), "Present"),
                                                               column(width = 4, numericInput(inputId = "sg_4_or_a", label = HTML("Group 1 <br/> (e.g., Treatment)"), value = NA)),
                                                               column(width = 4, numericInput(inputId = "sg_4_or_b", label = HTML("Group 2 <br/> (e.g., Control)"), value = NA))),
                                                      tags$div(class="form-group",
                                                               tags$label(class = "col-sm-4 control-label", `for` = "sg_4_or_d", "Absent"),
                                                               column(width = 4, numericInput(inputId = "sg_4_or_c", label = NULL, value = NA)),
                                                               column(width = 4, numericInput(inputId = "sg_4_or_d", label = NULL, value = NA))))),
                                             column(12,
                                                    numericInput(inputId = "subgroup_or_p", h4("p-value"), min = .0001, max = .05, value = .05, step = 0.01)))),
                                  mainPanel(
                                    tabsetPanel(type = "tabs",
                                                tabPanel("Power Curve", plotOutput("subgroup_or_plot", height = "600px"),
                                                         fluidRow(column(7,
                                                                         helpText("Note: Horizontal dashed line is 80% power.")))),
                                                tabPanel("Summary", verbatimTextOutput("subgroup_or_summary"))))))),

            ## Moderator Analysis
            ## Moderator Analysis
            ## Moderator Analysis

            navbarMenu("Moderator Analysis", icon = icon("user-friends"),

                       ## Cohen's d
                       tabPanel("Cohen's d", fluid = TRUE,#also icon = pretty picture
                                sidebarLayout(
                                  sidebarPanel(
                                    titlePanel("Cohen's d"),
                                    fluidRow(column(12,
                                                    sliderInput(inputId = "mod_d_k", h3("Number of Studies (Total)"), min = 2, max = 50, value = 20),
                                                    sliderInput(inputId = "mod_d_n", h3("Number of Participants (per study)"), min = 2, max = 300, value = 40),
                                                    numericInput(inputId = "mod_d_n_groups", h3("Number of groups"), min = 2, max = 10, value = 2),
                                                    textOutput("md_d_k")),
                                             dashboardBody( tags$head(tags$style(HTML('.form-group, .selectize-control { margin-bottom: 5px;}.box-body {padding-bottom: 5px;}'))),
                                                            shinydashboard::box(width = 12, title = "Group Effect Sizes",
                                                                                splitLayout(
                                                                                  numericInput("mod_d_es1", "Group 1", value = .1, step = .1),
                                                                                  numericInput("mod_d_es2", "Group 2", value = .3, step = .1),
                                                                                  numericInput("mod_d_es3", "Group 3", value = NULL, step = .1),
                                                                                  numericInput("mod_d_es4", "Group 4", value = NULL, step = .1)))),
                                             column(12,

                                                    numericInput(inputId = "mod_d_p", h3("p-value"), min = .0001, max = .05, value = .05, step = 0.01)))),
                                  mainPanel(
                                    tabsetPanel(type = "tabs",
                                                tabPanel("Power Curve", plotOutput("mod_d_plot", height = "600px"),
                                                         fluidRow(column(7,
                                                                         helpText("Note: Horizontal dashed line is 80% power.")))),
                                                tabPanel("Summary", verbatimTextOutput("mod_d_summary")))))),
                       tabPanel("Correlation", fluid = TRUE,#also icon = pretty picture
                                sidebarLayout(
                                  sidebarPanel(
                                    titlePanel("Correlation"),
                                    fluidRow(column(12,
                                                    sliderInput(inputId = "mod_c_k", h3("Number of Studies (total)"), min = 2, max = 50, value = 20),
                                                    sliderInput(inputId = "mod_c_n", h3("Number of Participants (per study)"), min = 2, max = 300, value = 40),
                                                    numericInput(inputId = "mod_c_n_groups", h3("Number of Groups"), min = 2, max = 10, value = 2),
                                                    textOutput("md_c_k")),

                                             shinydashboard::box(width = 12, title = "Effect Sizes",
                                                                 splitLayout(
                                                                   numericInput("mod_c_es1", "Group 1", value = .2, min = .1, max = .9, step = .05),
                                                                   numericInput("mod_c_es2", "Group 2", value = .4, min = .1, max = .99, step = .05),
                                                                   numericInput("mod_c_es3", "Group 3", value = NULL, min = .1, max = .99, step = .05),
                                                                   numericInput("mod_c_es4", "Group 4", value = NULL, min = .1, max = .99, step = .05))),
                                             column(12,
                                                    numericInput(inputId = "mod_c_p", h4("p-value"), min = .0001, max = .05, value = .05, step = 0.01)))),
                                  mainPanel(
                                    tabsetPanel(type = "tabs",
                                                tabPanel("Power Curve", plotOutput("mod_c_plot", height = "600px"),
                                                         fluidRow(column(7,
                                                                         helpText("Note: Horizontal dashed line is 80% power.")))),
                                                tabPanel("Summary", verbatimTextOutput("mod_c_summary")))))),
                       tabPanel("Odds Ratio", fluid = TRUE,#also icon = pretty picture
                                sidebarLayout(
                                  sidebarPanel(
                                    titlePanel("Odds Ratio"),
                                    fluidRow(column(12,
                                                    sliderInput(inputId = "mod_or_k", h3("Number of Studies (total)"), min = 2, max = 50, value = 20),
                                                    sliderInput(inputId = "mod_or_n", h3("Number of Participants (per study)"), min = 2, max = 300, value = 40),
                                                    numericInput(inputId = "mod_or_n_groups", h3("Number of Groups"), min = 2, max = 10, value = 2),
                                                    textOutput("md_or_k"),
                                                    br()),
                                             column(width = 12,offset = 0,
                                                    textInput("md1_or_name", "mod 1 (e.g., Men)", value = "Men"),
                                                    tags$form(
                                                      class="form-horizontal",
                                                      tags$div(class="form-group",
                                                               tags$label(class = "col-sm-4 control-label", `for` = "md_1_or_a", br(),br(), "Present"),
                                                               column(width = 4, numericInput(inputId = "md_1_or_a", label = HTML("Group 1 <br/> (e.g., Treatment)"), value = 6)),
                                                               column(width = 4, numericInput(inputId = "md_1_or_b", label = HTML("Group 2 <br/> (e.g., Control)"), value = 5))),
                                                      tags$div(class="form-group",
                                                               tags$label(class = "col-sm-4 control-label", `for` = "md_1_or_d", "Absent"),
                                                               column(width = 4, numericInput(inputId = "md_1_or_c", label = NULL, value = 4)),
                                                               column(width = 4, numericInput(inputId = "md_1_or_d", label = NULL, value = 5))))),
                                             column(width = 12,offset = 0,
                                                    textInput("md2_or_name", "mod 2 (e.g., Women)", value = "Women"),
                                                    tags$form(
                                                      class="form-horizontal",
                                                      tags$div(class="form-group",
                                                               tags$label(class = "col-sm-4 control-label", `for` = "md_2_or_a", br(),br(), "Present"),
                                                               column(width = 4, numericInput(inputId = "md_2_or_a", label = HTML("Group 1 <br/> (e.g., Treatment)"), value = 7)),
                                                               column(width = 4, numericInput(inputId = "md_2_or_b", label = HTML("Group 2 <br/> (e.g., Control)"), value = 5))),
                                                      tags$div(class="form-group",
                                                               tags$label(class = "col-sm-4 control-label", `for` = "md_2_or_d", "Absent"),
                                                               column(width = 4, numericInput(inputId = "md_2_or_c", label = NULL, value = 3)),
                                                               column(width = 4, numericInput(inputId = "md_2_or_d", label = NULL, value = 5))))),
                                             column(width = 12,offset = 0,
                                                    textInput("md3_or_name", "mod 3:"),
                                                    tags$form(
                                                      class="form-horizontal",
                                                      tags$div(class="form-group",
                                                               tags$label(class = "col-sm-4 control-label", `for` = "md_3_or_a", br(), "Present"),
                                                               column(width = 4, numericInput(inputId = "md_3_or_a", label = HTML("Group 1 <br/> (e.g., Treatment)"), value = NA)),
                                                               column(width = 4, numericInput(inputId = "md_3_or_b", label = HTML("Group 2 <br/> (e.g., Control)"), value = NA))),
                                                      tags$div(class="form-group",
                                                               tags$label(class = "col-sm-4 control-label", `for` = "md_3_or_d", "Absent"),
                                                               column(width = 4, numericInput(inputId = "md_3_or_c", label = NULL, value = NA)),
                                                               column(width = 4, numericInput(inputId = "md_3_or_d", label = NULL, value = NA))))),
                                             column(width = 12,offset = 0,
                                                    textInput("md4_or_name", "mod 4:"),
                                                    tags$form(
                                                      class="form-horizontal",
                                                      tags$div(class="form-group",
                                                               tags$label(class = "col-sm-4 control-label", `for` = "md_4_or_a", br(), "Present"),
                                                               column(width = 4, numericInput(inputId = "md_4_or_a", label = HTML("Group 1 <br/> (e.g., Treatment)"), value = NA)),
                                                               column(width = 4, numericInput(inputId = "md_4_or_b", label = HTML("Group 2 <br/> (e.g., Control)"), value = NA))),
                                                      tags$div(class="form-group",
                                                               tags$label(class = "col-sm-4 control-label", `for` = "md_4_or_d", "Absent"),
                                                               column(width = 4, numericInput(inputId = "md_4_or_c", label = NULL, value = NA)),
                                                               column(width = 4, numericInput(inputId = "md_4_or_d", label = NULL, value = NA))))),
                                             column(12,
                                                    numericInput(inputId = "mod_or_p", h4("p-value"), min = .0001, max = .05, value = .05, step = 0.01)))),
                                  mainPanel(
                                    tabsetPanel(type = "tabs",
                                                tabPanel("Power Curve", plotOutput("mod_or_plot", height = "600px"),
                                                         fluidRow(column(7,
                                                                         helpText("Note: Horizontal dashed line is 80% power.")))),
                                                tabPanel("Summary", verbatimTextOutput("mod_or_summary")))))))
             ))
