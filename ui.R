
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


library('shiny')
library('ggplot2')
source("functions.R")

shinyUI(fluidPage(

  # Application title
  titlePanel("Campaign Evaluator"),

  # Input
  verticalLayout(
    inputPanel(
       column(12,
      selectInput("panelchooser", "Choose Task",
                  c("Plan Campaign" = "plan",
                    "Evaluate Campaign" = "evaluate"),
                  selected="plan"))),

      conditionalPanel(condition="input.panelchooser=='plan'",
                       fluidRow(
                         column(6,
                                numericInput("conv1a", "Success Rate:", 0.5,
                                             min = 0, max = 1)),
                         column(6,
                                numericInput("value1a", "Success Value:", 1,
                                             min=0, max=10000))
                       ), # end fluidRow
                       fluidRow(
                         column(6,
                                numericInput("conv2a", "Success Rate:", 0.5,
                                             min = 0, max = 1)),
                         column(6,
                                numericInput("value2a", "Success Value:", 1,
                                             min=0, max=10000))
                       ), # end fluidRow
                       hr(),
                       fluidRow(
                         column(4,
                                numericInput("errorProb", "Error Probability:", 0.05,
                                             min = 0, max = 1)),
                         column(4,
                                numericInput("relErr", "Relative Error:", 0.2,
                                             min=0, max=1)),
                         column(4,
                                numericInput("countGoal", "Count Goal", 5,
                                             min=0, max=1000000))
                       ), # end fluidRow
                       helpText("Find a campaign size that will find the expected value of an action to within 
the chosen relative error, with confidence 1 - chosen Error Probability.")
      ), # end conditionalPanel

      conditionalPanel(condition="input.panelchooser=='evaluate'",
                       fluidRow(
                         column(4,
                                numericInput("actions1b", "Actions   ", 100,
                                             min=0, max=100000)),
                         column(4,
                                numericInput("success1b", "Successes", 1,
                                             min = 0, max = 100000)),
                         column(4,
                                numericInput("value1b", "Success Value:", 1,
                                             min=0, max=10000))
                       ), #end fluidRow
                       fluidRow(
                         column(4,
                                numericInput("actions2b", "Actions   ", 100,
                                             min=0, max=100000)),
                         column(4,
                                numericInput("success2b", "Successes", 1,
                                             min = 0, max = 100000)),
                         column(4,
                                numericInput("value2b", "Success Value:", 1,
                                             min=0, max=10000))
                       ), #end fluidRow
                       fluidRow(
                         column(6,
                       numericInput("wishPrice", "Wish Price", 0.05,
                                    min=0, max=100000)),
                       column(6,
                              numericInput("rescale", "Scale Factor", 1,
                                           min=0, max=100000)))

      ), # end conditionalPanel

    hr(),

    # Output
    mainPanel(
      
      # plan campaign: has both input and output
      conditionalPanel(condition="input.panelchooser=='plan'",
                       
                         
                                h4("Suggested Campaign Actions/Sizes"),
                                tableOutput("plan"),
                         
                                h4("Enter Campaign Actions/Sizes"),
                                # giving up and hard-coding the campaign names
                                numericInput("sizes1a", "", 0, min=0, max=100000),
                                numericInput("sizes2a", "", 0, min=0, max=100000),
                     
                       plotOutput("planGraph"),
                       h4("Posterior Probabilities"),
                       verbatimTextOutput("probTable"),
                       h4("Typical Outcome"), 
                       actionButton("reseed", "Regenerate"),
                       verbatimTextOutput("typicalTable"),
                       plotOutput("planGraph2T"),
                       verbatimTextOutput("probTable2T")


      ), # end conditionalPanel

      conditionalPanel(condition="input.panelchooser=='evaluate'",
                       verbatimTextOutput("resTable"),
                       plotOutput("planGraph2"),
                       h4("Posterior Probabilities"),
                       verbatimTextOutput("probTable2")
                       ) # end conditionalPanel
    ) # end mainPanel
  )) # end sidebarLayout, fluidPage
) # end shinyUI
