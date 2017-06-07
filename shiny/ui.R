
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
  titlePanel("Response Driven Campaign Planner"),
  a("(advertisment supported) Video training course:",
    href="https://www.youtube.com/playlist?list=PLAKBwakacHbRRw278HMXpCsOOIIcLYGX5"),
  p('This sheet: GPL3 license, no warranty'),
  a("Source code: GitHub Win/Vector/CampaignPlanner/",
    href="https://github.com/WinVector/CampaignPlanner/"),
  p(''),
  a("Online version: win-vector.shinyapps.io/CampaignPlanner/",
    href="https://win-vector.shinyapps.io/CampaignPlanner/"),
  p(''),
  a("New (better) version: win-vector.shinyapps.io/CampaignPlanner/",
    href="https://win-vector.shinyapps.io/CampaignPlanner_v3/"),
  
  
  # Input
  verticalLayout(
    inputPanel(
      helpText("Here the user chooses either to plan a new A/B test/campaign or evalute the results of an already run A/B campaign."),
       column(12,
      selectInput("panelchooser", "Choose Task",
                  c("Plan Campaign" = "plan",
                    "Evaluate Campaign" = "evaluate"),
                  selected="plan"))),

      conditionalPanel(condition="input.panelchooser=='plan'",
                       helpText("The purpose of the planning sheet is to use the user inputs (prior bounds on conversion rate and conversion value) to estimate an acceptable absolute error in campaign value.  This acceptable error rate is used to pick campaign sizes that ensure the campaign chosen has a good probability of being close to the best choice in terms of relative error.
  A good way to use this is to enter two different rates you wish to be able to distinguish between."),
                       helpText("Each row below is a campaign.  For each campaign we ask the user supply plausible (from pervious experience) lower bounds on the success rate (what fraction of actions return value) and the value per success assumed for the campaign."),
                       helpText("These controls help populate the Suggested Campaign Actions/Sizes panel and the Possible Values section."),
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
                       helpText("Below the user is asked what probability (Error Probability) of mis-estimating each campaign ot more than a relative multiple (Relative Error) of the most valuable campaign and for a minimum number of observed successes/conversions goal.  These choices determine an appropriate absolute error rate (in dollars per action) for the problem and then help populate the Suggested Campaign Actions/Sizes table."),
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
                       ) # end fluidRow
                       
      ), # end conditionalPanel

      conditionalPanel(condition="input.panelchooser=='evaluate'",
                       helpText("The purpose of the evaluation sheet is to take results from a previously run campaign results and show the likely possibilities for the unknown true values of the traffic sources."),
                       helpText("For each of two already run campaigns user enters the campaign results: size (number of trials/actions), number of successes and value of each success."),
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
                       helpText("Wish prices adds a price annotation to the graph, and Sale Factor > 1 lets the user see what a larger campaign with same result rates could look like."),
                       fluidRow(
                         column(6,
                       numericInput("wishPrice", "Wish Price", 0.05,
                                    min=0, max=100000)),
                       column(6,
                              numericInput("rescale", "Scale Factor", 1,
                                           min=0, max=100000))) #end fluidRow
                            ), # end conditionalPanel

    hr(),

    # Output
    mainPanel(
      # plan campaign: has both input and output
      conditionalPanel(condition="input.panelchooser=='plan'",
                                h4("Suggested Campaign Actions/Sizes"),
                                tableOutput("plan"),
                         helpText("Here the user inputs the proposed campaign sizes (look above to the Suggested Campaign Actions/Sizes for values to try).  This populate the Possible Values section."),
                                h4("Enter Campaign Actions/Sizes"),
                                # giving up and hard-coding the campaign names
                                numericInput("sizes1a", "Size of first campaign", 100, min=0, max=100000),
                                numericInput("sizes2a", "Size of secont campaign", 100, min=0, max=100000),
                       h4("Possible Values"), 
                       helpText("This section shows distribution (with probabilities) of observed success frequencies/values for a the user specified (unobserved) true campaign rates. These are the plots of the likely distribution of what will bee seen during estimation if the two campaign had rates as the user specified earlier in the sheet.  The idea is we need to know how often the campaign that is truly more valuable appears to be more valuable during measurement."),
                       plotOutput("planGraph"),
                       h4("Posterior Probabilities"),
                       verbatimTextOutput("probTable"),
                       h4("Typical Outcome"), 
                       helpText("This section simulates the evaluation sheet for the above campaings.  It shows for a given emprical obseration (drawn at random) from the user specified campaigs what distribution would the evaluation sheet estimate for the true values of the campaigns.  In practice the true values of the campaigs are the unknown quantities the evaluation sheet is trying to estimate.  In this simulation sheet the user should check if the values observed are near the simulation values they specified often enough."),
                       actionButton("reseed", "Regenerate"),
                       verbatimTextOutput("typicalTable"),
                       plotOutput("planGraph2T"),
                       verbatimTextOutput("probTable2T")


      ), # end conditionalPanel

      conditionalPanel(condition="input.panelchooser=='evaluate'",
                       helpText("Here we show the distribution on the unkown true values of the campaigns that the user can infer from the results entered."),
                       verbatimTextOutput("resTable"),
                       plotOutput("planGraph2"),
                       h4("Posterior Probabilities"),
                       verbatimTextOutput("probTable2")
                       ) # end conditionalPanel
    ) # end mainPanel
  )) # end sidebarLayout, fluidPage
) # end shinyUI
