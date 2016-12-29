# Define UI
shinyUI(navbarPage("National team model analysis",
                   theme = shinytheme("cerulean"), #united
                   tabPanel("Matches",
                            # One common panel
                            fluidPage(
                              useShinyjs(), # Set up shinyjs
                              conditionalPanel(condition="false",checkboxInput("showDownloadMatches","Show download button",value=FALSE)),
                              uiOutput("gameSelectInputs"),
                              br(),
                              dataTableOutput('rawDatTable'),
                              hidden(checkboxInput('showGoalScorers',"Show goal scorers",value=FALSE)),
                              uiOutput("matchTableWidgets")
                            )
                   ),
                   tabPanel("Models",
                            # Model sidebar panel
                            sidebarPanel(
                              uiOutput("modelSelectSliders")
                            ),
                            
                            # Model main panel
                            mainPanel(
                              dataTableOutput('modelSummaryTable')
                            )
                   ),
                   tabPanel("Models graph",
                            # Model graph sidebar panel
                            sidebarPanel(
                              uiOutput("modelGraphInputs")
                            ),
                            
                            # Model graph main panel
                            mainPanel(
                              uiOutput("graphP")
                              # ggvisOutput("modelsGraphG")
                            )
                   ),
                   tabPanel("Compare models",
                            # Model graph sidebar panel
                            sidebarPanel(
                              uiOutput("modelCompareInputs")
                            ),
                            
                            # Compare graph main panel
                            mainPanel(
                              uiOutput("comparePlotTitle"),
                              plotlyOutput("modelsCompareComp",width="100%",
                                           height="500px")
                            )
                   ),
                   tabPanel("About",
                            h4(HTML(aboutString))
                   )
))