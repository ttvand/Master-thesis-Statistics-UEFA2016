# Define UI
shinyUI(navbarPage("Premier league match analysis",
                   theme = shinytheme("united"),
                   tabPanel("Matches",
                            # Match sidebar panel
                            sidebarPanel(
                              uiOutput("gameSelectSliders")
                            ),
                            
                            # Match main panel
                            mainPanel(
                              dataTableOutput('rawDatTable')
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
                              plotlyOutput("modelsGraphP")
                              # ggvisOutput("modelsGraphG")
                            )
                   ),
                   tabPanel("Compare model to bookie",
                            # Model graph sidebar panel
                            sidebarPanel(
                              uiOutput("modelCompareInputs")
                            ),
                            
                            # Compare graph main panel
                            mainPanel(
                              h4(textOutput("modelBookieCorr"),align="center"),
                              plotlyOutput("modelsCompareComp",width="95%")
                            )
                   )
))