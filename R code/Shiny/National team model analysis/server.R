# Function to return the previous value unless if its zero in which case a 
# default value is returned
setLastVal <- function(value,default){
  if(is.null(value)){
    return(default)
  }
  return(value)
}

shinyServer(function(input, output, session) {
  show("showGoalScorers")
  
  # Generate the game selection input slider widgets
  output$gameSelectInputs <- renderUI({
    dateRange <- range(summary$Date)
    dateRangeC <- format(dateRange,"%d/%m/%Y")
    list(
      fluidRow(column(4,
                      dateRangeInput("dateSelection", "Date range:",
                                     start  = dateRange[1],
                                     end    = dateRange[2],
                                     min    = dateRange[1],
                                     max    = dateRange[2],
                                     format = "dd/mm/yyyy",
                                     separator = " - ")
      ),
      column(8,
             h3(dataString)
      )
      ),
      fluidRow(column(4,selectizeInput("selectedTeams","Selected teams - no selection implies using all teams",
                                       choices=sort(unique(c(summary$home,
                                                             summary$visitor))),
                                       multiple=TRUE)),
               column(4,sliderInput("minPopulation","Minimum country population (times 1,000)",
                                    step=2,min=0,max=1000,value=100)),
               column(4,sliderInput("minMatchCount","Minimum historical match count",
                                    step=1,min=0,max=100,value=20))
      ),
      fluidRow(column(4,actionButton("removeTeamSelection","Clear team selection",
                                     icon=icon("eraser"))),
               column(8,h3(textOutput("sizeExclusionText")))
      ),
      h3("")
    )
  })
  
  output$sizeExclusionText <- renderText({ 
    excludedCountries <-
      population$Country[population$Population<input$minPopulation*1000]
    nbExcluded <- length(excludedCountries)
    countryString <- ifelse(nbExcluded==1," country"," countries")
    out <- paste0(nbExcluded,countryString,
                  " excluded based on the population selection",
                  ifelse(nbExcluded>0,": ",""),
                  paste(excludedCountries,collapse=", "))
    
    out
  })
  
  # Clear team selection
  observeEvent(input$removeTeamSelection,{
    updateSelectizeInput(session,"selectedTeams",selected="")
  })
  
  # Subset the summary data on the date and team selection as well as the 
  # country's population size. An additional selection is made on the
  # number of historical national team matches.
  rawData <- reactive({
    if(is.null(input$dateSelection)) return()
    out <- summary
    out <- out[out$Date>=input$dateSelection[1] &
                 out$Date<=input$dateSelection[2],]
    if(!is.null(input$selectedTeams)){
      out <- out[out$home %in% input$selectedTeams |
                   out$visitor %in% input$selectedTeams,]
    }
    
    # Country size subsetting
    validPopulationCountries <-
      population$Country[population$Population>=input$minPopulation*1000]
    out <- out[out$home %in% validPopulationCountries &
                 out$visitor %in% validPopulationCountries,]
    
    # Historical match count subsetting
    out <- out[out$homeMatchCount >= input$minMatchCount &
                 out$awayMatchCount >= input$minMatchCount,]
    
    out
  })
  
  # Show the download button if there is data to be downloaded
  output$matchTableWidgets <- renderUI({
    rawData <- rawData()
    if(is.null(rawData) || nrow(rawData)<=0) return()
    
    downloadButton('downloadRawData', 'Download')
  })
  
  # Generate a data table of the raw data
  output$rawDatTable <- renderDataTable({
    if(is.null(rawData())) return()
    out <- rawData()
    
    # Discard prediction columns (too many)
    out <- out[,!grepl("prob",names(out))]# & !grepl("outcome",names(out))]
    
    # Convert strings to factors
    stringInd <- sapply(out,function(x) is.character(x) && length(unique(x))<100)
    out[stringInd] <- lapply(out[stringInd], as.factor)
    
    # Make the links clickable
    out$link <- paste0('<a href=',out$link,' target="_blank">Link</a>')
    
    # Hide the goal scorers
    if(!is.null(input$showGoalScorers) && !input$showGoalScorers){
      out$Goals <- NULL
    }
    
    out
  }, options = list(pageLength = 5,
                    lengthMenu = list(c(5, 10, 20, -1),
                                      c('5', '10', '20', 'All')),
                    searching=TRUE),
  filter="top", escape = FALSE
  )
  
  # Download logic of the raw data table
  output$downloadRawData <- downloadHandler(
    filename = function() {
      paste0("Match data.csv")},
    content = function(file){
      out <- rawData()
      
      # Discard prediction columns (too many)
      out <- out[,!grepl("prob",names(out))] 
      write.csv(out,file,row.names=FALSE)
    }
  )
  
  # Generate the model selection input slider widgets
  output$modelSelectSliders <- renderUI({
    if(is.null(rawData())) return()
    out <- list(
      sliderInput("halfPeriodSelection","Half period selection",
                  value=setLastVal(input$halfPeriodSelection,
                                   range(halfPeriods)),sep="",
                  min = min(halfPeriods),max = max(halfPeriods),
                  step=diff(sort(unique(halfPeriods)))[1]),
      sliderInput("relaxationSelection","Relaxation selection",
                  value=setLastVal(input$relaxationSelection,
                                   range(relaxations)),sep="",
                  min = min(relaxations),max = max(relaxations),
                  step=diff(sort(unique(relaxations)))[1])
    )
    
    # Add a checkbox for the selection of each of the models 
    allModels <- names(table(modelNames))
    for(i in 1:length(allModels)){
      model <- allModels[i]
      out <- c(out,
               list(checkboxInput(paste0(model,"Select"),model,
                                  value=setLastVal(input[[paste0(model,"Select")]],TRUE)
               )
               )
      )
    }
    out
  })
  
  # Calculate the model summary parameters for rawData, a row subset of summary
  modelSummary <- reactive({
    if(is.null(rawData())) return()
    out <- NULL
    rawData <- rawData()
    # rawData <- as.matrix(rawData)
    nbModels <- length(modelColumns)
    
    withProgress(message = 'Calculating model measures',
                 detail = '', value = 0, {
                   
                   for(i in 1:nbModels){
                     modelColumn <- modelColumns[i]
                     pH <- rawData[,modelColumn+1]
                     pD <- rawData[,modelColumn+2]
                     pA <- abs(1-rawData[,modelColumn+1]-rawData[,modelColumn+2])
                     
                     avReturn <- mean(1/rawData[,modelColumn])
                     if(!is.finite(avReturn)){
                       avReturn <- NA
                     }
                     avAcc <- mean(rawData[,modelColumn])
                     logLoss <- -mean(log(rawData[,modelColumn]))
                     maxProb <- pH
                     maxProb[pD>pH] <- pD[pD>pH]
                     maxProb[pA>maxProb] <- pA[pA>maxProb]
                     majCorrect <- mean(abs(maxProb-rawData[,modelColumn])<1e-12)
                     
                     if(i==1){
                       out <- rbind(out,
                                    data.frame(method=rep(modelNames[i],nbModels),
                                               halfPeriod=rep(halfPeriods[i],nbModels),
                                               relaxation=rep(relaxations[i],nbModels),
                                               avReturn=rep(avReturn,nbModels),
                                               avAcc=rep(avAcc,nbModels),
                                               majCorrect=rep(majCorrect,nbModels),
                                               logLoss=rep(logLoss,nbModels),
                                               stringsAsFactors=FALSE))
                     } else{
                       out[i,] <- list(modelNames[i], halfPeriods[i], relaxations[i],
                                       avReturn, avAcc, majCorrect, logLoss)
                     } 
                   }
                   incProgress(1/nbModels, detail = paste0("model ", i," of ",
                                                           nbModels))
                 })
    
    out$ID <- 1:nrow(out)
    
    names(out)[4:7] <- c("Average bet return","Average accuracy", "Majority correct",
                         "Average log loss")
    
    # Convert extreme values to Inf
    for(i in 4:7){
      out[is.na(out[,i]) | out[,i]>1e4,i] <- NA
    }
    
    out
  })
  
  # Calculate a subset of the model summary parameters
  modelSummarySubset <- reactive({
    if(is.null(modelSummary())) return()
    out <- modelSummary()
    
    # Subset models if the models tab has the sliders activated
    if(!is.null(input$halfPeriodSelection)){
      out <- out[out$halfPeriod>=input$halfPeriodSelection[1] &
                   out$halfPeriod<=input$halfPeriodSelection[2],]
      out <- out[out$relaxation>=input$relaxationSelection[1] &
                   out$relaxation<=input$relaxationSelection[2],]
      
      allModels <- names(table(modelNames))
      for(i in 1:length(allModels)){
        model <- allModels[i]
        checkboxValue <- input[[paste0(model,"Select")]]
        if(!is.null(checkboxValue) && !checkboxValue){
          out <- out[out$method!=model,]
        }
      }
    }
    
    names(out) <- c("Method",xModels,yModels,"ID")
    
    out
  })
  
  # Generate a data table of the subsetted model summary data
  output$modelSummaryTable <- renderDataTable({
    out <- modelSummarySubset()
    
    out
  }, options = list(pageLength = 5,
                    lengthMenu = list(c(5, 10, 20, -1),
                                      c('5', '10', '20', 'All')))
  )
  
  # Generate the model graph input widgets
  output$modelGraphInputs <- renderUI({
    if(is.null(modelSummary())) return()
    dummy <- input$xModels
    isolate({
      out <- list(
        radioButtons("xModels","X axis",choices=xModels,inline=TRUE,selected=setLastVal(input$xModels,xModels[2])),
        selectInput("yModels","Y axis",choices=yModels,selected=setLastVal(input$yModels,yModels[4])),
        br(),
        checkboxInput("textLabelModelsGraph","Show other X label on hover?",value=setLastVal(input$textLabelModelsGraph,TRUE)),
        checkboxInput("subsetModel","Subset on other X?",value=setLastVal(input$subsetModel,FALSE)),
        conditionalPanel(condition="input.subsetModel",
                         sliderInput("sliderotherX",label="",min=0,max=1,value=0))
      )
      
      out
    })
  })
  
  # Set the range of the other slider X
  observe({
    if(is.null(input$subsetModel)||input$subsetModel==FALSE) return()
    
    optionsX <- xModels
    selectedX <- input$xModels
    isolate({
      sliderX <- optionsX[optionsX!=selectedX][1]
      valuesX <- modelSummarySubset()[[sliderX]]
      xVals <- sort(unique(valuesX))
      
      updateSliderInput(session,"sliderotherX",label=sliderX,min=min(valuesX),
                        max=max(valuesX),value=min(valuesX),
                        step=xVals[2]-xVals[1])
    })
  })
  
  modelSummarySubsetPlot <- reactive({
    if(is.null(input$subsetModel) || is.null(modelSummarySubset())) return()
    out <- modelSummarySubset()
    
    if(input$subsetModel){
      optionsX <- xModels
      selectedX <- input$xModels
      sliderX <- optionsX[optionsX!=selectedX][1]
      
      out <- out[out[[sliderX]]==input$sliderotherX,]
    }
    out
  })
  
  # Function for generating models tooltip text
  models_tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$ID)) return(NULL)
    
    # Pick out the model with this ID
    models <- modelSummary()
    model <- models[models$ID == x$ID,]
    
    paste0("<b>", model$method, "</b><br>",
           "Half period: ", model$halfPeriod, "<br>",
           "Relaxation: ", model$relaxation
    )
  }
  
  #   # Generate the ggvis plot
  #   observe({
  #     if(is.null(modelSummarySubsetPlot()) || is.null(input$xModels) ||
  #        is.null(input$yModels)) return()
  #     xProp <- prop("x", as.name(input[["xModels"]]))
  #     yProp <- prop("y", as.name(input[["yModels"]]))
  #     fillProp <- props(fill = ~factor(modelSummarySubsetPlot()[[1]]))
  #     
  #     # Calculate the stroke color
  #     
  #     p <- modelSummarySubsetPlot() %>%
  #       ggvis(xProp,yProp, key:=~ID) %>%
  #       layer_points(fill = ~factor(Method), size := 50, size.hover := 200,
  #                    fillOpacity := 1, fillOpacity.hover := 0.5,
  #                    key := ~ID) %>%
  #       add_tooltip(models_tooltip, "hover") %>%
  #       set_options(width = "auto")
  #     p %>% bind_shiny("modelsGraphG")
  #   })
  
  output$graphP <- renderUI({
    # Check if the output graph contains no non-missing values
    if(is.null(modelSummarySubsetPlot()) || is.null(input$xModels) ||
       is.null(input$yModels) || nrow(modelSummarySubsetPlot())<=0) return()
    
    modelSummarySubset <- modelSummarySubsetPlot()
    names(modelSummarySubset) <- gsub(" ","",names(modelSummarySubset))
    yName <- gsub(" ","",input[["yModels"]])
    if(all(is.na(modelSummarySubset[[yName]]))){
      out <- h3(paste0("All ",input[["yModels"]]," values infinite"))
    } else{
      out <- plotlyOutput("modelsGraphP", height="550px",width="100%")
    }
    
    out
  })
  
  output$modelsGraphP <- renderPlotly({
    if(is.null(modelSummarySubsetPlot()) || is.null(input$xModels) ||
       is.null(input$yModels) || nrow(modelSummarySubsetPlot())<=0) return()
    
    modelSummarySubset <- modelSummarySubsetPlot()
    
    names(modelSummarySubset) <- gsub(" ","",names(modelSummarySubset))
    xName <- gsub(" ","",input[["xModels"]])
    otherId <- ifelse(xName==names(modelSummarySubset)[2],3,2)
    otherXName <- names(modelSummarySubset)[otherId]
    modelSummarySubset[[otherXName]] <- paste0(otherXName,": ",
                                               modelSummarySubset[[otherXName]])
    yName <- gsub(" ","",input[["yModels"]])
    
    # Replace y by -1 if all missing
    if(all(is.na(modelSummarySubset[[yName]]))){
      modelSummarySubset[[yName]] <- -1
    }
    
    if(!ggplot2plotly){
      modelSummarySubset[["xName"]] <- modelSummarySubset[[xName]]
      modelSummarySubset[["yName"]] <- modelSummarySubset[[yName]]
      x <- list(title = xName)
      y <- list(title = yName)
      out <- plot_ly(modelSummarySubset, x=xName, y=yName, mode = "markers",
                     color=Method) %>%
        layout(xaxis = x, yaxis = y)
    } else{
      gg <- ggplot(modelSummarySubset, aes_string(xName,yName,colour="Method"))
      if(input$textLabelModelsGraph){
        gg <- gg + geom_point(aes_string(text=otherXName))
      } else{
        gg <- gg + geom_point()
      }
      
      # Remove the method legend if there is only one method
      if(length(unique(modelSummarySubset[["Method"]]))<=1){
        gg <- gg + theme(legend.position="none")
      }
      out <- ggplotly(gg)
      # out <- layout(out, hovermode = 'closest')
    }
    
    # targetRows <- modelSummarySubset$Relaxation %in%
    #   c("Relaxation: 0", "Relaxation: 0.005", "Relaxation: 0.01",
    #     "Relaxation: 0.015")
    # modelSummarySubset <- modelSummarySubset[targetRows,]
    # modelSummarySubset$Relaxation <-
    #   sapply(modelSummarySubset$Relaxation, function(x) substring(x,13))
    # modelSummarySubset$Relaxation <- factor(modelSummarySubset$Relaxation)
    # 
    # gg <- ggplot(modelSummarySubset, aes_string("Halfperiod",yName,
    #                                             colour="Relaxation"))
    # gg +
    #   geom_point() +
    #   xlim(c(600,1600)) +
    #   ylim(c(0.9157,0.9162)) +
    #   xlab("Half period") +
    #   ylab("Average log loss") +
    #   # xlab("Elo update constant") +
    #   theme(axis.text=element_text(size=12),
    #         legend.text=element_text(size=11),
    #         axis.title=element_text(size=14),
    #         title=element_text(size=14))
    # 
    # browser()
    
    out
  })
  
  # Generate the model compare graph input widgets
  output$modelCompareInputs <- renderUI({
    if(is.null(modelSummary())) return()
    modelNames <- sort(unique(modelNames))
    out <- list(
      selectInput("yModelsCompare","Best target",choices=yModels,
                  selected=setLastVal(input$yModelsCompare,yModels[4])),
      actionButton("setBestModel","Set best model settings wrt best target"),
      HTML(paste(rep("<br>",2),collapse="")),
      selectInput("modelCompare1","Model 1",choices=modelNames,
                  selected=setLastVal(input$modelCompare1,modelNames[1])),
      selectInput("modelCompare2","Model 2",choices=modelNames,
                  selected=setLastVal(input$modelCompare2,modelNames[2])),
      sliderInput("halfPeriodSelectionComp1","Half period selection 1",
                  value=setLastVal(input$halfPeriodSelectionComp1,
                                   min(halfPeriods)),sep="",
                  min = min(halfPeriods),max = max(halfPeriods),
                  step=diff(sort(unique(halfPeriods)))[1]),
      sliderInput("halfPeriodSelectionComp2","Half period selection 2",
                  value=setLastVal(input$halfPeriodSelectionComp2,
                                   min(halfPeriods)),sep="",
                  min = min(halfPeriods),max = max(halfPeriods),
                  step=diff(sort(unique(halfPeriods)))[1]),
      
      sliderInput("relaxationSelectionComp1","Relaxation selection 1",
                  value=setLastVal(input$relaxationSelectionComp1,
                                   min(relaxations)),sep="",
                  min = min(relaxations),max = max(relaxations),
                  step=diff(sort(unique(relaxations)))[1]),
      sliderInput("relaxationSelectionComp2","Relaxation selection 2",
                  value=setLastVal(input$relaxationSelectionComp2,
                                   min(relaxations)),sep="",
                  min = min(relaxations),max = max(relaxations),
                  step=diff(sort(unique(relaxations)))[1]),
      
      checkboxInput("drawEqualityLine","Draw equality line?",
                    value=setLastVal(input$drawEqualityLine,TRUE))
    )
    
    out
  })
  
  # Set the best relaxation and half period setting on the compare tab
  observe({
    if(is.null(input$setBestModel) || input$setBestModel <=0) return()
    isolate({
      modelSummary <- modelSummary()
      
      modelSubset1 <- modelSummary[modelSummary$method == input$modelCompare1,]
      if(input$yModelsCompare %in% c("Average log loss","Average bet return")){
        bestId <- which.min(modelSubset1[[input$yModelsCompare]])
      } else{
        bestId <- which.max(modelSubset1[[input$yModelsCompare]])
      }
      
      updateSelectInput(session,"halfPeriodSelectionComp1",
                        selected=modelSubset1[bestId,"halfPeriod"])
      updateSelectInput(session,"relaxationSelectionComp1",
                        selected=modelSubset1[bestId,"relaxation"])
      
      modelSubset2 <- modelSummary[modelSummary$method == input$modelCompare2,]
      if(input$yModelsCompare %in% c("Average log loss","Average bet return")){
        bestId <- which.min(modelSubset2[[input$yModelsCompare]])
      } else{
        bestId <- which.max(modelSubset2[[input$yModelsCompare]])
      }
      
      updateSelectInput(session,"halfPeriodSelectionComp2",
                        selected=modelSubset2[bestId,"halfPeriod"])
      updateSelectInput(session,"relaxationSelectionComp2",
                        selected=modelSubset2[bestId,"relaxation"])
    })
  })
  
  # Calculate if both model selections are equal
  modelSelectionEqual <- reactive({
    if(is.null(input$modelCompare1)) return(FALSE)
    
    out <- input$modelCompare1 == input$modelCompare2 &&
      input$halfPeriodSelectionComp1 == input$halfPeriodSelectionComp2 &&
      input$relaxationSelectionComp1 == input$relaxationSelectionComp2
    
    out
  })
  
  # Show the histogram bin slider if both model selections are equal
  output$comparePlotTitle <- renderUI({
    if(!modelSelectionEqual()){
      h4(textOutput("modelBookieCorr"),align="center")
    } else{
      fluidRow(
        column(6,sliderInput("histBins","Number of bins",min=2,max=100,
                             value=setLastVal(input$histBins,30)),
               offset=3)
      )
    }
  })
  
  # Data for model compare plot to average bookies
  compareModelsData <- reactive({
    if(is.null(rawData()) ||
       is.null(input$halfPeriodSelectionComp1) ||
       is.null(input$halfPeriodSelectionComp2) ||
       is.null(input$relaxationSelectionComp1) ||
       is.null(input$relaxationSelectionComp2)) return()
    rawData <- rawData()
    
    # Combine the target model column
    targetCol1 <- paste(
      unlist(strsplit(names(which(modelNames==input$modelCompare1)[1]),"_"))[1],
      input$halfPeriodSelectionComp1[1],
      input$relaxationSelectionComp1[1],sep="_")
    targetCol2 <- paste(
      unlist(strsplit(names(which(modelNames==input$modelCompare2)[1]),"_"))[1],
      input$halfPeriodSelectionComp2[1],
      input$relaxationSelectionComp2[1],sep="_")
    
    matchString <- paste0(rawData$Date, " ",
                          rawData$home, " ",
                          rawData$hgoal, " - ",
                          rawData$vgoal, " ",
                          rawData$visitor, " (",
                          rownames(rawData),")"
    )
    
    # Calculate the average bookie outcome
    out <- data.frame(modelOutcome1 = rawData[[targetCol1]],
                      modelOutcome2 = rawData[[targetCol2]],
                      outcome = rawData[["result"]],
                      matchString = matchString,
                      ID = rownames(rawData))
    
    out
  })
  
  output$modelBookieCorr <- renderText({
    if(is.null(compareModelsData())) return()
    compareModelsData <- compareModelsData()
    
    correlation <- cor(compareModelsData[,1],compareModelsData[,2])
    
    out <- paste("Correlation between the models:", round(correlation,3))
    
    out
  })
  
  
  # Model compare plot to average bookies
  output$modelsCompareComp <- renderPlotly({
    if(is.null(compareModelsData())) return()
    
    compareModelsData <- compareModelsData()
    compareModelsData$Outcome <-
      factor(compareModelsData$outcome,levels=c("H","D","A"),
             labels=c("Home win","Draw","Away win"))
    drawEqualityLine <- input$drawEqualityLine
    
    # Draw a histogram of the predicted probabilities if both models are equal
    if(modelSelectionEqual()){
      nbBins <- ifelse(is.null(input$histBins),30,input$histBins)
      gg <- ggplot(compareModelsData, aes(modelOutcome1)) +
        geom_histogram(aes(y = ..density..), color="black", fill="grey",
                       bins=nbBins) +
        geom_density(color="blue")
    } else{
      
      gg <- ggplot(compareModelsData, aes(modelOutcome1,modelOutcome2)) +
        geom_point(aes(text=matchString,colour=Outcome))
      
      # Draw equality line
      if(drawEqualityLine){
        lineRange <- range(compareModelsData[,1:2])
        lineDf <- data.frame(modelOutcome1=lineRange,
                             modelOutcome2=lineRange)
        gg <- gg + 
          geom_path(data=lineDf,colour="black",size=1)
      }
    }
    
    out <- ggplotly(gg)
    # out <- layout(out, hovermode = 'closest')
    
    out
    
  })
  
  output$debugText <- renderText({ 
    out <- nrow(summary)
    
    out
  })
})