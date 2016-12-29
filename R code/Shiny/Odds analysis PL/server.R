# Load the merged data
load(mergedFile)
# merged <- merged[merged$Season>2012,]
rownames(merged) <- paste0("ID:",1:nrow(merged))
merged$maxOdds <- apply(merged[,31:57],1,max,na.rm=TRUE)

# Extract the model names, half periods and relaxation parameters
modelColumns <- grep("prob.*_.*_[^a-zA-Z]*$",names(merged))
modelNames <- sapply(names(merged)[modelColumns],function(x)
  unlist(strsplit(x,'_'))[1])
modelNames <- ifelse(modelNames=="prob","BT",
                     ifelse(modelNames=="probHA","EBT",
                            ifelse(modelNames=="probP","Poisson",
                                   ifelse(modelNames=="probGD","Combined BTP",
                                          ifelse(modelNames=="probDA","Extended Poisson","Elo")
                                   )
                            )
                     )
)
halfPeriods <- sapply(names(merged)[modelColumns],function(x)
  as.numeric(unlist(strsplit(x,'_'))[2]))
relaxations <- sapply(names(merged)[modelColumns],function(x)
  as.numeric(unlist(strsplit(x,'_'))[3]))

# Function to return the previous value unless if its zero in which case a 
# default value is returned
setLastVal <- function(value,default){
  if(is.null(value)){
    return(default)
  }
  return(value)
}

shinyServer(function(input, output, session) {
  # Generate the game selection input slider widgets
  output$gameSelectSliders <- renderUI({
    maxLeft <- max(c(merged$homeLeft,merged$awayLeft))+1
    list(
      sliderInput("seasonSelection","Season selection",
                  value=range(merged$Season),sep="",
                  min = min(merged$Season),max = max(merged$Season), step=1),
      sliderInput("excludeLast","Exclude last matches",value=c(2,maxLeft),min=0,
                  max = maxLeft),
      sliderInput("excludeHighOdds","Exclude matches with odds exceeding",
                  value=10,min=4,max = floor(max(merged$maxOdds)) + 1)
    )
  })
  
  # Subset the merged data on the season and minimum number matches left
  # selection
  rawData <- reactive({
    if(is.null(input$seasonSelection) || is.null(input$excludeLast) ||
       is.null(input$excludeHighOdds)) return()
    out <- merged
    out <- out[out$Season>=input$seasonSelection[1] &
                 out$Season<=input$seasonSelection[2],]
    out <- out[out$homeLeft>input$excludeLast[1] &
                 out$awayLeft>input$excludeLast[1] &
                 out$homeLeft<input$excludeLast[2] &
                 out$awayLeft<input$excludeLast[2],]
    out <- out[out$maxOdds<input$excludeHighOdds,]
    out
  })
  
  # Calculate the average best model blend of the selected models
  bestModelBlendTarget <- reactive({
    if(is.null(modelSummarySubset) || is.null(input$yModels)) return(NA)
    target <- input$yModels
    rawData <- rawData()
    modelSummarySubset <- modelSummarySubset()
    methods <- sort(unique(modelSummarySubset$Method))
    nbMethods <- length(methods)
    
    if(nbMethods>0){
      meanPred <- rep(0,nrow(rawData))
      for(i in 1:nbMethods){
        method <- methods[i]
        methodSubset <- modelSummarySubset[modelSummarySubset$Method==method,]
        dataId <- which.min(methodSubset[[target]])

        targetCol <- paste(
          unlist(strsplit(names(which(modelNames==method)[1]),"_"))[1],
          methodSubset$`Half period`[dataId],
          methodSubset$Relaxation[dataId],sep="_")
        meanPred <- meanPred + rawData[[targetCol]]
      }
      out <- meanPred/nbMethods
    } else{
      out <- NA
    }
    
    out
  })
  
  # Generate a data table of the raw data
  output$rawDatTable <- renderDataTable({
    if(is.null(rawData())) return()
    out <- rawData()
    
    # Discard prediction columns (too many)
    out <- out[,!grepl("prob",names(out))]# & !grepl("outcome",names(out))]
    out
  }, options = list(pageLength = 5,
                    lengthMenu = list(c(5, 10, 20, -1),
                                      c('5', '10', '20', 'All')))
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
  
  # Calculate the model summary parameters for rawData, a row subset of merged
  modelSummary <- reactive({
    if(is.null(rawData())) return()
    out <- NULL
    rawData <- rawData()
    # rawData <- as.matrix(rawData)
    nbModels <- length(modelColumns)
    for(i in 1:nbModels){
      modelColumn <- modelColumns[i]
      pH <- rawData[,modelColumn+1]
      pD <- rawData[,modelColumn+2]
      pA <- abs(1-rawData[,modelColumn+1]-rawData[,modelColumn+2])
      
      avReturn <- mean(1/rawData[,modelColumn])
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
    
    out$ID <- 1:nrow(out)
    
    names(out)[4:7] <- c("Average return","Average accuracy", "Majority correct",
                         "Average log loss")
    
    out
  })
  
  # Calculate the bookie summary
  bookieSummary <- reactive({
    if(is.null(rawData())) return()
    
    rawData <- rawData()
    rawData <- rawData[,!grepl("prob",names(rawData))]
    rawData <- rawData[,-c(1:30,68:70)]
    modelColumns <- 3*((1:9)-1)
    outcomeColumns <- 28:36
    sH <- rowMeans(rawData[,modelColumns+1],na.rm = TRUE)
    sD <- rowMeans(rawData[,modelColumns+2],na.rm = TRUE)
    sA <- rowMeans(rawData[,modelColumns+3],na.rm = TRUE)
    safety <- 1/sH+1/sD+1/sA
    pH <- 1/sH/safety
    pD <- 1/sD/safety
    pA <- 1/sA/safety
    meanOutcome <- rowMeans(rawData[,outcomeColumns],na.rm = TRUE)
    
    avReturn <- mean(1/meanOutcome)
    avAcc <- mean(meanOutcome)
    logLoss <- -mean(log(meanOutcome))
    prediction <- rep("H",nrow(rawData))
    prediction[pD>pH] <- "D"
    prediction[pA>pH & pA>pD] <- "A"
    majCorrect <- mean(prediction==rawData$outcome)
    
    out <- data.frame(method="Bookie",
                      avReturn=avReturn,
                      avAcc=avAcc,
                      majCorrect=majCorrect,
                      logLoss=logLoss,
                      stringsAsFactors=FALSE)
    names(out) <- c("Method", "Averagebetreturn", 
                    "Averageaccuracy", "Majoritycorrect", "Averagelogloss")
    out
  })
  
  polrMatchFit <- reactive({
    if(is.null(rawData())) return()
    
    merged <- rawData()
    
    predictorIds <- 13:30
    predictors <- merged[,predictorIds]
    
    # Drop predictors that have missing values
    missingCounts <- sapply(predictors,function(x) sum(is.na(x)))
    predictors <- predictors[,missingCounts==0]
    
    # Pair the predictors
    for(i in 1:(ncol(predictors)/2)){
      predictors[,1+(i-1)*2] <- predictors[,1+(i-1)*2]-predictors[,i*2]
      names(predictors)[1+(i-1)*2] <- paste0(names(predictors)[1+(i-1)*2],
                                             " - ",
                                             names(predictors)[i*2])
    }
    predictors <- predictors[,2*(1:(ncol(predictors)/2))-1]
    
    # Combine the predictors with the outcome
    data <- cbind(merged$outcome,predictors)
    names(data)[1] <- "Result"
    
    # Randomly shuffle the data
    randomShuffleIds <- sample(nrow(data))
    data <- data[randomShuffleIds,]
    
    # Calculate the cross validation indices using K folds
    folds <- cut(seq(1,nrow(data)),breaks=K,labels=FALSE)
    
    polrPredictProb <- rep(NA,nrow(data))
    prediction <- rep(NA,nrow(data))
    for(i in 1:K){
      # Split up training and test data
      testIndexes <- which(folds==i,arr.ind=TRUE)
      trainData <- data[-testIndexes,]
      testData <- data[testIndexes,]
      
      # Fit POLR model
      fitPolr <- polr(Result ~ ., data=trainData)
      polrPredict <- predict(fitPolr,type="probs",newdata = testData)
      polrPredictProb[randomShuffleIds[testIndexes]] <-
        ifelse(testData$Result=="A",polrPredict[,1],
               ifelse(testData$Result=="D",polrPredict[,2],
                      polrPredict[,3]))
      prediction[testIndexes] <-
        c("A","D","H")[apply(polrPredict,1,function(x) which.max(x))]
    }
    
    avReturn <- mean(1/polrPredictProb)
    avAcc <- mean(polrPredictProb)
    logLoss <- -mean(log(polrPredictProb))
    majCorrect <- mean(prediction==data$Result)
    
    out <- data.frame(method="POLR",
                      avReturn=avReturn,
                      avAcc=avAcc,
                      majCorrect=majCorrect,
                      logLoss=logLoss,
                      stringsAsFactors=FALSE)
    
    names(out) <- c("Method", "Averagebetreturn", 
                    "Averageaccuracy", "Majoritycorrect", "Averagelogloss")
    
    
    combined <- list(out=out,polrPredictProb=polrPredictProb)
    
    combined
    
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
    out <- list(
      selectInput("xModels","X axis",choices=xModels,selected=setLastVal(input$xModels,xModels[2])),
      selectInput("yModels","Y axis",choices=yModels,selected=setLastVal(input$yModels,yModels[4])),
      br(),
      checkboxInput("showBookieComparison","Compare to average of bookies?",value=setLastVal(input$showBookieComparison,TRUE)),
      checkboxInput("showMatchModelFit","Compare to match model fit?",value=setLastVal(input$showMatchModelFit,TRUE)),
      br(),
      checkboxInput("textLabelModelsGraph","Show other X label on hover?",value=setLastVal(input$textLabelModelsGraph,TRUE)),
      checkboxInput("subsetModel","Subset on other X?",value=setLastVal(input$subsetModel,FALSE)),
      conditionalPanel(condition="input.subsetModel",
                       sliderInput("sliderotherX",label="",min=0,max=1,value=0))
    )
    
    out
  })
  
  # Set the range of the other slider X
  observe({
    if(is.null(input$subsetModel)||input$subsetModel==FALSE) return()
    optionsX <- xModels
    selectedX <- input$xModels
    
    sliderX <- optionsX[optionsX!=selectedX][1]
    valuesX <- modelSummarySubset()[[sliderX]]
    xVals <- sort(unique(valuesX))
    
    updateSliderInput(session,"sliderotherX",label=sliderX,min=min(valuesX),
                      max=max(valuesX),value=min(valuesX),
                      step=xVals[2]-xVals[1])
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
  
  output$modelsGraphP <- renderPlotly({
    if(is.null(modelSummarySubsetPlot()) || is.null(input$xModels) ||
       is.null(input$yModels) || is.null(bookieSummary()) ||
       is.null(polrMatchFit()) || nrow(modelSummarySubsetPlot())<=0) return()
    
    # Extract input data
    modelSummarySubset <- modelSummarySubsetPlot()
    showBookieComparison <- input$showBookieComparison
    showMatchModelFit <- input$showMatchModelFit
    bestModelBlendTarget <- bestModelBlendTarget()
    
    names(modelSummarySubset) <- gsub(" ","",names(modelSummarySubset))
    xName <- gsub(" ","",input[["xModels"]])
    otherId <- ifelse(xName==names(modelSummarySubset)[2],3,2)
    otherXName <- names(modelSummarySubset)[otherId]
    modelSummarySubset[[otherXName]] <- paste0(otherXName,": ",
                                               modelSummarySubset[[otherXName]])
    yName <- gsub(" ","",input[["yModels"]])
    yNameSpaced <- input[["yModels"]]
    
    modelSummarySubset$xPlot <- modelSummarySubset[[xName]]
    modelSummarySubset$yPlot <- modelSummarySubset[[yName]]
    
    # # Drop remaining columns
    # modelSummarySubset <- modelSummarySubset[,names(modelSummarySubset) %in%
    #                                            c(otherXName,"xPlot","yPlot",
    #                                              "Method")]
    
    #     # Convert strings to factors
    #     stringCols <- sapply(modelSummarySubset,is.character)
    #     modelSummarySubset[,stringCols] <-
    #       lapply(modelSummarySubset[,stringCols],as.factor)
    
    if(ggplot2plotly){
      gg <- ggplot(modelSummarySubset, aes_string(xName,yName,colour="Method"))
      if(input$textLabelModelsGraph){
        gg <- gg + geom_point(aes_string(text=otherXName))
      } else{
        gg <- gg + geom_point()
      }
      
      # Optionally add the bookie comparison line
      if(showBookieComparison){
        bookieSummary <- bookieSummary()
        gg <- gg +
          geom_hline(data=bookieSummary,aes_string(yintercept=yName),size=1)
      }
      
      if(showMatchModelFit){
        polrMatchFit <- polrMatchFit()[[1]]
        # polrMatchFit[[5]] <- 0.9423123
        gg <- gg + 
          geom_hline(data=polrMatchFit,aes_string(yintercept=yName),size=1,
                     color="green4")
      }
      
      # # Thesis graphs
      # # gg + ylim(c(1,1.15)) + xlab("Half period") + ylab("Average log loss") +
      # #   annotate("text", 200, 1.0845, label = "Average bookmaker model",
      # #            color="black") +
      # #   annotate("text", 200, 1.0199, label = "Average match data model",
      # #            color="green4") +
      # gg + ylim(c(0.94,1.05)) + xlab("Half period") + ylab("Average log loss") +
      # annotate("text", 220, 0.963, label = "Average bookmaker model",
      #          color="black") +
      #   annotate("text", 220, 0.9488, label = "Average match data model",
      #            color="green4") +
      #   # xlab("Elo update constant") + 
      #   theme(axis.text=element_text(size=12),
      #         legend.text=element_text(size=11),
      #         axis.title=element_text(size=14),
      #         title=element_text(size=14))
      # browser()
      
      
      out <- ggplotly(gg)
      
    } else{
      
      colorSet <- "Set1"
      if(input$textLabelModelsGraph){
        modelSummarySubset$text <- modelSummarySubset[[otherXName]]
        out <- plot_ly(modelSummarySubset,x=xPlot,y=yPlot,color=Method,
                       text = text, colors = colorSet, mode="markers")
      } else{
        out <- plot_ly(modelSummarySubset,x=xPlot,y=yPlot,color=Method,
                       colors = colorSet, mode="markers")
      }
      out <- out %>%
        layout(xaxis = list(title = xName),
               yaxis = list(title = yNameSpaced))
      
      # Optionally add the bookie comparison line
      if(showBookieComparison){
        bookieSummary <- bookieSummary()
        bookieSummary <- rbind(bookieSummary,bookieSummary)
        bookieSummary$xPlot <- c(min(modelSummarySubset$xPlot),
                                 max(modelSummarySubset$xPlot))
        bookieSummary$yPlot <- bookieSummary[[yName]]
        out <- add_trace(out,x=xPlot,y=yPlot,mode="lines",data = bookieSummary,
                         name="Bookie summary",
                         line=list(color="blue"))
      }
      
      if(showMatchModelFit){
        polrMatchFit <- polrMatchFit()[[1]]
        polrMatchFit <- rbind(polrMatchFit,polrMatchFit)
        polrMatchFit$xPlot <- c(min(modelSummarySubset$xPlot),
                                max(modelSummarySubset$xPlot))
        polrMatchFit$yPlot <- polrMatchFit[[yName]]
        out <- add_trace(out,x=xPlot,y=yPlot,mode="lines",data = polrMatchFit,
                         name="Match model summary",
                         line=list(color="green"))
      }
    }
    
    out
  })
  
  # Generate the model compare graph input widgets
  output$modelCompareInputs <- renderUI({
    if(is.null(modelSummary())) return()
    modelNames <- sort(unique(modelNames))
    out <- list(
      selectInput("modelCompare","Model",choices=modelNames,
                  selected=setLastVal(input$modelCompare,modelNames[1])),
      br(),
      selectInput("yModelsCompare","Best target",choices=yModels,
                  selected=setLastVal(input$yModelsCompare,yModels[4])),
      actionButton("setBestModel","Set best model settings wrt best target"),
      h4(" "),
      h4(" "),
      sliderInput("halfPeriodSelectionComp","Half period selection",
                  value=setLastVal(input$halfPeriodSelectionComp,
                                   min(halfPeriods)),sep="",
                  min = min(halfPeriods),max = max(halfPeriods),
                  step=diff(sort(unique(halfPeriods)))[1]),
      sliderInput("relaxationSelectionComp","Relaxation selection",
                  value=setLastVal(input$relaxationSelectionComp,
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
      
      modelSubset <- modelSummary[modelSummary$method == input$modelCompare,]
      if(input$yModelsCompare %in% c("Average log loss","Average bet return")){
        bestId <- which.min(modelSubset[[input$yModelsCompare]])
      } else{
        bestId <- which.max(modelSubset[[input$yModelsCompare]])
      }
      
      updateSelectInput(session,"halfPeriodSelectionComp",
                        selected=modelSubset[bestId,"halfPeriod"])
      updateSelectInput(session,"relaxationSelectionComp",
                        selected=modelSubset[bestId,"relaxation"])
    })
  })
  
  # Data for model compare plot to average bookies
  compareModelBookieData <- reactive({
    if(is.null(rawData()) ||
       # is.null(input$yModelsCompare) ||
       is.null(input$halfPeriodSelectionComp) ||
       is.null(input$relaxationSelectionComp)) return()
    rawData <- rawData()
    
    # Combine the target model column
    targetCol <- paste(
      unlist(strsplit(names(which(modelNames==input$modelCompare)[1]),"_"))[1],
      input$halfPeriodSelectionComp[1],
      input$relaxationSelectionComp[1],sep="_")
    
    # Extract the match outcome values
    matchStatsOurcome <- polrMatchFit()[[2]]
    
    # Calculate the average bookie outcome
    bookieCols <- 58:66
    bookieOutcome <- rowMeans(rawData[,bookieCols],na.rm=TRUE)
    out <- data.frame(bookieOutcome = bookieOutcome,
                      modelOutcome = rawData[[targetCol]],
                      outcome = rawData[["outcome"]],
                      ID = rownames(rawData),
                      matchStatsOurcome=matchStatsOurcome)
    
    out
  })
  
  output$modelBookieCorr <- renderText({
    if(is.null(compareModelBookieData())) return()
    compareModelBookieData <- compareModelBookieData()
    
    # browser()
    correlation <- cor(compareModelBookieData[,1],compareModelBookieData[,2])
    correlationMS <- cor(compareModelBookieData$modelOutcome,
                         compareModelBookieData$matchStatsOurcome)
    
    # browser()
    
    out <- paste0("Correlation between the model and average bookie prediction (match stats model): ",
                  round(correlation,3)," (",round(correlationMS,3),")")
    
    out
  })
  
  
  # Model compare plot to average bookies
  output$modelsCompareComp <- renderPlotly({
    if(is.null(compareModelBookieData())) return()
    
    compareModelBookieData <- compareModelBookieData()
    compareModelBookieData$Outcome <-
      factor(compareModelBookieData$outcome,levels=c("H","D","A"),
             labels=c("Home win","Draw","Away win"))
    drawEqualityLine <- input$drawEqualityLine
    
    gg <- ggplot(compareModelBookieData, aes(modelOutcome,bookieOutcome)) +
      geom_point(aes(text=ID,colour=Outcome))
    
    # Draw equality line
    if(drawEqualityLine){
      lineRange <- range(compareModelBookieData[,1:2])
      lineDf <- data.frame(modelOutcome=lineRange,
                           bookieOutcome=lineRange)
      gg <- gg + 
        geom_path(data=lineDf,colour="black",size=0.1)
    }
    
    # gg + ylim(c(0,1)) + xlab("Model outcome probability") +
    #   ylab("Bookmaker outcome probability") +
    # geom_path(data=lineDf,colour="black",size=1.5) +
    #   theme(axis.text=element_text(size=12),
    #         legend.text=element_text(size=11),
    #         axis.title=element_text(size=14),
    #         title=element_text(size=14))
    # browser()
    
    out <- ggplotly(gg)
    # out <- layout(out, hovermode = 'closest')
    
    out
    
  })
  
  output$debugText <- renderText({ 
    out <- nrow(merged)
    
    out
  })
})