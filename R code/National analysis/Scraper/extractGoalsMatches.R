# Script to scrape the eu-football.info page

# Clear workspace and setwd
rm(list=ls())
setwd("C:/Users/Tom/Documents/Sta dat Tom/Thesis/R/Scraper")

# Load required libraries
library(rvest)
library(magrittr)
library(beepr)

# Name of the results file
resultsFile <- "internationalResultsEuropePG.RData"

# Name of the processed results file
processedResultsFile <- "internationalResultsEuropePG.RData"

# Base url
baseUrl <- "http://eu-football.info/"

# Load the processed match data frame 
load(resultsFile)

# Wrapper for the read_html that adds a fixed delay after reading the page in
# order to avoid overloading the server (and avoid being banned, again :))
readHtml <- function(url,delay=1){
  out <- read_html(url)
  Sys.sleep(delay)
  out
}

# Function to extract valid links from an xml nodeset
getLinks <- function(nodeset){
  nodeset <- nodeset %>% html_nodes("a") %>% as.character()
  nodeset <- nodeset[!grepl("page=",nodeset)]
  nodeset <- nodeset[grepl("match.php",nodeset)]
  links <- gsub('.*a href=\\"',"",gsub('\\">.*$',"",nodeset))
  
  return(paste0(baseUrl,links))
}

# Final extraction
consideredNationalIds <- sort(unique(allMatches$TeamID[is.na(allMatches$link)]))
nbNationalIds <- length(consideredNationalIds)

# hids <- consideredNationalIds[match(allMatches$Home,consideredTeams)]
# vids <- consideredNationalIds[match(allMatches$Away,consideredTeams)]
# ids <- pmin(hids,vids,na.rm=TRUE)

# Loop over all national ids
i <- 1
while(i<=nbNationalIds){
  teamId <- consideredNationalIds[i]
  cat("\nProcessing team ",i," of ",nbNationalIds,". ",sep="")
  
  url <- paste0("http://eu-football.info/_matches.php?id=",teamId)
  
  # Read page
  validUrl = FALSE
  try({nationalPage <- readHtml(url); validUrl<- TRUE}, silent=TRUE)
  
  if(validUrl){
    loopCompleted <- FALSE
    # Try the following code until it succeeds (might fail due to interrupted
    # network connection)
    while(!loopCompleted){
      try({
        # Calculate the number of pages for the considered national team
        tdNodes <- nationalPage %>% html_nodes("table") %>% extract2(5) %>%
          html_nodes("td")
        lastNode <- tdNodes[length(tdNodes)]
        nextLinks <- lastNode %>% html_nodes("a")
        nbNextLinks <- length(nextLinks)
        if(nbNextLinks>0){
          lastLink <- as.character(nextLinks[nbNextLinks])
          nbPages <- as.numeric(gsub('\".*',"",gsub("^.*page=","",lastLink)))
        } else{
          nbPages <- 1
        }
        
        teamMatches <- NULL
        # Loop over all pages and store the results of all matches
        for(j in 1:nbPages){
          cat("j:",j," ",sep="")
          if(j>1){
            url <- paste0("http://eu-football.info/_matches.php?id=",teamId,
                          "&page=",j)
            validUrl = FALSE
            try({nationalPage <- readHtml(url); validUrl<- TRUE}, silent=TRUE)
            if(!validUrl){
              beep()
              stop("Loading error tab page")
            }
          }
          matches <- nationalPage %>% html_nodes("table") %>% 
            extract2(5) %>% html_table(header = TRUE)
          links <- getLinks(
            nationalPage %>% html_nodes("table") %>% extract2(5))
          
          # Drop first column and last row if there is more than one page
          matches <- matches[,-1,drop=FALSE]
          if(nbPages>1){
            matches <- matches[-nrow(matches),,drop=FALSE]
          }
          
          # Check correctness of links to individual matches
          if(length(links)!=nrow(matches)){
            beep(sound=2)
            browser()
          }
          
          # Rename columns
          names(matches) <- c("Date","Venue","Comp","Home","Result","Away")
          
          # Check which matches need individual processing
          matches$Date <- as.Date(matches$Date,format="%d.%m.%Y")
          matches$key <- paste0(matches$Home,matches$Away,matches$Date)
          allMatchesIds <- match(matches$key,allMatches$key)
          matchesToLoad <- which(!is.na(allMatchesIds))
          targetRowsAllMatches <- allMatchesIds[matchesToLoad]
          
          # Drop matches that were loaded before
          matchesToLoad <-
            matchesToLoad[is.na(allMatches[targetRowsAllMatches,"link"])]
          targetRowsAllMatches <- allMatchesIds[matchesToLoad]
          
          # Load additional match data of individual matches that were not yet
          # processed
          nbIndividualMatches <- length(targetRowsAllMatches)
          if(nbIndividualMatches>0){
            k <- 1
            allMatches$link[targetRowsAllMatches] <- links[matchesToLoad]
            while(k<=nbIndividualMatches){
              cat("k:",k," ",sep="")
              link <- links[matchesToLoad[k]]
              targetRow <- targetRowsAllMatches[k]
              
              # Read page
              validUrlPage = FALSE
              try({matchPage <- readHtml(link); validUrlPage<- TRUE},
                  silent=TRUE)
              
              if(validUrlPage){
                # Extract competition and goals information
                matchInfo <- matchPage %>% html_nodes("table") %>%
                  extract2(4) %>% html_table()
                allMatches[targetRow,"Competition"] <- matchInfo[3,2]
                allMatches[targetRow,"Goals"] <- matchInfo[6,2]
                
                # Increment match counter
                k <- k+1
              } else{
                beep()
                warning("Loading error team page")
              }
            }
            save(allMatches, file = processedResultsFile)
          }
        }
        loopCompleted <- TRUE
      }, silent = TRUE)
      if(!loopCompleted){
        beep()
        Sys.sleep(30)
        url <- paste0("http://eu-football.info/_matches.php?id=",teamId)
        try(nationalPage <- readHtml(url), silent=TRUE)
      }
    }
    # Save the allMatches file
    save(allMatches, file = processedResultsFile)
    
    i <- i+1
  } else{
    beep()
    warning("Loading error team page")
  }
}

# Save processed european matches
save(allMatches, file = resultsFile)
beep(sound="fanfare")