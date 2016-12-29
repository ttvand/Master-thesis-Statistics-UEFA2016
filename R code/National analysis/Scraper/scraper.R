# Script to scrape the eu-football.info page

# Clear workspace and setwd
rm(list=ls())
setwd("C:/Users/Tom/Documents/Sta dat Tom/Thesis/R/Scraper")

# Load required libraries
library(rvest)
library(magrittr)
library(beepr)

# Name of the results file
resultsFile <- "internationalResultsEurope.RData"

# Data frame to append all matches to
allMatches <- NULL

# Wrapper for the read_html that adds a fixed delay after reading the page in
# order to avoid overloading the server (and avoid being banned, again :))
readHtml <- function(url,delay=1){
  out <- read_html(url)
  Sys.sleep(delay)
  out
}

# Extract valid nation ids
mainUrl <- "http://eu-football.info/"
mainPage <- readHtml(mainUrl)
options <- mainPage %>% html_nodes("table") %>% 
  extract2(6) %>% html_nodes("option") %>% as.character()
if(length(options)==0){
  options <- mainPage %>% html_nodes("table") %>% 
    extract2(9) %>% html_nodes("option") %>% as.character()
}
yearOption <- grep("select a year",options)
options <- options[2:(yearOption-1)]

# Final extraction
consideredNationalIds <-
  as.numeric(gsub('\"',"",gsub('<option value=\"',"",gsub('>.*$',"",options))))
consideredTeams <- gsub("&#13","",gsub("<.*","",gsub('.*\">\\s',"",
                                                     gsub(";.*","",options))))
consideredTeams <- gsub("&amp","& Herzegovina",consideredTeams)
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
          
          # Drop first column and last row if there is more than one page
          matches <- matches[,-1,drop=FALSE]
          if(nbPages>1){
            matches <- matches[-nrow(matches),,drop=FALSE]
          }
          
          # Store team id
          matches$teamId <- teamId
          
          # Rename columns
          names(matches) <- c("Date","Venue","Comp","Home","Result","Away",
                              "TeamID")
          
          teamMatches <- rbind(teamMatches,matches)
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
    # Append the matches to the combined file
    allMatches <- rbind(allMatches,teamMatches)
    
    i <- i+1
  } else{
    beep()
    warning("Loading error team page")
  }
}

# Save processed european matches
save(allMatches, file = resultsFile)
beep(sound="fanfare")
